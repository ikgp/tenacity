/**********************************************************************

  Tenacity

  AudioIoCallback.cpp

  Dominic Mazzoni

  Avery King split from AudioIO.cpp

**********************************************************************/

#include "AudioIoCallback.h"
#include "AudioIOListener.h"

#include <mutex>
#include <thread>

#include <wx/string.h>
#include <wx/time.h>
#include <wx/wxcrtvararg.h>

// Tenacity libraries
#include <lib-audio-devices/Meter.h>
#include <lib-math/Resample.h>
#include <lib-utility/MessageBuffer.h>

#include "effects/RealtimeEffectManager.h"

// Static members
int AudioIoCallback::mNextStreamToken = 0;
double AudioIoCallback::mCachedBestRateOut;
bool AudioIoCallback::mCachedBestRatePlaying;
bool AudioIoCallback::mCachedBestRateCapturing;

int AudioIoCallback::AudioCallback(const void *inputBuffer, void *outputBuffer,
                                   unsigned long framesPerBuffer,
                                   const PaStreamCallbackTimeInfo *timeInfo,
                                   const PaStreamCallbackFlags statusFlags, void *WXUNUSED(userData))
{
    mbHasSoloTracks = CountSoloingTracks() > 0;
    mCallbackReturn = paContinue;

#ifdef EXPERIMENTAL_MIDI_OUT
    // MIDI
    // ComputeMidiTimings may modify mFramesPerBuffer and mNumFrames,
    // but it does nothing unless we have EXPERIMENTAL_MIDI_OUT
    // TODO: Possibly rename variables to make it clearer which ones are MIDI specific
    // and which ones affect all audio.
    ComputeMidiTimings(
        timeInfo,
        framesPerBuffer);
#ifndef USE_MIDI_THREAD
    if (mMidiStream)
        FillMidiBuffers();
#endif
#endif

    // ------ MEMORY ALLOCATIONS -----------------------------------------------
    // tempFloats will be a reusable scratch pad for (possibly format converted)
    // audio data.  One temporary use is for the InputMeter data.
    const auto numPlaybackChannels = mNumPlaybackChannels;
    const auto numCaptureChannels = mNumCaptureChannels;
    std::unique_ptr<float> tempFloats(
        new float[framesPerBuffer * std::max(numCaptureChannels, numPlaybackChannels)]);

    float *outputMeterFloats = static_cast<float *>(outputBuffer);
    // ----- END of MEMORY ALLOCATIONS ------------------------------------------

    if (inputBuffer && numCaptureChannels)
    {
        float *inputSamples;

        if (mCaptureFormat == floatSample)
        {
            inputSamples = (float *)inputBuffer;
        }
        else
        {
            SamplesToFloats(reinterpret_cast<constSamplePtr>(inputBuffer),
                            mCaptureFormat, tempFloats.get(), framesPerBuffer * numCaptureChannels);
            inputSamples = tempFloats.get();
        }

        SendVuInputMeterData(
            inputSamples,
            framesPerBuffer);

        // This function may queue up a pause or resume.
        // TODO this is a bit dodgy as it toggles the Pause, and
        // relies on an idle event to have handled that, so could
        // queue up multiple toggle requests and so do nothing.
        // Eventually it will sort itself out by random luck, but
        // the net effect is a delay in starting/stopping sound activated
        // recording.
        CheckSoundActivatedRecordingLevel(
            inputSamples,
            framesPerBuffer);
    }

    // Even when paused, we do playthrough.
    // Initialise output buffer to zero or to playthrough data.
    // Initialise output meter values.
    DoPlaythrough(
        inputBuffer,
        outputBuffer,
        framesPerBuffer,
        outputMeterFloats);

    // Test for no track audio to play (because we are paused and have faded out)
    if (mPaused && ((!mbMicroFades) || AllTracksAlreadySilent()))
        return mCallbackReturn;

    // To add track output to output (to play sound on speaker)
    // possible exit, if we were seeking.
    if (FillOutputBuffers(
            outputBuffer,
            framesPerBuffer,
            outputMeterFloats))
        return mCallbackReturn;

    // To move the cursor onwards.  (uses mMaxFramesOutput)
    UpdateTimePosition(framesPerBuffer);

    // To capture input into track (sound from microphone)
    FillInputBuffers(
        inputBuffer,
        framesPerBuffer,
        statusFlags,
        tempFloats.get());

    SendVuOutputMeterData(outputMeterFloats, framesPerBuffer);

    return mCallbackReturn;
}

int AudioIoCallback::CallbackDoSeek()
{
    const int token = mStreamToken;
    std::lock_guard<std::mutex> locker(mSuspendAudioThread);
    if (token != mStreamToken)
        // This stream got destroyed while we waited for it
        return paAbort;

    const auto numPlaybackTracks = mPlaybackTracks.size();

    // Pause audio thread and wait for it to finish
    mAudioThreadFillBuffersLoopRunning = false;
    while (mAudioThreadFillBuffersLoopActive)
    {
        std::this_thread::sleep_for(std::chrono::milliseconds(50));
    }

    // Calculate the NEW time position, in the PortAudio callback
    const auto time = mPlaybackSchedule.ClampTrackTime(
        mPlaybackSchedule.GetTrackTime() + mSeek);
    mPlaybackSchedule.SetTrackTime(time);
    mSeek = 0.0;

    mPlaybackSchedule.RealTimeInit(time);

    // Reset mixer positions and flush buffers for all tracks
    for (size_t i = 0; i < numPlaybackTracks; i++)
    {
        const bool skipping = true;
        mPlaybackMixers[i]->Reposition(time, skipping);
        const auto toDiscard =
            mPlaybackBuffers[i]->AvailForGet();
        const auto discarded =
            mPlaybackBuffers[i]->Discard(toDiscard);
        // wxASSERT( discarded == toDiscard );
        // but we can't assert in this thread
        wxUnusedVar(discarded);
    }

    // Reload the ring buffers
    mAudioThreadShouldCallFillBuffersOnce = true;
    while (mAudioThreadShouldCallFillBuffersOnce)
    {
        std::this_thread::sleep_for(std::chrono::milliseconds(50));
    }

    // Reenable the audio thread
    mAudioThreadFillBuffersLoopRunning = true;

    return paContinue;
}

void AudioIoCallback::CallbackCheckCompletion(
    int &callbackReturn, unsigned long len)
{
    if (mPaused)
        return;

    bool done = mPlaybackSchedule.PassIsComplete();
    if (!done)
        return;

    done = mPlaybackSchedule.PlayingAtSpeed()
           // some leftover length allowed in this case
           || (mPlaybackSchedule.PlayingStraight() && len == 0);
    if (!done)
        return;

#ifdef EXPERIMENTAL_MIDI_OUT
    mMidiOutputComplete = true,
#endif
    callbackReturn = paComplete;
}

#ifdef EXPERIMENTAL_MIDI_OUT

PmTimestamp MidiTime(void *WXUNUSED(info))
{
    return AudioIO::Get()->MidiTime();
}

// Set up state to iterate NoteTrack events in sequence.
// Sends MIDI control changes up to the starting point mT0
// if send is true. Output is delayed by offset to facilitate
// looping (each iteration is delayed more).
void AudioIoCallback::PrepareMidiIterator(bool send, double offset)
{
    int i;
    int nTracks = mMidiPlaybackTracks.size();
    // instead of initializing with an Alg_seq, we use begin_seq()
    // below to add ALL Alg_seq's.
    mIterator = std::make_unique<Alg_iterator>(nullptr, false);
    // Iterator not yet initialized, must add each track...
    for (i = 0; i < nTracks; i++)
    {
        const auto t = mMidiPlaybackTracks[i].get();
        Alg_seq_ptr seq = &t->GetSeq();
        // mark sequence tracks as "in use" since we're handing this
        // off to another thread and want to make sure nothing happens
        // to the data until playback finishes. This is just a sanity check.
        seq->set_in_use(true);
        mIterator->begin_seq(seq,
                             // casting away const, but allegro just uses the pointer as an opaque "cookie"
                             (void *)t, t->GetOffset() + offset);
    }
    GetNextEvent(); // prime the pump for FillMidiBuffers

    // Start MIDI from current cursor position
    mSendMidiState = true;
    while (mNextEvent &&
           mNextEventTime < mPlaybackSchedule.mT0 + offset)
    {
        if (send)
            OutputEvent();
        GetNextEvent();
    }
    mSendMidiState = false;
}

bool AudioIoCallback::StartPortMidiStream()
{
    int i;
    int nTracks = mMidiPlaybackTracks.size();
    // Only start MIDI stream if there is an open track
    if (nTracks == 0)
        return false;

    // wxPrintf("StartPortMidiStream: mT0 %g mTime %g\n",
    //        mT0, mTime);

    /* get midi playback device */
    PmDeviceID playbackDevice = Pm_GetDefaultOutputDeviceID();
    wxString playbackDeviceName = gPrefs->Read(wxT("/MidiIO/PlaybackDevice"),
                                               wxT(""));
    mSynthLatency = gPrefs->Read(wxT("/MidiIO/SynthLatency"),
                                 DEFAULT_SYNTH_LATENCY);
    if (wxStrcmp(playbackDeviceName, wxT("")) != 0)
    {
        for (i = 0; i < Pm_CountDevices(); i++)
        {
            const PmDeviceInfo *info = Pm_GetDeviceInfo(i);
            if (!info)
                continue;
            if (!info->output)
                continue;
            wxString interf = wxSafeConvertMB2WX(info->interf);
            wxString name = wxSafeConvertMB2WX(info->name);
            interf.Append(wxT(": ")).Append(name);
            if (wxStrcmp(interf, playbackDeviceName) == 0)
            {
                playbackDevice = i;
            }
        }
    } // (else playback device has Pm_GetDefaultOuputDeviceID())

    if (playbackDevice < 0)
        return false;

    /* open output device */
    mLastPmError = Pm_OpenOutput(&mMidiStream,
                                 playbackDevice,
                                 NULL,
                                 0,
                                 &::MidiTime,
                                 NULL,
                                 MIDI_MINIMAL_LATENCY_MS);
    if (mLastPmError == pmNoError)
    {
        mMidiStreamActive = true;
        mMidiPaused = false;
        mMidiLoopPasses = 0;
        mMidiOutputComplete = false;
        mMaxMidiTimestamp = 0;
        PrepareMidiIterator();

        // It is ok to call this now, but do not send timestamped midi
        // until after the first audio callback, which provides necessary
        // data for MidiTime().
        Pm_Synchronize(mMidiStream); // start using timestamps
        // start midi output flowing (pending first audio callback)
        mMidiThreadFillBuffersLoopRunning = true;
    }
    return (mLastPmError == pmNoError);
}
#endif

size_t AudioIoCallback::GetCommonlyReadyPlayback()
{
    if (mPlaybackTracks.empty())
        return 0;

    auto commonlyAvail = mPlaybackBuffers[0]->AvailForGet();
    for (unsigned i = 1; i < mPlaybackTracks.size(); ++i)
        commonlyAvail = std::min(commonlyAvail,
                                 mPlaybackBuffers[i]->AvailForGet());
    return commonlyAvail;
}

void AudioIoCallback::SetListener(
    const std::shared_ptr<AudioIOListener> &listener)
{
    if (IsBusy())
        return;

    mListener = listener;
}

#ifdef EXPERIMENTAL_MIDI_OUT

static Alg_update gAllNotesOff; // special event for loop ending
// the fields of this event are never used, only the address is important

double AudioIoCallback::UncorrectedMidiEventTime()
{
    double time;
    if (mPlaybackSchedule.mEnvelope)
        time =
            mPlaybackSchedule.RealDuration(mNextEventTime - MidiLoopOffset()) + mPlaybackSchedule.mT0 + (mMidiLoopPasses * mPlaybackSchedule.mWarpedLength);
    else
        time = mNextEventTime;

    return time + PauseTime();
}

void AudioIoCallback::OutputEvent()
{
    int channel = (mNextEvent->chan) & 0xF; // must be in [0..15]
    int command = -1;
    int data1 = -1;
    int data2 = -1;

    double eventTime = UncorrectedMidiEventTime();

    // 0.0005 is for rounding
    double time = eventTime + 0.0005 -
                  (mSynthLatency * 0.001);

    time += 1; // MidiTime() has a 1s offset
    // state changes have to go out without delay because the
    // midi stream time gets reset when playback starts, and
    // we don't want to leave any control changes scheduled for later
    if (time < 0 || mSendMidiState)
        time = 0;
    PmTimestamp timestamp = (PmTimestamp)(time * 1000); /* s to ms */

    // The special event gAllNotesOff means "end of playback, send
    // all notes off on all channels"
    if (mNextEvent == &gAllNotesOff)
    {
        bool looping = mPlaybackSchedule.Looping();
        AllNotesOff(looping);
        if (looping)
        {
            // jump back to beginning of loop
            ++mMidiLoopPasses;
            PrepareMidiIterator(false, MidiLoopOffset());
        }
        else
        {
            mNextEvent = NULL;
        }
        return;
    }

    // if mNextEvent's channel is visible, play it, visibility can
    // be updated while playing. Be careful: if we have a note-off,
    // then we must not pay attention to the channel selection
    // or mute/solo buttons because we must turn the note off
    // even if the user changed something after the note began
    // Note that because multiple tracks can output to the same
    // MIDI channels, it is not a good idea to send "All Notes Off"
    // when the user presses the mute button. We have no easy way
    // to know what notes are sounding on any given muted track, so
    // we'll just wait for the note-off events to happen.
    // Also note that note-offs are only sent when we call
    // mIterator->request_note_off(), so notes that are not played
    // will not generate random note-offs. There is the interesting
    // case that if the playback is paused, all-notes-off WILL be sent
    // and if playback resumes, the pending note-off events WILL also
    // be sent (but if that is a problem, there would also be a problem
    // in the non-pause case.
    if (((mNextEventTrack->IsVisibleChan(channel)) &&
         // only play if note is not muted:
         !((mHasSolo || mNextEventTrack->GetMute()) &&
           !mNextEventTrack->GetSolo())) ||
        (mNextEvent->is_note() && !mNextIsNoteOn))
    {
        // Note event
        if (mNextEvent->is_note() && !mSendMidiState)
        {
            // Pitch and velocity
            data1 = mNextEvent->get_pitch();
            if (mNextIsNoteOn)
            {
                data2 = mNextEvent->get_loud(); // get velocity
                int offset = mNextEventTrack->GetVelocity();
                data2 += offset; // offset comes from per-track slider
                // clip velocity to insure a legal note-on value
                data2 = (data2 < 1 ? 1 : (data2 > 127 ? 127 : data2));
                // since we are going to play this note, we need to get a note_off
                mIterator->request_note_off();

#ifdef AUDIO_IO_GB_MIDI_WORKAROUND
                mPendingNotesOff.push_back(std::make_pair(channel, data1));
#endif
            }
            else
            {
                data2 = 0; // 0 velocity means "note off"
#ifdef AUDIO_IO_GB_MIDI_WORKAROUND
                auto end = mPendingNotesOff.end();
                auto iter = std::find(
                    mPendingNotesOff.begin(), end, std::make_pair(channel, data1));
                if (iter != end)
                    mPendingNotesOff.erase(iter);
#endif
            }
            command = 0x90; // MIDI NOTE ON (or OFF when velocity == 0)
            // Update event
        }
        else if (mNextEvent->is_update())
        {
            // this code is based on allegrosmfwr.cpp -- it could be improved
            // by comparing attribute pointers instead of string compares
            Alg_update_ptr update = (Alg_update_ptr)mNextEvent;
            const char *name = update->get_attribute();

            if (!strcmp(name, "programi"))
            {
                // Instrument change
                data1 = update->parameter.i;
                data2 = 0;
                command = 0xC0; // MIDI PROGRAM CHANGE
            }
            else if (!strncmp(name, "control", 7))
            {
                // Controller change

                // The number of the controller being changed is embedded
                // in the parameter name.
                data1 = atoi(name + 7);
                // Allegro normalizes controller values
                data2 = ROUND(update->parameter.r * 127);
                command = 0xB0;
            }
            else if (!strcmp(name, "bendr"))
            {
                // Bend change

                // Reverse Allegro's post-processing of bend values
                int temp = ROUND(0x2000 * (update->parameter.r + 1));
                if (temp > 0x3fff)
                    temp = 0x3fff; // 14 bits maximum
                if (temp < 0)
                    temp = 0;
                data1 = temp & 0x7f; // low 7 bits
                data2 = temp >> 7;   // high 7 bits
                command = 0xE0;      // MIDI PITCH BEND
            }
            else if (!strcmp(name, "pressurer"))
            {
                // Pressure change
                data1 = (int)(update->parameter.r * 127);
                if (update->get_identifier() < 0)
                {
                    // Channel pressure
                    data2 = 0;
                    command = 0xD0; // MIDI CHANNEL PRESSURE
                }
                else
                {
                    // Key pressure
                    data2 = data1;
                    data1 = update->get_identifier();
                    command = 0xA0; // MIDI POLY PRESSURE
                }
            }
        }
        if (command != -1)
        {
            // keep track of greatest timestamp used
            if (timestamp > mMaxMidiTimestamp)
            {
                mMaxMidiTimestamp = timestamp;
            }
            Pm_WriteShort(mMidiStream, timestamp,
                          Pm_Message((int)(command + channel),
                                     (long)data1, (long)data2));
            /* wxPrintf("Pm_WriteShort %lx (%p) @ %d, advance %d\n",
                   Pm_Message((int) (command + channel),
                              (long) data1, (long) data2),
                              mNextEvent, timestamp, timestamp - Pt_Time()); */
        }
    }
}

void AudioIoCallback::GetNextEvent()
{
    mNextEventTrack = NULL; // clear it just to be safe
    // now get the next event and the track from which it came
    double nextOffset;
    if (!mIterator)
    {
        mNextEvent = NULL;
        return;
    }
    auto midiLoopOffset = MidiLoopOffset();
    mNextEvent = mIterator->next(&mNextIsNoteOn,
                                 (void **)&mNextEventTrack,
                                 &nextOffset, mPlaybackSchedule.mT1 + midiLoopOffset);

    mNextEventTime = mPlaybackSchedule.mT1 + midiLoopOffset + 1;
    if (mNextEvent)
    {
        mNextEventTime = (mNextIsNoteOn ? mNextEvent->time : mNextEvent->get_end_time()) + nextOffset;
        ;
    }
    if (mNextEventTime > (mPlaybackSchedule.mT1 + midiLoopOffset))
    { // terminate playback at mT1
        mNextEvent = &gAllNotesOff;
        mNextEventTime = mPlaybackSchedule.mT1 + midiLoopOffset - ALG_EPS;
        mNextIsNoteOn = true; // do not look at duration
        mIterator->end();
        mIterator.reset(); // debugging aid
    }
}

bool AudioIoCallback::SetHasSolo(bool hasSolo)
{
    mHasSolo = hasSolo;
    return mHasSolo;
}

void AudioIoCallback::FillMidiBuffers()
{
    // Keep track of time paused. If not paused, fill buffers.
    if (IsPaused())
    {
        if (!mMidiPaused)
        {
            mMidiPaused = true;
            AllNotesOff(); // to avoid hanging notes during pause
        }
        return;
    }

    if (mMidiPaused)
    {
        mMidiPaused = false;
    }

    //---- Duplicated code -----
    // TODO this code is duplicated.  Look for mbHasSoloTracks.
    bool hasSolo = false;
    auto numPlaybackTracks = mPlaybackTracks.size();
    for (unsigned t = 0; t < numPlaybackTracks; t++)
        if (mPlaybackTracks[t]->GetSolo())
        {
            hasSolo = true;
            break;
        }
    auto numMidiPlaybackTracks = mMidiPlaybackTracks.size();
    for (unsigned t = 0; t < numMidiPlaybackTracks; t++)
        if (mMidiPlaybackTracks[t]->GetSolo())
        {
            hasSolo = true;
            break;
        }
    SetHasSolo(hasSolo);
    //---- End duplicated code -----

    // If we compute until mNextEventTime > current audio time,
    // we would have a built-in compute-ahead of mAudioOutLatency, and
    // it's probably good to compute MIDI when we compute audio (so when
    // we stop, both stop about the same time).
    double time = AudioTime(); // compute to here
    // But if mAudioOutLatency is very low, we might need some extra
    // compute-ahead to deal with mSynthLatency or even this thread.
    double actual_latency = (MIDI_SLEEP + THREAD_LATENCY +
                             MIDI_MINIMAL_LATENCY_MS + mSynthLatency) *
                            0.001;
    if (actual_latency > mAudioOutLatency)
    {
        time += actual_latency - mAudioOutLatency;
    }
    while (mNextEvent &&
           UncorrectedMidiEventTime() < time)
    {
        OutputEvent();
        GetNextEvent();
    }
}

double AudioIoCallback::PauseTime()
{
    return mNumPauseFrames / mRate;
}

// MidiTime() is an estimate in milliseconds of the current audio
// output (DAC) time + 1s. In other words, what audacity track time
// corresponds to the audio (including pause insertions) at the output?
//
PmTimestamp AudioIoCallback::MidiTime()
{
    // note: the extra 0.0005 is for rounding. Round down by casting to
    // unsigned long, then convert to PmTimeStamp (currently signed)

    // PRL: the time correction is really Midi latency achieved by different
    // means than specifying it to Pm_OpenStream.  The use of the accumulated
    // sample count generated by the audio callback (in AudioTime()) might also
    // have the virtue of keeping the Midi output synched with audio.

    PmTimestamp ts;
    // subtract latency here because mSystemMinusAudioTime gets us
    // to the current *write* time, but we're writing ahead by audio output
    // latency (mAudioOutLatency).
    double now = SystemTime(mUsingAlsa);
    ts = (PmTimestamp)((unsigned long)(1000 * (now + 1.0005 -
                                               mSystemMinusAudioTimePlusLatency)));
    // wxPrintf("AudioIO::MidiTime() %d time %g sys-aud %g\n",
    //        ts, now, mSystemMinusAudioTime);
    return ts + MIDI_MINIMAL_LATENCY_MS;
}

void AudioIoCallback::AllNotesOff(bool looping)
{
#ifdef __WXGTK__
    bool doDelay = !looping;
#else
    bool doDelay = false;
    static_cast<void>(looping); // compiler food.
#endif

    // to keep track of when MIDI should all be delivered,
    // update mMaxMidiTimestamp to now:
    PmTimestamp now = MidiTime();
    if (mMaxMidiTimestamp < now)
    {
        mMaxMidiTimestamp = now;
    }
#ifdef AUDIO_IO_GB_MIDI_WORKAROUND
    // PRL:
    // Send individual note-off messages for each note-on not yet paired.

    // RBD:
    // Even this did not work as planned. My guess is ALSA does not use
    // a "stable sort" for timed messages, so that when a note-off is
    // added later at the same time as a future note-on, the order is
    // not respected, and the note-off can go first, leaving a stuck note.
    // The workaround here is to use mMaxMidiTimestamp to ensure that
    // note-offs come at least 1ms later than any previous message

    // PRL:
    // I think we should do that only when stopping or pausing, not when looping
    // Note that on Linux, MIDI always uses ALSA, no matter whether portaudio
    // uses some other host api.

    mMaxMidiTimestamp += 1;
    for (const auto &pair : mPendingNotesOff)
    {
        Pm_WriteShort(mMidiStream,
                      (doDelay ? mMaxMidiTimestamp : 0),
                      Pm_Message(
                          0x90 + pair.first, pair.second, 0));
        mMaxMidiTimestamp++; // allow 1ms per note-off
    }
    mPendingNotesOff.clear();

    // Proceed to do the usual messages too.
#endif

    for (int chan = 0; chan < 16; chan++)
    {
        Pm_WriteShort(mMidiStream,
                      (doDelay ? mMaxMidiTimestamp : 0),
                      Pm_Message(0xB0 + chan, 0x7B, 0));
        mMaxMidiTimestamp++; // allow 1ms per all-notes-off
    }
}

#endif

void AudioIoCallback::ComputeMidiTimings(
    const PaStreamCallbackTimeInfo *timeInfo,
    unsigned long framesPerBuffer)
{
#ifdef EXPERIMENTAL_MIDI_OUT
    if (mCallbackCount++ == 0)
    {
        // This is effectively mSystemMinusAudioTime when the buffer is empty:
        mStartTime = SystemTime(mUsingAlsa) - mPlaybackSchedule.mT0;
        // later, mStartTime - mSystemMinusAudioTime will tell us latency
    }

    /* for Linux, estimate a smooth audio time as a slowly-changing
       offset from system time */
    // rnow is system time as a double to simplify math
    double rnow = SystemTime(mUsingAlsa);
    // anow is next-sample-to-be-computed audio time as a double
    double anow = AudioTime();

    if (mUsingAlsa)
    {
        // timeInfo's fields are not all reliable.

        // enow is audio time estimated from our clock synchronization protocol,
        //   which produces mSystemMinusAudioTime. But we want the estimate
        //   to drift low, so we steadily increase mSystemMinusAudioTime to
        //   simulate a fast system clock or a slow audio clock. If anow > enow,
        //   we'll update mSystemMinusAudioTime to keep in sync. (You might think
        //   we could just use anow as the "truth", but it has a lot of jitter,
        //   so we are using enow to smooth out this jitter, in fact to < 1ms.)
        // Add worst-case clock drift using previous framesPerBuffer:
        const auto increase =
            mAudioFramesPerBuffer * 0.0002 / mRate;
        mSystemMinusAudioTime += increase;
        mSystemMinusAudioTimePlusLatency += increase;
        double enow = rnow - mSystemMinusAudioTime;

        // now, use anow instead if it is ahead of enow
        if (anow > enow)
        {
            mSystemMinusAudioTime = rnow - anow;
            // Update our mAudioOutLatency estimate during the first 20 callbacks.
            // During this period, the buffer should fill. Once we have a good
            // estimate of mSystemMinusAudioTime (expected in fewer than 20 callbacks)
            // we want to stop the updating in case there is clock drift, which would
            // cause the mAudioOutLatency estimation to drift as well. The clock drift
            // in the first 20 callbacks should be negligible, however.
            if (mCallbackCount < 20)
            {
                mAudioOutLatency = mStartTime -
                                   mSystemMinusAudioTime;
            }
            mSystemMinusAudioTimePlusLatency =
                mSystemMinusAudioTime + mAudioOutLatency;
        }
    }
    else
    {
        // If not using Alsa, rely on timeInfo to have meaningful values that are
        // more precise than the output latency value reported at stream start.
        mSystemMinusAudioTime = rnow - anow;
        mSystemMinusAudioTimePlusLatency =
            mSystemMinusAudioTime +
            (timeInfo->outputBufferDacTime - timeInfo->currentTime);
    }

    mAudioFramesPerBuffer = framesPerBuffer;
    if (IsPaused()
        // PRL:  Why was this added?  Was it only because of the mysterious
        // initial leading zeroes, now solved by setting mStreamToken early?
        // JKC: I think it's used for the MIDI time cursor.  See comments
        // at head of file about AudioTime().
        || mStreamToken <= 0)
        mNumPauseFrames += framesPerBuffer;

    // PRL:  Note that when there is a separate MIDI thread, it is effectively
    // blocked until the first visit to this line during a playback, and will
    // not read mSystemMinusAudioTimePlusLatency sooner:
    mNumFrames += framesPerBuffer;
#endif
}

// Stop recording if 'silence' is detected
// Start recording if sound detected.
//
//   By using CallAfter(), we can schedule the call to the toolbar
//   to run in the main GUI thread after the next event loop iteration.
//   That's important, because Pause() updates GUI, such as status bar,
//   and that should NOT happen in this audio non-gui thread.
void AudioIoCallback::CheckSoundActivatedRecordingLevel(
    float *inputSamples,
    unsigned long framesPerBuffer)
{
    // Quick returns if next to nothing to do.
    if (!mPauseRec)
        return;

    float maxPeak = 0.;
    for (unsigned long i = 0, cnt = framesPerBuffer * mNumCaptureChannels; i < cnt; ++i)
    {
        float sample = fabs(*(inputSamples++));
        if (sample > maxPeak)
        {
            maxPeak = sample;
        }
    }

    bool bShouldBePaused = maxPeak < mSilenceLevel;
    if (bShouldBePaused != IsPaused())
    {
        auto pListener = GetListener();
        if (pListener)
            pListener->OnSoundActivationThreshold();
    }
}

// A function to apply the requested gain, fading up or down from the
// most recently applied gain.
void AudioIoCallback::AddToOutputChannel(unsigned int chan,
                                         float *outputMeterFloats,
                                         float *outputFloats,
                                         float *tempBuf,
                                         bool drop,
                                         unsigned long len,
                                         std::shared_ptr<WaveTrack> &vt)
{
    const auto numPlaybackChannels = mNumPlaybackChannels;

    float gain = vt->GetChannelGain(chan);
    if (drop || !mAudioThreadFillBuffersLoopRunning || mPaused)
        gain = 0.0;

    // Output volume emulation: possibly copy meter samples, then
    // apply volume, then copy to the output buffer
    if (outputMeterFloats != outputFloats)
        for (unsigned i = 0; i < len; ++i)
            outputMeterFloats[numPlaybackChannels * i + chan] +=
                gain * tempBuf[i];

    float oldGain = vt->GetOldChannelGain(chan);
    if (gain != oldGain)
        vt->SetOldChannelGain(chan, gain);
    // if no microfades, jump in volume.
    if (!mbMicroFades)
        oldGain = gain;
    wxASSERT(len > 0);

    // Linear interpolate.
    float deltaGain = (gain - oldGain) / len;
    for (unsigned i = 0; i < len; i++)
        outputFloats[numPlaybackChannels * i + chan] += (oldGain + deltaGain * i) * tempBuf[i];
};

// Limit values to -1.0..+1.0
void ClampBuffer(float *pBuffer, unsigned long len)
{
    for (unsigned i = 0; i < len; i++)
        pBuffer[i] = wxClip(pBuffer[i], -1.0f, 1.0f);
}

// return true, IFF we have fully handled the callback.
//
// Mix and copy to PortAudio's output buffer
//
bool AudioIoCallback::FillOutputBuffers(
    void *outputBuffer,
    unsigned long framesPerBuffer, float *outputMeterFloats)
{
    const auto numPlaybackTracks = mPlaybackTracks.size();
    const auto numPlaybackChannels = mNumPlaybackChannels;
    const auto numCaptureChannels = mNumCaptureChannels;

    mMaxFramesOutput = 0;

    // Quick returns if next to nothing to do.
    if (mStreamToken <= 0)
        return false;
    if (!outputBuffer)
        return false;
    if (numPlaybackChannels <= 0)
        return false;

    float *outputFloats = (float *)outputBuffer;

#ifdef EXPERIMENTAL_SCRUBBING_SUPPORT
    // While scrubbing, ignore seek requests
    if (mSeek && mPlaybackSchedule.Interactive())
        mSeek = 0.0;
#endif

    if (mSeek)
    {
        mCallbackReturn = CallbackDoSeek();
        return true;
    }

    // ------ MEMORY ALLOCATION ----------------------
    // These are small structures.
    WaveTrackArray chans;
    StackAllocator<float> floatAllocator;
    std::unique_ptr<float *> tempBufs(new float *[numPlaybackChannels]);
    chans.resize(numPlaybackChannels);

    // And these are larger structures....
    // GP: not needed
    for (unsigned int c = 0; c < numPlaybackChannels; c++)
    {
        auto &buf = tempBufs.get()[c];
        buf = floatAllocator.Allocate(true, framesPerBuffer);
    }
    // ------ End of MEMORY ALLOCATION ---------------

    auto &em = RealtimeEffectManager::Get();
    em.RealtimeProcessStart();

    bool selected = false;
    int group = 0;
    int chanCnt = 0;

    // Choose a common size to take from all ring buffers
    const auto toGet =
        std::min<size_t>(framesPerBuffer, GetCommonlyReadyPlayback());

    // The drop and dropQuickly booleans are so named for historical reasons.
    // JKC: The original code attempted to be faster by doing nothing on silenced audio.
    // This, IMHO, is 'premature optimisation'.  Instead clearer and cleaner code would
    // simply use a gain of 0.0 for silent audio and go on through to the stage of
    // applying that 0.0 gain to the data mixed into the buffer.
    // Then (and only then) we would have if needed fast paths for:
    // - Applying a uniform gain of 0.0.
    // - Applying a uniform gain of 1.0.
    // - Applying some other uniform gain.
    // - Applying a linearly interpolated gain.
    // I would expect us not to need the fast paths, since linearly interpolated gain
    // is very cheap to process.

    bool drop = false;        // Track should become silent.
    bool dropQuickly = false; // Track has already been faded to silence.
    for (unsigned t = 0; t < numPlaybackTracks; t++)
    {
        std::shared_ptr<WaveTrack> vt = mPlaybackTracks[t];
        chans[chanCnt] = vt;

        // TODO: more-than-two-channels
        auto nextTrack =
            t + 1 < numPlaybackTracks
                ? mPlaybackTracks[t + 1].get()
                : nullptr;

        // First and last channel in this group (for example left and right
        // channels of stereo).
        bool firstChannel = vt->IsLeader();
        bool lastChannel = !nextTrack || nextTrack->IsLeader();

        if (firstChannel)
        {
            selected = vt->GetSelected();
            // IF mono THEN clear 'the other' channel.
            if (lastChannel && (numPlaybackChannels > 1))
            {
                // TODO: more-than-two-channels
                auto buf = tempBufs.get()[1];
                memset(buf, 0, framesPerBuffer * sizeof(float));
            }
            drop = TrackShouldBeSilent(*vt);
            dropQuickly = drop;
        }

        if (mbMicroFades)
            dropQuickly = dropQuickly && TrackHasBeenFadedOut(*vt);

        decltype(framesPerBuffer) len = 0;

        if (dropQuickly)
        {
            len = mPlaybackBuffers[t]->Discard(toGet);
            // keep going here.
            // we may still need to issue a paComplete.
        }
        else
        {
            len = mPlaybackBuffers[t]->Get((samplePtr)tempBufs.get()[chanCnt],
                                           floatSample,
                                           toGet);
            // wxASSERT( len == toGet );
            if (len < framesPerBuffer)
            {
                // This used to happen normally at the end of non-looping
                // plays, but it can also be an anomalous case where the
                // supply from FillBuffers fails to keep up with the
                // real-time demand in this thread (see bug 1932).  We
                // must supply something to the sound card, so pad it with
                // zeroes and not random garbage.
                memset((void *)&tempBufs.get()[chanCnt][len], 0,
                       (framesPerBuffer - len) * sizeof(float));
            }
            chanCnt++;
        }

        // PRL:  Bug1104:
        // There can be a difference of len in different loop passes if one channel
        // of a stereo track ends before the other!  Take a max!

        // PRL:  More recent rewrites of FillBuffers should guarantee a
        // padding out of the ring buffers so that equal lengths are
        // available, so maxLen ought to increase from 0 only once
        mMaxFramesOutput = std::max(mMaxFramesOutput, len);

        if (!lastChannel)
            continue;

        // Last channel of a track seen now
        len = mMaxFramesOutput;

        if (!dropQuickly && selected)
            len = em.RealtimeProcess(group, chanCnt, tempBufs.get(), len);
        group++;

        CallbackCheckCompletion(mCallbackReturn, len);
        if (dropQuickly) // no samples to process, they've been discarded
            continue;

        // Our channels aren't silent.  We need to pass their data on.
        //
        // Note that there are two kinds of channel count.
        // c and chanCnt are counting channels in the Tracks.
        // chan (and numPlayBackChannels) is counting output channels on the device.
        // chan = 0 is left channel
        // chan = 1 is right channel.
        //
        // Each channel in the tracks can output to more than one channel on the device.
        // For example mono channels output to both left and right output channels.
        if (len > 0)
            for (int c = 0; c < chanCnt; c++)
            {
                vt = chans[c];

                if (vt->GetChannelIgnoringPan() == Track::LeftChannel ||
                    vt->GetChannelIgnoringPan() == Track::MonoChannel)
                    AddToOutputChannel(0, outputMeterFloats, outputFloats,
                                       tempBufs.get()[c], drop, len, vt);

                if (vt->GetChannelIgnoringPan() == Track::RightChannel ||
                    vt->GetChannelIgnoringPan() == Track::MonoChannel)
                    AddToOutputChannel(1, outputMeterFloats, outputFloats,
                                       tempBufs.get()[c], drop, len, vt);
            }

        chanCnt = 0;
    }

    // Poke: If there are no playback tracks, then the earlier check
    // about the time indicator being past the end won't happen;
    // do it here instead (but not if looping or scrubbing)
    if (numPlaybackTracks == 0)
        CallbackCheckCompletion(mCallbackReturn, 0);

    // wxASSERT( maxLen == toGet );

    em.RealtimeProcessEnd();
    mLastPlaybackTimeMillis = ::wxGetUTCTimeMillis();

    ClampBuffer(outputFloats, framesPerBuffer * numPlaybackChannels);
    if (outputMeterFloats != outputFloats)
        ClampBuffer(outputMeterFloats, framesPerBuffer * numPlaybackChannels);

    return false;
}

void AudioIoCallback::UpdateTimePosition(unsigned long framesPerBuffer)
{
    // Quick returns if next to nothing to do.
    if (mStreamToken <= 0)
        return;

    // Update the position seen by drawing code
    if (mPlaybackSchedule.Interactive())
        // To do: do this in all cases and remove TrackTimeUpdate
        mPlaybackSchedule.SetTrackTime(mTimeQueue.Consumer(mMaxFramesOutput, mRate));
    else
        mPlaybackSchedule.TrackTimeUpdate(framesPerBuffer / mRate);
}

// return true, IFF we have fully handled the callback.
//
// Copy from PortAudio to our input buffers.
//
void AudioIoCallback::FillInputBuffers(
    const void *inputBuffer,
    unsigned long framesPerBuffer,
    const PaStreamCallbackFlags statusFlags,
    float *tempFloats)
{
    const auto numPlaybackTracks = mPlaybackTracks.size();
    const auto numPlaybackChannels = mNumPlaybackChannels;
    const auto numCaptureChannels = mNumCaptureChannels;

    // Quick returns if next to nothing to do.
    if (mStreamToken <= 0)
        return;
    if (!inputBuffer)
        return;
    if (numCaptureChannels <= 0)
        return;

    // If there are no playback tracks, and we are recording, then the
    // earlier checks for being past the end won't happen, so do it here.
    if (mPlaybackSchedule.PassIsComplete())
    {
        mCallbackReturn = paComplete;
    }

    // The error likely from a too-busy CPU falling behind real-time data
    // is paInputOverflow
    bool inputError =
        (statusFlags & (paInputOverflow)) && !(statusFlags & paPrimingOutput);

    // But it seems it's easy to get false positives, at least on Mac
    // So we have not decided to enable this extra detection yet in
    // production

    size_t len = framesPerBuffer;
    for (unsigned t = 0; t < numCaptureChannels; t++)
        len = std::min(len, mCaptureBuffers[t]->AvailForPut());

    if (mSimulateRecordingErrors && 100LL * rand() < RAND_MAX)
        // Make spurious errors for purposes of testing the error
        // reporting
        len = 0;

    // A different symptom is that len < framesPerBuffer because
    // the other thread, executing FillBuffers, isn't consuming fast
    // enough from mCaptureBuffers; maybe it's CPU-bound, or maybe the
    // storage device it writes is too slow
    if (mDetectDropouts &&
        ((mDetectUpstreamDropouts && inputError) ||
         len < framesPerBuffer))
    {
        // Assume that any good partial buffer should be written leftmost
        // and zeroes will be padded after; label the zeroes.
        auto start = mPlaybackSchedule.GetTrackTime() +
                     len / mRate + mRecordingSchedule.mLatencyCorrection;
        auto duration = (framesPerBuffer - len) / mRate;
        auto pLast = mLostCaptureIntervals.empty()
                         ? nullptr
                         : &mLostCaptureIntervals.back();
        if (pLast &&
            fabs(pLast->first + pLast->second - start) < 0.5 / mRate)
            // Make one bigger interval, not two butting intervals
            pLast->second = start + duration - pLast->first;
        else
            mLostCaptureIntervals.emplace_back(start, duration);
    }

    if (len < framesPerBuffer)
    {
        mLostSamples += (framesPerBuffer - len);
        wxPrintf(wxT("lost %d samples\n"), (int)(framesPerBuffer - len));
    }

    if (len <= 0)
        return;

    // We have an ASSERT in the AudioIO constructor to alert us to
    // possible issues with the (short*) cast.  We'd have a problem if
    // sizeof(short) > sizeof(float) since our buffers are sized for floats.
    for (unsigned t = 0; t < numCaptureChannels; t++)
    {

        // dmazzoni:
        // Un-interleave.  Ugly special-case code required because the
        // capture channels could be in three different sample formats;
        // it'd be nice to be able to call CopySamples, but it can't
        // handle multiplying by the gain and then clipping.  Bummer.

        switch (mCaptureFormat)
        {
        case floatSample:
        {
            float *inputFloats = (float *)inputBuffer;
            for (unsigned i = 0; i < len; i++)
                tempFloats[i] =
                    inputFloats[numCaptureChannels * i + t];
        }
        break;
        case int24Sample:
            // We should never get here. Audacity's int24Sample format
            // is different from PortAudio's sample format and so we
            // make PortAudio return float samples when recording in
            // 24-bit samples.
            wxASSERT(false);
            break;
        case int16Sample:
        {
            short *inputShorts = (short *)inputBuffer;
            short *tempShorts = (short *)tempFloats;
            for (unsigned i = 0; i < len; i++)
            {
                float tmp = inputShorts[numCaptureChannels * i + t];
                tmp = wxClip(-32768, tmp, 32767);
                tempShorts[i] = (short)(tmp);
            }
        }
        break;
        } // switch

        // JKC: mCaptureFormat must be for samples with sizeof(float) or
        // fewer bytes (because tempFloats is sized for floats).  All
        // formats are 2 or 4 bytes, so we are OK.
        // const auto put = // unused
        mCaptureBuffers[t]->Put(
            (samplePtr)tempFloats, mCaptureFormat, len);
        // wxASSERT(put == len);
        // but we can't assert in this thread
    }
}

//////////////////////////////////////////////////////////////////////
//
//    PortAudio callback thread context
//
//////////////////////////////////////////////////////////////////////

static void DoSoftwarePlaythrough(const void *inputBuffer,
                                  sampleFormat inputFormat,
                                  unsigned inputChannels,
                                  float *outputBuffer,
                                  int len)
{
    for (unsigned int i = 0; i < inputChannels; i++)
    {
        samplePtr inputPtr = ((samplePtr)inputBuffer) + (i * SAMPLE_SIZE(inputFormat));

        SamplesToFloats(inputPtr, inputFormat,
                        outputBuffer + i, len, inputChannels, 2);
    }

    // One mono input channel goes to both output channels...
    if (inputChannels == 1)
        for (int i = 0; i < len; i++)
            outputBuffer[2 * i + 1] = outputBuffer[2 * i];
}

// return true, IFF we have fully handled the callback.
// Prime the output buffer with 0's, optionally adding in the playthrough.
void AudioIoCallback::DoPlaythrough(
    const void *inputBuffer,
    void *outputBuffer,
    unsigned long framesPerBuffer,
    float *outputMeterFloats)
{
    const auto numCaptureChannels = mNumCaptureChannels;
    const auto numPlaybackChannels = mNumPlaybackChannels;

    // Quick returns if next to nothing to do.
    if (!outputBuffer)
        return;
    if (numPlaybackChannels <= 0)
        return;

    float *outputFloats = (float *)outputBuffer;
    for (unsigned i = 0; i < framesPerBuffer * numPlaybackChannels; i++)
        outputFloats[i] = 0.0;

    if (inputBuffer && mSoftwarePlaythrough)
    {
        DoSoftwarePlaythrough(inputBuffer, mCaptureFormat,
                              numCaptureChannels,
                              (float *)outputBuffer, (int)framesPerBuffer);
    }

    // Copy the results to outputMeterFloats if necessary
    if (outputMeterFloats != outputFloats)
    {
        for (unsigned i = 0; i < framesPerBuffer * numPlaybackChannels; ++i)
        {
            outputMeterFloats[i] = outputFloats[i];
        }
    }
}

/* Send data to recording VU meter if applicable */
// Also computes rms
void AudioIoCallback::SendVuInputMeterData(
    float *inputSamples,
    unsigned long framesPerBuffer)
{
    const auto numCaptureChannels = mNumCaptureChannels;

    auto pInputMeter = mInputMeter.lock();
    if (!pInputMeter)
        return;
    if (pInputMeter->IsMeterDisabled())
        return;

    // get here if meters are actually live , and being updated
    /* It's critical that we don't update the meters while StopStream is
     * trying to stop PortAudio, otherwise it can lead to a freeze.  We use
     * two variables to synchronize:
     *   mUpdatingMeters tells StopStream when the callback is about to enter
     *     the code where it might update the meters, and
     *   mUpdateMeters is how the rest of the code tells the callback when it
     *     is allowed to actually do the updating.
     * Note that mUpdatingMeters must be set first to avoid a race condition.
     */
    // TODO use atomics instead.
    mUpdatingMeters = true;
    if (mUpdateMeters)
    {
        pInputMeter->UpdateDisplay(numCaptureChannels,
                                   framesPerBuffer,
                                   inputSamples);
    }
    mUpdatingMeters = false;
}

/* Send data to playback VU meter if applicable */
void AudioIoCallback::SendVuOutputMeterData(
    float *outputMeterFloats,
    unsigned long framesPerBuffer)
{
    const auto numPlaybackChannels = mNumPlaybackChannels;

    auto pOutputMeter = mOutputMeter.lock();
    if (!pOutputMeter)
        return;
    if (pOutputMeter->IsMeterDisabled())
        return;
    if (!outputMeterFloats)
        return;

    // Get here if playback meter is live
    /* It's critical that we don't update the meters while StopStream is
     * trying to stop PortAudio, otherwise it can lead to a freeze.  We use
     * two variables to synchronize:
     *  mUpdatingMeters tells StopStream when the callback is about to enter
     *    the code where it might update the meters, and
     *  mUpdateMeters is how the rest of the code tells the callback when it
     *    is allowed to actually do the updating.
     * Note that mUpdatingMeters must be set first to avoid a race condition.
     */
    mUpdatingMeters = true;
    if (mUpdateMeters)
    {
        pOutputMeter->UpdateDisplay(numPlaybackChannels,
                                    framesPerBuffer,
                                    outputMeterFloats);

        // v Vaughan, 2011-02-25: Moved this update back to TrackPanel::OnTimer()
        //     as it helps with playback issues reported by Bill and noted on Bug 258.
        //     The problem there occurs if Software Playthrough is on.
        //     Could conditionally do the update here if Software Playthrough is off,
        //     and in TrackPanel::OnTimer() if Software Playthrough is on, but not now.
        //  PRL 12 Jul 2015: and what was in TrackPanel::OnTimer is now handled by means of event
        //  type EVT_TRACK_PANEL_TIMER
        // MixerBoard* pMixerBoard = mOwningProject->GetMixerBoard();
        // if (pMixerBoard)
        //    pMixerBoard->UpdateMeters(GetStreamTime(),
        //                               (pProj->GetControlToolBar()->GetLastPlayMode() == loopedPlay));
    }
    mUpdatingMeters = false;
}

unsigned AudioIoCallback::CountSoloingTracks()
{
    const auto numPlaybackTracks = mPlaybackTracks.size();

    // MOVE_TO: CountSoloedTracks() function
    unsigned numSolo = 0;
    for (unsigned t = 0; t < numPlaybackTracks; t++)
        if (mPlaybackTracks[t]->GetSolo())
            numSolo++;
#ifdef EXPERIMENTAL_MIDI_OUT
    auto numMidiPlaybackTracks = mMidiPlaybackTracks.size();
    for (unsigned t = 0; t < numMidiPlaybackTracks; t++)
        if (mMidiPlaybackTracks[t]->GetSolo())
            numSolo++;
#endif
    return numSolo;
}

// TODO: Consider making the two Track status functions into functions of
// WaveTrack.

// true IFF the track should be silent.
// The track may not yet be silent, since it may still be
// fading out.
bool AudioIoCallback::TrackShouldBeSilent(const WaveTrack &wt)
{
    return mPaused || (!wt.GetSolo() && (
                                            // Cut if somebody else is soloing
                                            mbHasSoloTracks ||
                                            // Cut if we're muted (and not soloing)
                                            wt.GetMute()));
}

// This is about micro-fades.
bool AudioIoCallback::TrackHasBeenFadedOut(const WaveTrack &wt)
{
    const auto channel = wt.GetChannelIgnoringPan();
    if ((channel == Track::LeftChannel || channel == Track::MonoChannel) &&
        wt.GetOldChannelGain(0) != 0.0)
        return false;
    if ((channel == Track::RightChannel || channel == Track::MonoChannel) &&
        wt.GetOldChannelGain(1) != 0.0)
        return false;
    return true;
}

bool AudioIoCallback::AllTracksAlreadySilent()
{
    const bool dropAllQuickly = std::all_of(
        mPlaybackTracks.begin(), mPlaybackTracks.end(),
        [&](const std::shared_ptr<WaveTrack> &vt)
        { return TrackShouldBeSilent(*vt) &&
                 TrackHasBeenFadedOut(*vt); });
    return dropAllQuickly;
}
