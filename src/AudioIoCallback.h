/**********************************************************************

  Tenacity

  AudioIoCallback.h

  Dominic Mazzoni

  Avery King split from AudioIO.h

**********************************************************************/

#pragma once

// Tenacity libraries
#include <lib-audio-devices/AudioIOBase.h>

#include <memory>
#include <portaudio.h>

#include "MessageBuffer.h"
#include "Mix.h"
#include "PlaybackSchedule.h"
#include "RingBuffer.h"
#include "WaveTrack.h"
    
/** @brief AudioIoCallback is a class that implements the callback required by 
 * PortAudio. The callback needs to be responsive, has no GUI, and copies data
 * into and out of the sound card buffers. It also sends data to the meters.
 **/
class TENACITY_DLL_API AudioIoCallback /* not final */ : public AudioIOBase
{
    public:
        AudioIoCallback();
        ~AudioIoCallback();

    public:
        // This function executes in a thread spawned by the PortAudio library
        int AudioCallback(
            const void *inputBuffer, void *outputBuffer,
            unsigned long framesPerBuffer,
            const PaStreamCallbackTimeInfo *timeInfo,
            const PaStreamCallbackFlags statusFlags, void *userData);

    #ifdef EXPERIMENTAL_MIDI_OUT
        void PrepareMidiIterator(bool send = true, double offset = 0);
        bool StartPortMidiStream();

        // Compute nondecreasing real time stamps, accounting for pauses, but not the
        // synth latency.
        double UncorrectedMidiEventTime();

        void OutputEvent();
        void FillMidiBuffers();
        void GetNextEvent();
        double PauseTime();
        void AllNotesOff(bool looping = false);

        /** \brief Compute the current PortMidi timestamp time.
         *
         * This is used by PortMidi to synchronize midi time to audio samples
         */
        PmTimestamp MidiTime();

        // Note: audio code solves the problem of soloing/muting tracks by scanning
        // all playback tracks on every call to the audio buffer fill routine.
        // We do the same for Midi, but it seems wasteful for at least two
        // threads to be frequently polling to update status. This could be
        // eliminated (also with a reduction in code I think) by updating mHasSolo
        // each time a solo button is activated or deactivated. For now, I'm
        // going to do this polling in the FillMidiBuffer routine to localize
        // changes for midi to the midi code, but I'm declaring the variable
        // here so possibly in the future, Audio code can use it too. -RBD
    private:
        bool mHasSolo; // is any playback solo button pressed?
    public:
        bool SetHasSolo(bool hasSolo);
        bool GetHasSolo() { return mHasSolo; }
    #endif

        std::shared_ptr<AudioIOListener> GetListener() const
        {
            return mListener.lock();
        }
        void SetListener(const std::shared_ptr<AudioIOListener> &listener);

        // Part of the callback
        int CallbackDoSeek();

        // Part of the callback
        void CallbackCheckCompletion(
            int &callbackReturn, unsigned long len);

        int mbHasSoloTracks;
        int mCallbackReturn;
        // Helpers to determine if tracks have already been faded out.
        unsigned CountSoloingTracks();
        bool TrackShouldBeSilent(const WaveTrack &wt);
        bool TrackHasBeenFadedOut(const WaveTrack &wt);
        bool AllTracksAlreadySilent();

        // These eight functions do different parts of AudioCallback().
        void ComputeMidiTimings(
            const PaStreamCallbackTimeInfo *timeInfo,
            unsigned long framesPerBuffer);
        void CheckSoundActivatedRecordingLevel(
            float *inputSamples,
            unsigned long framesPerBuffer);
        void AddToOutputChannel(unsigned int chan,
                                float *outputMeterFloats,
                                float *outputFloats,
                                float *tempBuf,
                                bool drop,
                                unsigned long len,
                                std::shared_ptr<WaveTrack> &vt);
        bool FillOutputBuffers(
            void *outputBuffer,
            unsigned long framesPerBuffer, float *outputMeterFloats);
        void FillInputBuffers(
            const void *inputBuffer,
            unsigned long framesPerBuffer,
            const PaStreamCallbackFlags statusFlags,
            float *tempFloats);
        void UpdateTimePosition(
            unsigned long framesPerBuffer);
        void DoPlaythrough(
            const void *inputBuffer,
            void *outputBuffer,
            unsigned long framesPerBuffer,
            float *outputMeterFloats);
        void SendVuInputMeterData(
            float *inputSamples,
            unsigned long framesPerBuffer);
        void SendVuOutputMeterData(
            float *outputMeterFloats,
            unsigned long framesPerBuffer);

    // Required by these functions...
    #ifdef EXPERIMENTAL_MIDI_OUT
        double AudioTime()
        {
            return mPlaybackSchedule.mT0 + mNumFrames / mRate;
        }
    #endif

        /** \brief Get the number of audio samples ready in all of the playback
         * buffers.
         *
         * Returns the smallest of the buffer ready space values in the event that
         * they are different. */
        size_t GetCommonlyReadyPlayback();

    #ifdef EXPERIMENTAL_MIDI_OUT
        //   MIDI_PLAYBACK:
        PmStream *mMidiStream;
        int mLastPmError;

        /// Latency of MIDI synthesizer
        long mSynthLatency; // ms

        // These fields are used to synchronize MIDI with audio:

        /// Number of frames output, including pauses
        volatile long mNumFrames;
        /// How many frames of zeros were output due to pauses?
        volatile long mNumPauseFrames;
        /// total of backward jumps
        volatile int mMidiLoopPasses;
        inline double MidiLoopOffset()
        {
            return mMidiLoopPasses * (mPlaybackSchedule.mT1 - mPlaybackSchedule.mT0);
        }

        volatile long mAudioFramesPerBuffer;
        /// Used by Midi process to record that pause has begun,
        /// so that AllNotesOff() is only delivered once
        volatile bool mMidiPaused;
        /// The largest timestamp written so far, used to delay
        /// stream closing until last message has been delivered
        PmTimestamp mMaxMidiTimestamp;

        /// Offset from ideal sample computation time to system time,
        /// where "ideal" means when we would get the callback if there
        /// were no scheduling delays or computation time
        double mSystemMinusAudioTime;
        /// audio output latency reported by PortAudio
        /// (initially; for Alsa, we adjust it to the largest "observed" value)
        double mAudioOutLatency;

        // Next two are used to adjust the previous two, if
        // PortAudio does not provide the info (using ALSA):

        /// time of first callback
        /// used to find "observed" latency
        double mStartTime;
        /// number of callbacks since stream start
        long mCallbackCount;

        /// Make just one variable to communicate from audio to MIDI thread,
        /// to avoid problems of atomicity of updates
        volatile double mSystemMinusAudioTimePlusLatency;

        Alg_seq *mSeq;
        std::unique_ptr<Alg_iterator> mIterator;
        /// The next event to play (or null)
        Alg_event *mNextEvent;

    #ifdef AUDIO_IO_GB_MIDI_WORKAROUND
        std::vector<std::pair<int, int>> mPendingNotesOff;
    #endif

        /// Real time at which the next event should be output, measured in seconds.
        /// Note that this could be a note's time+duration for note offs.
        double mNextEventTime;
        /// Track of next event
        NoteTrack *mNextEventTrack;
        /// Is the next event a note-on?
        bool mNextIsNoteOn;
        /// when true, mSendMidiState means send only updates, not note-on's,
        /// used to send state changes that precede the selected notes
        bool mSendMidiState;
        NoteTrackConstArray mMidiPlaybackTracks;
    #endif

        ArrayOf<std::unique_ptr<Resample>> mResample;
        ArrayOf<std::unique_ptr<RingBuffer>> mCaptureBuffers;
        WaveTrackArray mCaptureTracks;
        ArrayOf<std::unique_ptr<RingBuffer>> mPlaybackBuffers;
        WaveTrackArray mPlaybackTracks;

        ArrayOf<std::unique_ptr<Mixer>> mPlaybackMixers;
        static int mNextStreamToken;
        double mFactor;
        unsigned long mMaxFramesOutput; // The actual number of frames output.
        bool mbMicroFades;

        double mSeek;
        double mPlaybackRingBufferSecs;
        double mCaptureRingBufferSecs;

        /// Preferred batch size for replenishing the playback RingBuffer
        size_t mPlaybackSamplesToCopy;
        /// Occupancy of the queue we try to maintain, with bigger batches if needed
        size_t mPlaybackQueueMinimum;

        double mMinCaptureSecsToCopy;
        bool mSoftwarePlaythrough;
        /// True if Sound Activated Recording is enabled
        bool mPauseRec;
        float mSilenceLevel;
        unsigned int mNumCaptureChannels;
        unsigned int mNumPlaybackChannels;
        sampleFormat mCaptureFormat;
        unsigned long long mLostSamples{0};
        volatile bool mAudioThreadShouldCallFillBuffersOnce;
        volatile bool mAudioThreadFillBuffersLoopRunning;
        volatile bool mAudioThreadFillBuffersLoopActive;

        wxLongLong mLastPlaybackTimeMillis;

    #ifdef EXPERIMENTAL_MIDI_OUT
        volatile bool mMidiThreadFillBuffersLoopRunning;
        volatile bool mMidiThreadFillBuffersLoopActive;
    #endif

        volatile double mLastRecordingOffset;
        PaError mLastPaError;

    protected:
        bool mUpdateMeters;
        volatile bool mUpdatingMeters;

        std::weak_ptr<AudioIOListener> mListener;

        bool mUsingAlsa{false};

        // For cacheing supported sample rates
        static double mCachedBestRateOut;
        static bool mCachedBestRatePlaying;
        static bool mCachedBestRateCapturing;

        // Serialize main thread and PortAudio thread's attempts to pause and change
        // the state used by the third, Audio thread.
        std::mutex mSuspendAudioThread;

    #ifdef EXPERIMENTAL_SCRUBBING_SUPPORT
    public:
        struct ScrubState : public NonInterferingBase
        {
            ScrubState(double t0,
                        double rate,
                        const ScrubbingOptions &options)
                : mRate(rate)
                , mStartTime( t0 )
            {
                const double t1 = options.bySpeed ? options.initSpeed : t0;
                Update( t1, options );
            }

            void Update(double end, const ScrubbingOptions &options)
            {
                // Called by another thread
                mMessage.Write({ end, options });
            }

            void Get(sampleCount &startSample, sampleCount &endSample,
                    sampleCount inDuration, sampleCount &duration)
            {
                // Called by the thread that calls AudioIO::FillBuffers
                startSample = endSample = duration = -1LL;
                sampleCount s0Init;

                Message message( mMessage.Read() );
                if ( !mStarted ) {
                    s0Init = llrint( mRate *
                        std::max( message.options.minTime,
                        std::min( message.options.maxTime, mStartTime ) ) );

                    // Make some initial silence. This is not needed in the case of
                    // keyboard scrubbing or play-at-speed, because the initial speed
                    // is known when this function is called the first time.
                    if ( !(message.options.isKeyboardScrubbing ||
                        message.options.isPlayingAtSpeed) ) {
                        mData.mS0 = mData.mS1 = s0Init;
                        mData.mGoal = -1;
                        mData.mDuration = duration = inDuration;
                        mData.mSilence = 0;
                    }
                }

                if (mStarted || message.options.isKeyboardScrubbing ||
                    message.options.isPlayingAtSpeed) {
                    Data newData;
                    inDuration += mAccumulatedSeekDuration;

                    // If already started, use the previous end as NEW start.
                    const auto s0 = mStarted ? mData.mS1 : s0Init;
                    const sampleCount s1 ( message.options.bySpeed
                        ? s0.as_double() +
                        lrint(inDuration.as_double() * message.end) // end is a speed
                        : lrint(message.end * mRate)            // end is a time
                    );
                    auto success =
                        newData.Init(mData, s0, s1, inDuration, message.options, mRate);
                    if (success)
                        mAccumulatedSeekDuration = 0;
                    else {
                        mAccumulatedSeekDuration += inDuration;
                        return;
                    }
                    mData = newData;
                };

                mStarted = true;

                Data &entry = mData;
                if (  mStopped.load( std::memory_order_relaxed ) ) {
                    // We got the shut-down signal, or we discarded all the work.
                    // Output the -1 values.
                }
                else if (entry.mDuration > 0) {
                    // First use of the entry
                    startSample = entry.mS0;
                    endSample = entry.mS1;
                    duration = entry.mDuration;
                    entry.mDuration = 0;
                }
                else if (entry.mSilence > 0) {
                    // Second use of the entry
                    startSample = endSample = entry.mS1;
                    duration = entry.mSilence;
                    entry.mSilence = 0;
                }
            }

            void Stop()
            {
                mStopped.store( true, std::memory_order_relaxed );
            }

            #if 0
            // Needed only for the DRAG_SCRUB experiment
            // Should make mS1 atomic then?
            double LastTrackTime() const
            {
                // Needed by the main thread sometimes
                return mData.mS1.as_double() / mRate;
            }
            #endif

            ~ScrubState() {}

            private:
            struct Data
            {
                Data()
                    : mS0(0)
                    , mS1(0)
                    , mGoal(0)
                    , mDuration(0)
                    , mSilence(0)
                {}

                bool Init(Data &rPrevious, sampleCount s0, sampleCount s1,
                    sampleCount duration,
                    const ScrubbingOptions &options, double rate)
                {
                    auto previous = &rPrevious;
                    auto origDuration = duration;
                    mSilence = 0;

                    const bool &adjustStart = options.adjustStart;

                    wxASSERT(duration > 0);
                    double speed =
                        (std::abs((s1 - s0).as_long_long())) / duration.as_double();
                    bool adjustedSpeed = false;

                    auto minSpeed = std::min(options.minSpeed, options.maxSpeed);
                    wxASSERT(minSpeed == options.minSpeed);

                    // May change the requested speed and duration
                    if (!adjustStart && speed > options.maxSpeed)
                    {
                        // Reduce speed to the maximum selected in the user interface.
                        speed = options.maxSpeed;
                        mGoal = s1;
                        adjustedSpeed = true;
                    }
                    else if (!adjustStart &&
                        previous->mGoal >= 0 &&
                        previous->mGoal == s1)
                    {
                        // In case the mouse has not moved, and playback
                        // is catching up to the mouse at maximum speed,
                        // continue at no less than maximum.  (Without this
                        // the final catch-up can make a slow scrub interval
                        // that drops the pitch and sounds wrong.)
                        minSpeed = options.maxSpeed;
                        mGoal = s1;
                        adjustedSpeed = true;
                    }
                    else
                        mGoal = -1;

                    if (speed < minSpeed) {
                        if (s0 != s1 && adjustStart)
                        // Do not trim the duration.
                        ;
                        else
                        // Trim the duration.
                        duration =
                            std::max(0L, lrint(speed * duration.as_double() / minSpeed));

                        speed = minSpeed;
                        adjustedSpeed = true;
                    }

                    if (speed < ScrubbingOptions::MinAllowedScrubSpeed()) {
                        // Mixers were set up to go only so slowly, not slower.
                        // This will put a request for some silence in the work queue.
                        adjustedSpeed = true;
                        speed = 0.0;
                    }

                    // May change s1 or s0 to match speed change or stay in bounds of the project

                    if (adjustedSpeed && !adjustStart)
                    {
                        // adjust s1
                        const sampleCount diff = lrint(speed * duration.as_double());
                        if (s0 < s1)
                        s1 = s0 + diff;
                        else
                        s1 = s0 - diff;
                    }

                    bool silent = false;

                    // Adjust s1 (again), and duration, if s1 is out of bounds,
                    // or abandon if a stutter is too short.
                    // (Assume s0 is in bounds, because it equals the last scrub's s1 which was checked.)
                    if (s1 != s0)
                    {
                        // When playback follows a fast mouse movement by "stuttering"
                        // at maximum playback, don't make stutters too short to be useful.
                        if (options.adjustStart &&
                            duration < llrint( options.minStutterTime * rate ) )
                        return false;

                        sampleCount minSample { llrint(options.minTime * rate) };
                        sampleCount maxSample { llrint(options.maxTime * rate) };
                        auto newDuration = duration;
                        const auto newS1 = std::max(minSample, std::min(maxSample, s1));
                        if(s1 != newS1)
                        newDuration = std::max( sampleCount{ 0 },
                            sampleCount(
                                duration.as_double() * (newS1 - s0).as_double() /
                                    (s1 - s0).as_double()
                            )
                        );
                        if (newDuration == 0) {
                        // A silent scrub with s0 == s1
                        silent = true;
                        s1 = s0;
                        }
                        else if (s1 != newS1) {
                        // Shorten
                        duration = newDuration;
                        s1 = newS1;
                        }
                    }

                    if (adjustStart && !silent)
                    {
                        // Limit diff because this is seeking.
                        const sampleCount diff =
                        lrint(std::min(options.maxSpeed, speed) * duration.as_double());
                        if (s0 < s1)
                        s0 = s1 - diff;
                        else
                        s0 = s1 + diff;
                    }

                    mS0 = s0;
                    mS1 = s1;
                    mDuration = duration;
                    if (duration < origDuration)
                        mSilence = origDuration - duration;

                    return true;
                }

                sampleCount mS0;
                sampleCount mS1;
                sampleCount mGoal;
                sampleCount mDuration;
                sampleCount mSilence;
            };

            double mStartTime;
            bool mStarted{ false };
            std::atomic<bool> mStopped { false };
            Data mData;
            const double mRate;
            struct Message {
                Message() = default;
                Message(const Message&) = default;
                double end;
                ScrubbingOptions options;
            };
            MessageBuffer<Message> mMessage;
            sampleCount mAccumulatedSeekDuration{};
        };

        std::unique_ptr<ScrubState> mScrubState;

        bool mSilentScrub;
        double mScrubSpeed;
        sampleCount mScrubDuration;
    #endif

    protected:
        // A flag tested and set in one thread, cleared in another.  Perhaps
        // this guarantee of atomicity is more cautious than necessary.
        std::atomic<int> mRecordingException{};
        void SetRecordingException()
        {
            mRecordingException++;
        }

        void ClearRecordingException()
        {
            if (mRecordingException)
            {
                mRecordingException--;
            }
        }

        std::vector<std::pair<double, double>> mLostCaptureIntervals;
        bool mDetectDropouts{true};

    public:
        // Pairs of starting time and duration
        const std::vector<std::pair<double, double>> &LostCaptureIntervals()
        {
            return mLostCaptureIntervals;
        }

        // Used only for testing purposes in alpha builds
        bool mSimulateRecordingErrors{false};

        // Whether to check the error code passed to audacityAudioCallback to
        // detect more dropouts
        bool mDetectUpstreamDropouts{true};

    protected:
        RecordingSchedule mRecordingSchedule{};

        // Another circular buffer
        // Holds track time values corresponding to every nth sample in the playback
        // buffers, for some large n
        struct TimeQueue
        {
            Doubles mData;
            size_t mSize{0};
            double mLastTime{};
            // These need not be updated atomically, because we rely on the atomics
            // in the playback ring buffers to supply the synchronization.  Still,
            // align them to avoid false sharing.
            struct Cursor
            {
                size_t mIndex{};
                size_t mRemainder{};
            };
            NonInterfering<Cursor> mHead, mTail;

            void Producer(
                const PlaybackSchedule &schedule, double rate, double scrubSpeed,
                size_t nSamples);
            double Consumer(size_t nSamples, double rate);
        } mTimeQueue;

        PlaybackSchedule mPlaybackSchedule;
};
