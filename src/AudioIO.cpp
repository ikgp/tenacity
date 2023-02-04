/**********************************************************************

  Audacity: A Digital Audio Editor

  AudioIO.cpp

  Copyright 2000-2004:
  Dominic Mazzoni
  Joshua Haberman
  Markus Meyer
  Matt Brubeck

  This program is free software; you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

********************************************************************//**

\class AudioIO
\brief AudioIO uses the PortAudio library to play and record sound.

  Great care and attention to detail are necessary for understanding and
  modifying this system.  The code in this file is run from three
  different thread contexts: the UI thread, the disk thread (which
  this file creates and maintains; in the code, this is called the
  Audio Thread), and the PortAudio callback thread.
  To highlight this deliniation, the file is divided into three parts
  based on what thread context each function is intended to run in.

  \par EXPERIMENTAL_MIDI_OUT
  If EXPERIMENTAL_MIDI_OUT is defined, this class also manages
  MIDI playback. The reason for putting MIDI here rather than in, say,
  class MidiIO, is that there is no high-level synchronization and
  transport architecture, so Audio and MIDI must be coupled in order
  to start/stop/pause and synchronize them.

  \par MIDI With Audio
  When Audio and MIDI play simultaneously, MIDI synchronizes to Audio.
  This is necessary because the Audio sample clock is not the same
  hardware as the system time used to schedule MIDI messages. MIDI
  is synchronized to Audio because it is simple to pause or rush
  the dispatch of MIDI messages, but generally impossible to pause
  or rush synchronous audio samples (without distortion).

  \par
  MIDI output is driven by yet another thread. In principle, we could
  output timestamped MIDI data at the same time we fill audio buffers
  from disk, but audio buffers are filled far in advance of playback
  time, and there is a lower latency thread (PortAudio's callback) that
  actually sends samples to the output device. The relatively low
  latency to the output device allows Audacity to stop audio output
  quickly. We want the same behavior for MIDI, but there is not
  periodic callback from PortMidi (because MIDI is asynchronous), so
  this function is performed by the MidiThread class.

  \par
  When Audio is running, MIDI is synchronized to Audio. Globals are set
  in the Audio callback (audacityAudioCallback) for use by a time
  function that reports milliseconds to PortMidi. (Details below.)

  \par MIDI Without Audio
  When Audio is not running, PortMidi uses its own millisecond timer
  since there is no audio to synchronize to. (Details below.)

  \par Implementation Notes and Details for MIDI
  When opening devices, successAudio and successMidi indicate errors
  if false, so normally both are true. Use playbackChannels,
  captureChannels and mMidiPlaybackTracks.empty() to determine if
  Audio or MIDI is actually in use.

  \par Audio Time
  Normally, the current time during playback is given by the variable
  mTime. mTime normally advances by frames / samplerate each time an
  audio buffer is output by the audio callback. However, Audacity has
  a speed control that can perform continuously variable time stretching
  on audio. This is achieved in two places: the playback "mixer" that
  generates the samples for output processes the audio according to
  the speed control. In a separate algorithm, the audio callback updates
  mTime by (frames / samplerate) * factor, where factor reflects the
  speed at mTime. This effectively integrates speed to get position.
  Negative speeds are allowed too, for instance in scrubbing.

  \par The Big Picture
@verbatim

Sample
Time (in seconds, = total_sample_count / sample_rate)
  ^
  |                                             /         /
  |             y=x-mSystemTimeMinusAudioTime /         /
  |                                         /     #   /
  |                                       /         /
  |                                     /   # <- callbacks (#) showing
  |                                   /#        /   lots of timing jitter.
  |       top line is "full buffer" /         /     Some are later,
  |                     condition /         /       indicating buffer is
  |                             /         /         getting low. Plot
  |                           /     #   /           shows sample time
  |                         /    #    /             (based on how many
  |                       /    #    /               samples previously
  |                     /         /                 *written*) vs. real
  |                   / #       /                   time.
  |                 /<------->/ audio latency
  |               /#       v/
  |             /         / bottom line is "empty buffer"
  |           /   #     /      condition = DAC output time =
  |         /         /
  |       /      # <-- rapid callbacks as buffer is filled
  |     /         /
0 +...+---------#---------------------------------------------------->
  0 ^ |         |                                            real time
    | |         first callback time
    | mSystemMinusAudioTime
    |
    Probably the actual real times shown in this graph are very large
    in practice (> 350,000 sec.), so the X "origin" might be when
    the computer was booted or 1970 or something.


@endverbatim

  To estimate the true DAC time (needed to synchronize MIDI), we need
  a mapping from track time to DAC time. The estimate is the theoretical
  time of the full buffer (top diagonal line) + audio latency. To
  estimate the top diagonal line, we "draw" the line to be at least
  as high as any sample time corresponding to a callback (#), and we
  slowly lower the line in case the sample clock is slow or the system
  clock is fast, preventing the estimated line from drifting too far
  from the actual callback observations. The line is occasionally
  "bumped" up by new callback observations, but continuously
  "lowered" at a very low rate.  All adjustment is accomplished
  by changing mSystemMinusAudioTime, shown here as the X-intercept.\n
    theoreticalFullBufferTime = realTime - mSystemMinusAudioTime\n
  To estimate audio latency, notice that the first callback happens on
  an empty buffer, but the buffer soon fills up. This will cause a rapid
  re-estimation of mSystemMinusAudioTime. (The first estimate of
  mSystemMinusAudioTime will simply be the real time of the first
  callback time.) By watching these changes, which happen within ms of
  starting, we can estimate the buffer size and thus audio latency.
  So, to map from track time to real time, we compute:\n
    DACoutputTime = trackTime + mSystemMinusAudioTime\n
  There are some additional details to avoid counting samples while
  paused or while waiting for initialization, MIDI latency, etc.
  Also, in the code, track time is measured with respect to the track
  origin, so there's an extra term to add (mT0) if you start somewhere
  in the middle of the track.
  Finally, when a callback occurs, you might expect there is room in
  the output buffer for the requested frames, so maybe the "full buffer"
  sample time should be based not on the first sample of the callback, but
  the last sample time + 1 sample. I suspect, at least on Linux, that the
  callback occurs as soon as the last callback completes, so the buffer is
  really full, and the callback thread is going to block waiting for space
  in the output buffer.

  \par Midi Time
  MIDI is not warped according to the speed control. This might be
  something that should be changed. (Editorial note: Wouldn't it
  make more sense to display audio at the correct time and allow
  users to stretch audio the way they can stretch MIDI?) For now,
  MIDI plays at 1 second per second, so it requires an unwarped clock.
  In fact, MIDI time synchronization requires a millisecond clock that
  does not pause. Note that mTime will stop progress when the Pause
  button is pressed, even though audio samples (zeros) continue to
  be output.

  \par
  Therefore, we define the following interface for MIDI timing:
  \li \c AudioTime() is the time based on all samples written so far, including zeros output during pauses. AudioTime() is based on the start location mT0, not zero.
  \li \c PauseTime() is the amount of time spent paused, based on a count of zero-padding samples output.
  \li \c MidiTime() is an estimate in milliseconds of the current audio output time + 1s. In other words, what audacity track time corresponds to the audio (plus pause insertions) at the DAC output?

  \par AudioTime() and PauseTime() computation
  AudioTime() is simply mT0 + mNumFrames / mRate.
  mNumFrames is incremented in each audio callback. Similarly, PauseTime()
  is mNumPauseFrames / mRate. mNumPauseFrames is also incremented in
  each audio callback when a pause is in effect or audio output is ready to start.

  \par MidiTime() computation
  MidiTime() is computed based on information from PortAudio's callback,
  which estimates the system time at which the current audio buffer will
  be output. Consider the (unimplemented) function RealToTrack() that
  maps real audio write time to track time. If writeTime is the system
  time for the first sample of the current output buffer, and
  if we are in the callback, so AudioTime() also refers to the first sample
  of the buffer, then \n
  RealToTrack(writeTime) = AudioTime() - PauseTime()\n
  We want to know RealToTrack of the current time (when we are not in the
  callback, so we use this approximation for small d: \n
  RealToTrack(t + d) = RealToTrack(t) + d, or \n
  Letting t = writeTime and d = (systemTime - writeTime), we can
  substitute to get:\n
  RealToTrack(systemTime)
     = RealToTrack(writeTime) + systemTime - writeTime\n
     = AudioTime() - PauseTime() + (systemTime - writeTime) \n
  MidiTime() should include pause time, so that it increases smoothly,
  and audioLatency so that MidiTime() corresponds to the time of audio
  output rather than audio write times.  Also MidiTime() is offset by 1
  second to avoid negative time at startup, so add 1: \n
  MidiTime(systemTime) in seconds\n
     = RealToTrack(systemTime) + PauseTime() - audioLatency + 1 \n
     = AudioTime() + (systemTime - writeTime) - audioLatency + 1 \n
  (Note that audioLatency is called mAudioOutLatency in the code.)
  When we schedule a MIDI event with track time TT, we need
  to map TT to a PortMidi timestamp. The PortMidi timestamp is exactly
  MidiTime(systemTime) in ms units, and \n
     MidiTime(x) = RealToTrack(x) + PauseTime() + 1, so \n
     timestamp = TT + PauseTime() + 1 - midiLatency \n
  Note 1: The timestamp is incremented by the PortMidi stream latency
  (midiLatency) so we subtract midiLatency here for the timestamp
  passed to PortMidi. \n
  Note 2: Here, we're setting x to the time at which RealToTrack(x) = TT,
  so then MidiTime(x) is the desired timestamp. To be completely
  correct, we should assume that MidiTime(x + d) = MidiTime(x) + d,
  and consider that we compute MidiTime(systemTime) based on the
  *current* system time, but we really want the MidiTime(x) for some
  future time corresponding when MidiTime(x) = TT.)

  \par
  Also, we should assume PortMidi was opened with mMidiLatency, and that
  MIDI messages become sound with a delay of mSynthLatency. Therefore,
  the final timestamp calculation is: \n
     timestamp = TT + PauseTime() + 1 - (mMidiLatency + mSynthLatency) \n
  (All units here are seconds; some conversion is needed in the code.)

  \par
  The difference AudioTime() - PauseTime() is the time "cursor" for
  MIDI. When the speed control is used, MIDI and Audio will become
  unsynchronized. In particular, MIDI will not be synchronized with
  the visual cursor, which moves with scaled time reported in mTime.

  \par Timing in Linux
  It seems we cannot get much info from Linux. We can read the time
  when we get a callback, and we get a variable frame count (it changes
  from one callback to the next). Returning to the RealToTrack()
  equations above: \n
  RealToTrack(outputTime) = AudioTime() - PauseTime() - bufferDuration \n
  where outputTime should be PortAudio's estimate for the most recent output
  buffer, but at least on my Dell Latitude E7450, PortAudio is getting zero
  from ALSA, so we need to find a proxy for this.

  \par Estimating outputTime (Plan A, assuming double-buffered, fixed-size buffers, please skip to Plan B)
  One can expect the audio callback to happen as soon as there is room in
  the output for another block of samples, so we could just measure system
  time at the top of the callback. Then we could add the maximum delay
  buffered in the system. E.g. if there is simple double buffering and the
  callback is computing one of the buffers, the callback happens just as
  one of the buffers empties, meaning the other buffer is full, so we have
  exactly one buffer delay before the next computed sample is output.

  If computation falls behind a bit, the callback will be later, so the
  delay to play the next computed sample will be less. I think a reasonable
  way to estimate the actual output time is to assume that the computer is
  mostly keeping up and that *most* callbacks will occur immediately when
  there is space. Note that the most likely reason for the high-priority
  audio thread to fall behind is the callback itself, but the start of the
  callback should be pretty consistently keeping up.

  Also, we do not have to have a perfect estimate of the time. Suppose we
  estimate a linear mapping from sample count to system time by saying
  that the sample count maps to the system time at the most recent callback,
  and set the slope to 1% slower than real time (as if the sample clock is
  slow). Now, at each callback, if the callback seems to occur earlier than
  expected, we can adjust the mapping to be earlier. The earlier the
  callback, the more accurate it must be. On the other hand, if the callback
  is later than predicted, it must be a delayed callback (or else the
  sample clock is more than 1% slow, which is really a hardware problem.)
  How bad can this be? Assuming callbacks every 30ms (this seems to be what
  I'm observing in a default setup), you'll be a maximum of 1ms off even if
  2 out of 3 callbacks are late. This is pretty reasonable given that
  PortMIDI clock precision is 1ms. If buffers are larger and callback timing
  is more erratic, errors will be larger, but even a few ms error is
  probably OK.

  \par Estimating outputTime (Plan B, variable framesPerBuffer in callback, please skip to Plan C)
  ALSA is complicated because we get varying values of
  framesPerBuffer from callback to callback. Assume you get more frames
  when the callback is later (because there is more accumulated input to
  deliver and more more accumulated room in the output buffers). So take
  the current time and subtract the duration of the frame count in the
  current callback. This should be a time position that is relatively
  jitter free (because we estimated the lateness by frame count and
  subtracted that out). This time position intuitively represents the
  current ADC time, or if no input, the time of the tail of the output
  buffer. If we wanted DAC time, we'd have to add the total output
  buffer duration, which should be reported by PortAudio. (If PortAudio
  is wrong, we'll be systematically shifted in time by the error.)

  Since there is still bound to be jitter, we can smooth these estimates.
  First, we will assume a linear mapping from system time to audio time
  with slope = 1, so really it's just the offset we need, which is going
  to be a double that we can read/write atomically without locks or
  anything fancy. (Maybe it should be "volatile".)

  To improve the estimate, we get a new offset every callback, so we can
  create a "smooth" offset by using a simple regression model (also
  this could be seen as a first order filter). The following formula
  updates smooth_offset with a new offset estimate in the callback:
      smooth_offset = smooth_offset * 0.9 + new_offset_estimate * 0.1
  Since this is smooth, we'll have to be careful to give it a good initial
  value to avoid a long convergence.

  \par Estimating outputTime (Plan C)
  ALSA is complicated because we get varying values of
  framesPerBuffer from callback to callback. It seems there is a lot
  of variation in callback times and buffer space. One solution would
  be to go to fixed size double buffer, but Audacity seems to work
  better as is, so Plan C is to rely on one invariant which is that
  the output buffer cannot overflow, so there's a limit to how far
  ahead of the DAC time we can be writing samples into the
  buffer. Therefore, we'll assume that the audio clock runs slow by
  about 0.2% and we'll assume we're computing at that rate. If the
  actual output position is ever ahead of the computed position, we'll
  increase the computed position to the actual position. Thus whenever
  the buffer is less than near full, we'll stay ahead of DAC time,
  falling back at a rate of about 0.2% until eventually there's
  another near-full buffer callback that will push the time back ahead.

  \par Interaction between MIDI, Audio, and Pause
  When Pause is used, PauseTime() will increase at the same rate as
  AudioTime(), and no more events will be output. Because of the
  time advance of mAudioOutputLatency + MIDI_SLEEP + latency and the
  fact that
  AudioTime() advances stepwise by mAudioBufferDuration, some extra MIDI
  might be output, but the same is true of audio: something like
  mAudioOutputLatency audio samples will be in the output buffer
  (with up to mAudioBufferDuration additional samples, depending on
  when the Pause takes effect). When playback is resumed, there will
  be a slight delay corresponding to the extra data previously sent.
  Again, the same is true of audio. Audio and MIDI will not pause and
  resume at exactly the same times, but their pause and resume times
  will be within the low tens of milliseconds, and the streams will
  be synchronized in any case. I.e. if audio pauses 10ms earlier than
  MIDI, it will resume 10ms earlier as well.

  \par PortMidi Latency Parameter
  PortMidi has a "latency" parameter that is added to all timestamps.
  This value must be greater than zero to enable timestamp-based timing,
  but serves no other function, so we will set it to 1. All timestamps
  must then be adjusted down by 1 before messages are sent. This
  adjustment is on top of all the calculations described above. It just
  seem too complicated to describe everything in complete detail in one
  place.

  \par Midi with a time track
  When a variable-speed time track is present, MIDI events are output
  with the times used by the time track (rather than the raw times).
  This ensures MIDI is synchronized with audio.

  \par Midi While Recording Only or Without Audio Playback
  To reduce duplicate code and to ensure recording is synchronised with
  MIDI, a portaudio stream will always be used, even when there is no
  actual audio output.  For recording, this ensures that the recorded
  audio will by synchronized with the MIDI (otherwise, it gets out-of-
  sync if played back with correct timing).

  \par NoteTrack PlayLooped
  When mPlayLooped is true, output is supposed to loop from mT0 to mT1.
  For NoteTracks, we interpret this to mean that any note-on or control
  change in the range mT0 <= t < mT1 is sent (notes that start before
  mT0 are not played even if they extend beyond mT0). Then, all notes
  are turned off. Events in the range mT0 <= t < mT1 are then repeated,
  offset by (mT1 - mT0), etc.  We do NOT go back to the beginning and
  play all control changes (update events) up to mT0, nor do we "undo"
  any state changes between mT0 and mT1.

  \par NoteTrack PlayLooped Implementation
  The mIterator object (an Alg_iterator) returns NULL when there are
  no more events scheduled before mT1. At mT1, we want to output
  all notes off messages, but the FillMidiBuffers() loop will exit
  if mNextEvent is NULL, so we create a "fake" mNextEvent for this
  special "event" of sending all notes off. After that, we destroy
  the iterator and use PrepareMidiIterator() to set up a NEW one.
  At each iteration, time must advance by (mT1 - mT0), so the
  accumulated complete loop time (in "unwarped," track time) is computed
  by MidiLoopOffset().

  \todo run through all functions called from audio and portaudio threads
  to verify they are thread-safe. Note that synchronization of the style:
  "A sets flag to signal B, B clears flag to acknowledge completion"
  is not thread safe in a general multiple-CPU context. For example,
  B can write to a buffer and set a completion flag. The flag write can
  occur before the buffer write due to out-of-order execution. Then A
  can see the flag and read the buffer before buffer writes complete.

*//****************************************************************//**

\class AudioIOListener
\brief Monitors record play start/stop and new sample blocks.  Has
callbacks for these events.

*//****************************************************************//**

\class AudioIOStartStreamOptions
\brief struct holding stream options, including a pointer to the 
time warp info and AudioIOListener and whether the playback is looped.

*//*******************************************************************/


#include "AudioIO.h"

#include "AudioIOListener.h"
#include "AudioIoCallback.h"

#include "DeviceManager.h"

#include <string>
#include <cfloat>
#include <cmath>
#include <cstdlib>
#include <stdexcept>
#include <thread>
#include <optional>

#include "portaudio.h"

#include <wx/app.h>
#include <wx/log.h>

#if defined(__WXMAC__) || defined(__WXMSW__)
#include <wx/power.h>
#endif

// Tenacity libraries
#include <lib-basic-ui/BasicUI.h>
#include <lib-exceptions/TenacityException.h>
#include <lib-math/float_cast.h>
#include <lib-math/Resample.h>
#include <lib-preferences/Prefs.h>
#include <lib-utility/MessageBuffer.h>

#include "Meter.h"
#include "Mix.h"
#include "RingBuffer.h"
#include "Decibels.h"
#include "Prefs.h"
#include "Project.h"
#include "DBConnection.h"
#include "ProjectFileIO.h"
#include "ProjectWindows.h"
#include "WaveTrack.h"

#include "effects/RealtimeEffectManager.h"
#include "QualitySettings.h"
#include "widgets/AudacityMessageBox.h"

#ifdef EXPERIMENTAL_MIDI_OUT

#include "../lib-src/portmidi/pm_common/portmidi.h"
#include "../lib-src/portmidi/porttime/porttime.h"
#include "../lib-src/header-substitutes/allegro.h"

   constexpr int MIDI_SLEEP = 10; // milliseconds
   // how long do we think the thread that fills MIDI buffers,
   // if it is separate from the portaudio thread,
   // might be delayed due to other threads?
   #ifdef USE_MIDI_THREAD
      constexpr int THREAD_LATENCY = 10; // milliseconds
   #else
      constexpr int THREAD_LATENCY = 0; // milliseconds
   #endif
   template <typename FloatType>
   constexpr int ROUND(FloatType x)
   {
      return static_cast<int>(x + 0.5);
   }
   //#include <cstring>
//   #include "../lib-src/portmidi/pm_common/portmidi.h"
   #include "../lib-src/portaudio-v19/src/common/pa_util.h"
   #include "NoteTrack.h"
#endif

/*
 Adapt and rename the implementation of PaUtil_GetTime from commit
 c5d2c51bd6fe354d0ee1119ba932bfebd3ebfacc of portaudio
 */
#if defined( __APPLE__ )

#include <mach/mach_time.h>

/* Scaler to convert the result of mach_absolute_time to seconds */
static double machSecondsConversionScaler_ = 0.0;

/* Initialize it */
static struct InitializeTime { InitializeTime() {
   mach_timebase_info_data_t info;
   kern_return_t err = mach_timebase_info( &info );
   if( err == 0  )
       machSecondsConversionScaler_ = 1e-9 * (double) info.numer / (double) info.denom;
} } initializeTime;

static PaTime util_GetTime( void )
{
   return mach_absolute_time() * machSecondsConversionScaler_;
}

#elif defined( __WXMSW__ )

#include "profileapi.h"
#include "sysinfoapi.h"
#include "timeapi.h"

static int usePerformanceCounter_;
static double secondsPerTick_;

static struct InitializeTime { InitializeTime() {
    LARGE_INTEGER ticksPerSecond;

    if( QueryPerformanceFrequency( &ticksPerSecond ) != 0 )
    {
        usePerformanceCounter_ = 1;
        secondsPerTick_ = 1.0 / (double)ticksPerSecond.QuadPart;
    }
    else
    {
        usePerformanceCounter_ = 0;
    }
} } initializeTime;

static double util_GetTime( void )
{
    LARGE_INTEGER time;

    if( usePerformanceCounter_ )
    {
        /*
            Note: QueryPerformanceCounter has a known issue where it can skip forward
            by a few seconds (!) due to a hardware bug on some PCI-ISA bridge hardware.
            This is documented here:
            http://support.microsoft.com/default.aspx?scid=KB;EN-US;Q274323&
            The work-arounds are not very paletable and involve querying GetTickCount
            at every time step.
            Using rdtsc is not a good option on multi-core systems.
            For now we just use QueryPerformanceCounter(). It's good, most of the time.
        */
        QueryPerformanceCounter( &time );
        return time.QuadPart * secondsPerTick_;
    }
    else
    {
	#if defined(WINAPI_FAMILY) && (WINAPI_FAMILY == WINAPI_FAMILY_APP)
        return GetTickCount64() * .001;
	#else
        return timeGetTime() * .001;
	#endif
    }
}

#elif defined(HAVE_CLOCK_GETTIME)

#include <time.h>

static PaTime util_GetTime( void )
{
    struct timespec tp;
    clock_gettime(CLOCK_REALTIME, &tp);
    return (PaTime)(tp.tv_sec + tp.tv_nsec * 1e-9);
}

#else

#include <sys/time.h>

static PaTime util_GetTime( void )
{
    struct timeval tv;
    gettimeofday( &tv, NULL );
    return (PaTime) tv.tv_usec * 1e-6 + tv.tv_sec;
}

#endif

using std::max;
using std::min;

AudioIO *AudioIO::Get()
{
   return static_cast< AudioIO* >( AudioIOBase::Get() );
}

wxDEFINE_EVENT(EVT_AUDIOIO_PLAYBACK, wxCommandEvent);
wxDEFINE_EVENT(EVT_AUDIOIO_CAPTURE, wxCommandEvent);
wxDEFINE_EVENT(EVT_AUDIOIO_MONITOR, wxCommandEvent);

enum {
   // This is the least positive latency we can
   // specify to Pm_OpenOutput, 1 ms, which prevents immediate
   // scheduling of events:
   MIDI_MINIMAL_LATENCY_MS = 1
};

constexpr size_t TimeQueueGrainSize = 2000;

#ifdef EXPERIMENTAL_SCRUBBING_SUPPORT

#ifdef __WXGTK__
   // Might #define this for a useful thing on Linux
   #undef REALTIME_ALSA_THREAD
#else
   // never on the other operating systems
   #undef REALTIME_ALSA_THREAD
#endif

#ifdef REALTIME_ALSA_THREAD
#include "pa_linux_alsa.h"
#endif

#endif

#ifdef EXPERIMENTAL_MIDI_OUT
// return the system time as a double
static double streamStartTime = 0; // bias system time to small number

static double SystemTime(bool usingAlsa)
{
#ifdef __WXGTK__
   if (usingAlsa) {
      struct timespec now;
      // CLOCK_MONOTONIC_RAW is unaffected by NTP or adj-time
      clock_gettime(CLOCK_MONOTONIC_RAW, &now);
      //return now.tv_sec + now.tv_nsec * 0.000000001;
      return (now.tv_sec + now.tv_nsec * 0.000000001) - streamStartTime;
   }
#else
   static_cast<void>(usingAlsa);//compiler food.
#endif

   return util_GetTime() - streamStartTime;
}
#endif

int audacityAudioCallback(const void *inputBuffer, void *outputBuffer,
                          unsigned long framesPerBuffer,
                          const PaStreamCallbackTimeInfo *timeInfo,
                          PaStreamCallbackFlags statusFlags, void *userData );

void StartAudioIOThread()
{
   AudioIO *gAudioIO;
   while( (gAudioIO = AudioIO::Get()) != nullptr )
   {
      using Clock = std::chrono::steady_clock;
      auto loopPassStart = Clock::now();
      const auto interval = ScrubPollInterval_ms;

      // Set LoopActive outside the tests to avoid race condition
      gAudioIO->mAudioThreadFillBuffersLoopActive = true;
      if( gAudioIO->mAudioThreadShouldCallFillBuffersOnce )
      {
         gAudioIO->FillBuffers();
         gAudioIO->mAudioThreadShouldCallFillBuffersOnce = false;
      }
      else if( gAudioIO->mAudioThreadFillBuffersLoopRunning )
      {
         gAudioIO->FillBuffers();
      }
      gAudioIO->mAudioThreadFillBuffersLoopActive = false;

      if ( gAudioIO->mPlaybackSchedule.Interactive() )
      {
         std::this_thread::sleep_until(
            loopPassStart + std::chrono::milliseconds( interval ) );
      } else
      {
         std::this_thread::sleep_for(std::chrono::milliseconds(10));
      }
   }
}

#ifdef EXPERIMENTAL_MIDI_OUT
void StartMidiIOThread()
{
   AudioIO *gAudioIO;
   while ( (gAudioIO = AudioIO::Get()) != nullptr )
   {
      // Set LoopActive outside the tests to avoid race condition
      gAudioIO->mMidiThreadFillBuffersLoopActive = true;
      if( gAudioIO->mMidiThreadFillBuffersLoopRunning &&
          // mNumFrames signals at least one callback, needed for MidiTime()
          gAudioIO->mNumFrames > 0)
      {
         gAudioIO->FillMidiBuffers();
      }

      gAudioIO->mMidiThreadFillBuffersLoopActive = false;
      std::this_thread::sleep_for(std::chrono::milliseconds(MIDI_SLEEP));
   }
}
#endif

//////////////////////////////////////////////////////////////////////
//
//     UI Thread Context
//
//////////////////////////////////////////////////////////////////////

void AudioIO::Init()
{
   ugAudioIO.reset(safenew AudioIO());

   // Start the audio (and MIDI) IO threads
   std::thread audioThread(StartAudioIOThread);
   audioThread.detach();

   #if defined(EXPERIMENTAL_MIDI_OUT) && defined(USE_MIDI_THREAD)
   std::thread midiThread(StartMidiIOThread);
   midiThread.detach();
   #endif

   // Make sure device prefs are initialized
   if (gPrefs->Read(wxT("AudioIO/RecordingDevice"), wxT("")).empty()) {
      int i = getRecordDevIndex();
      const PaDeviceInfo *info = Pa_GetDeviceInfo(i);
      if (info) {
         AudioIORecordingDevice.Write(DeviceName(info));
         AudioIOHost.Write(HostName(info));
      }
   }

   if (gPrefs->Read(wxT("AudioIO/PlaybackDevice"), wxT("")).empty()) {
      int i = getPlayDevIndex();
      const PaDeviceInfo *info = Pa_GetDeviceInfo(i);
      if (info) {
         AudioIOPlaybackDevice.Write(DeviceName(info));
         AudioIOHost.Write(HostName(info));
      }
   }

   gPrefs->Flush();
}

void AudioIO::Deinit()
{
   ugAudioIO.reset();
}

bool AudioIO::ValidateDeviceNames(const wxString &play, const wxString &rec)
{
   const PaDeviceInfo *pInfo = Pa_GetDeviceInfo(getPlayDevIndex(play));
   const PaDeviceInfo *rInfo = Pa_GetDeviceInfo(getRecordDevIndex(rec));

   // Valid iff both defined and the same api.
   return pInfo != nullptr && rInfo != nullptr && pInfo->hostApi == rInfo->hostApi;
}

AudioIO::AudioIO()
{
   if (!std::atomic<double>{}.is_lock_free()) {
      // If this check fails, then the atomic<double> members in AudioIO.h
      // might be changed to atomic<float> to be more efficient with some
      // loss of precision.  That could be conditionally compiled depending
      // on the platform.
      throw std::runtime_error("atomic<double> could be changed to atomic<float>, reducing precision");
   }

   // This exception is thrown because of casting in the callback 
   // functions where we cast a tempFloats buffer to a (short*) buffer.
   // We assert this at compile time, although we previously asserted in the
   // GUI (afterwards throwing exceptions).
   static_assert(sizeof(short) <= sizeof(float), "sizeof(short) is not less than sizeof(float)");

   mAudioThreadShouldCallFillBuffersOnce = false;
   mAudioThreadFillBuffersLoopRunning = false;
   mAudioThreadFillBuffersLoopActive = false;
   mPortStreamV19 = NULL;

#ifdef EXPERIMENTAL_MIDI_OUT
   mMidiStream = NULL;
   mMidiThreadFillBuffersLoopRunning = false;
   mMidiThreadFillBuffersLoopActive = false;
   mMidiStreamActive = false;
   mSendMidiState = false;
   mIterator = NULL;

   mNumFrames = 0;
   mNumPauseFrames = 0;
#endif

   mStreamToken = 0;

   mLastPaError = paNoError;

   mLastRecordingOffset = 0.0;
   mNumCaptureChannels = 0;
   mPaused = false;
   mSilenceLevel = 0.0;

   mUpdateMeters = false;
   mUpdatingMeters = false;

   mOwningProject = NULL;
   mOutputMeter.reset();

   PaError err = Pa_Initialize();

   if (err != paNoError) {
      auto errStr = XO("Could not find any audio devices.\n");
      errStr += XO("You will not be able to play or record audio.\n\n");
      wxString paErrStr = LAT1CTOWX(Pa_GetErrorText(err));
      if (!paErrStr.empty())
      {
         errStr += XO("Error: %s").Format( paErrStr );
      }

      throw SimpleMessageBoxException(ExceptionType::Internal, errStr, XO("PortAudio Error"));

      // Since PortAudio is not initialized, all calls to PortAudio
      // functions will fail.  This will give reasonable behavior, since
      // the user will be able to do things not relating to audio i/o,
      // but any attempt to play or record will simply fail.
   }

#ifdef EXPERIMENTAL_MIDI_OUT
   PmError pmErr = Pm_Initialize();

   if (pmErr != pmNoError) {
      auto errStr =
              XO("There was an error initializing the midi i/o layer.\n");
      errStr += XO("You will not be able to play midi.\n\n");
      wxString pmErrStr = LAT1CTOWX(Pm_GetErrorText(pmErr));
      if (!pmErrStr.empty())
      {
         errStr += XO("Error: %s").Format( pmErrStr );
      }

      throw SimpleMessageBoxException(ExceptionType::Internal, errStr, XO("PortMidi Error"));

      // Same logic for PortMidi as described above for PortAudio
   }
#endif

   mLastPlaybackTimeMillis = 0;

#ifdef EXPERIMENTAL_SCRUBBING_SUPPORT
   mScrubState = NULL;
   mScrubDuration = 0;
   mSilentScrub = false;
#endif
}

AudioIO::~AudioIO()
{

   // FIXME: ? TRAP_ERR.  Pa_Terminate probably OK if err without reporting.
   Pa_Terminate();

#ifdef EXPERIMENTAL_MIDI_OUT
   Pm_Terminate();

   /* Delete is a "graceful" way to stop the thread.
   (Kill is the not-graceful way.) */

/*#ifdef USE_MIDI_THREAD
   //mMidiThread->join();
   delete mMidiThread;
#endif*/

#endif

   /* Delete is a "graceful" way to stop the thread.
      (Kill is the not-graceful way.) */

   // This causes reentrancy issues during application shutdown
   // wxTheApp->Yield();

   //mThread->join();
   //delete mThread;*/
}

static PaSampleFormat AudacityToPortAudioSampleFormat(sampleFormat format)
{
   switch(format) {
   case int16Sample:
      return paInt16;
   case int24Sample:
      return paInt24;
   case floatSample:
   default:
      return paFloat32;
   }
}

bool AudioIO::StartPortAudioStream(const AudioIOStartStreamOptions &options,
                                   unsigned int numPlaybackChannels,
                                   unsigned int numCaptureChannels,
                                   sampleFormat captureFormat)
{
   auto sampleRate = options.rate;
#ifdef EXPERIMENTAL_MIDI_OUT
   mNumFrames = 0;
   mNumPauseFrames = 0;
   // we want this initial value to be way high. It should be
   // sufficient to assume AudioTime is zero and therefore
   // mSystemMinusAudioTime is SystemTime(), but we'll add 1000s
   // for good measure. On the first callback, this should be
   // reduced to SystemTime() - mT0, and note that mT0 is always
   // positive.
   mSystemMinusAudioTimePlusLatency =
      mSystemMinusAudioTime = SystemTime(mUsingAlsa) + 1000;
   mAudioOutLatency = 0.0; // set when stream is opened
   mCallbackCount = 0;
   mAudioFramesPerBuffer = 0;
#endif
   mOwningProject = options.pProject;

   // PRL:  Protection from crash reported by David Bailes, involving starting
   // and stopping with frequent changes of active window, hard to reproduce
   if (!mOwningProject)
      return false;

   mInputMeter.reset();
   mOutputMeter.reset();

   mLastPaError = paNoError;
   // pick a rate to do the audio I/O at, from those available. The project
   // rate is suggested, but we may get something else if it isn't supported
   mRate = GetBestRate(numCaptureChannels > 0, numPlaybackChannels > 0, sampleRate);

   // July 2016 (Carsten and Uwe)
   // BUG 193: Tell PortAudio sound card will handle 24 bit (under DirectSound) using 
   // userData.
   int captureFormat_saved = captureFormat;
   // Special case: Our 24-bit sample format is different from PortAudio's
   // 3-byte packed format. So just make PortAudio return float samples,
   // since we need float values anyway to apply the gain.
   // ANSWER-ME: So we *never* actually handle 24-bit?! This causes mCapture to 
   // be set to floatSample below.
   // JKC: YES that's right.  Internally Audacity uses float, and float has space for
   // 24 bits as well as exponent.  Actual 24 bit would require packing and
   // unpacking unaligned bytes and would be inefficient.
   // ANSWER ME: is floatSample 64 bit on 64 bit machines?
   if (captureFormat == int24Sample)
      captureFormat = floatSample;

   mNumPlaybackChannels = numPlaybackChannels;
   mNumCaptureChannels = numCaptureChannels;

   bool usePlayback = false, useCapture = false;
   PaStreamParameters playbackParameters{};
   PaStreamParameters captureParameters{};

   auto latencyDuration = AudioIOLatencyDuration.Read();

   if( numPlaybackChannels > 0)
   {
      usePlayback = true;

      // this sets the device index to whatever is "right" based on preferences,
      // then defaults
      playbackParameters.device = getPlayDevIndex();

      const PaDeviceInfo *playbackDeviceInfo;
      playbackDeviceInfo = Pa_GetDeviceInfo( playbackParameters.device );

      if( playbackDeviceInfo == NULL )
         return false;

      // regardless of source formats, we always mix to float
      playbackParameters.sampleFormat = paFloat32;
      playbackParameters.hostApiSpecificStreamInfo = NULL;
      playbackParameters.channelCount = mNumPlaybackChannels;

      if (mSoftwarePlaythrough)
         playbackParameters.suggestedLatency =
            playbackDeviceInfo->defaultLowOutputLatency;
      else {
         // When using WASAPI, the suggested latency does not affect
         // the latency of the playback, but the position of playback is given as if
         // there was the suggested latency. This results in the last "suggested latency"
         // of a selection not being played. So for WASAPI use 0.0 for the suggested
         // latency regardless of user setting. See bug 1949.
         const PaHostApiInfo* hostInfo = Pa_GetHostApiInfo(playbackDeviceInfo->hostApi);
         bool isWASAPI = (hostInfo && hostInfo->type == paWASAPI);
         playbackParameters.suggestedLatency = isWASAPI ? 0.0 : latencyDuration/1000.0;
      }

      mOutputMeter = options.playbackMeter;
   }

   if( numCaptureChannels > 0)
   {
      useCapture = true;
      mCaptureFormat = captureFormat;

      const PaDeviceInfo *captureDeviceInfo;
      // retrieve the index of the device set in the prefs, or a sensible
      // default if it isn't set/valid
      captureParameters.device = getRecordDevIndex();

      captureDeviceInfo = Pa_GetDeviceInfo( captureParameters.device );

      if( captureDeviceInfo == NULL )
         return false;

      captureParameters.sampleFormat =
         AudacityToPortAudioSampleFormat(mCaptureFormat);

      captureParameters.hostApiSpecificStreamInfo = NULL;
      captureParameters.channelCount = mNumCaptureChannels;

      if (mSoftwarePlaythrough)
         captureParameters.suggestedLatency =
            captureDeviceInfo->defaultHighInputLatency;
      else
         captureParameters.suggestedLatency = latencyDuration/1000.0;

      SetCaptureMeter( mOwningProject, options.captureMeter );
   }

   SetMeters();

   // July 2016 (Carsten and Uwe)
   // BUG 193: Possibly tell portAudio to use 24 bit with DirectSound. 
   int  userData = 24;
   int* lpUserData = (captureFormat_saved == int24Sample) ? &userData : NULL;

   // (Linux, bug 1885) After scanning devices it takes a little time for the
   // ALSA device to be available, so allow retries.
   // On my test machine, no more than 3 attempts are required.
   unsigned int maxTries = 1;
#ifdef __WXGTK__
   if (DeviceManager::Instance()->GetTimeSinceRescan() < 10)
      maxTries = 5;
#endif

   for (unsigned int tries = 0; tries < maxTries; tries++) {
      mLastPaError = Pa_OpenStream( &mPortStreamV19,
                                    useCapture ? &captureParameters : NULL,
                                    usePlayback ? &playbackParameters : NULL,
                                    mRate, paFramesPerBufferUnspecified,
                                    paNoFlag,
                                    audacityAudioCallback, lpUserData );
      if (mLastPaError == paNoError) {
         break;
      }
      wxLogDebug("Attempt %u to open capture stream failed with: %d", 1 + tries, mLastPaError);
      std::this_thread::sleep_for(std::chrono::milliseconds(1000));
   }

#ifdef EXPERIMENTAL_MIDI_OUT
   // We use audio latency to estimate how far ahead of DACS we are writing
   if (mPortStreamV19 != NULL && mLastPaError == paNoError) {
      const PaStreamInfo* info = Pa_GetStreamInfo(mPortStreamV19);
      // this is an initial guess, but for PA/Linux/ALSA it's wrong and will be
      // updated with a better value:
      mAudioOutLatency = info->outputLatency;
      mSystemMinusAudioTimePlusLatency += mAudioOutLatency;
   }
#endif

#if (defined(__WXMAC__) || defined(__WXMSW__)) && wxCHECK_VERSION(3,1,0)
   // Don't want the system to sleep while audio I/O is active
   if (mPortStreamV19 != NULL && mLastPaError == paNoError) {
      wxPowerResource::Acquire(wxPOWER_RESOURCE_SCREEN, _("Tenacity Audio"));
   }
#endif

   return (mLastPaError == paNoError);
}

std::string AudioIO::LastPaErrorString()
{
   return std::string(std::to_string(mLastPaError) + 
                      std::string(Pa_GetErrorText(mLastPaError))
   );
}

void AudioIO::StartMonitoring( const AudioIOStartStreamOptions &options )
{
   if ( mPortStreamV19 || mStreamToken )
      return;

   bool success;
   auto captureFormat = QualitySettings::SampleFormatChoice();
   auto captureChannels = AudioIORecordChannels.Read();
   gPrefs->Read(wxT("/AudioIO/SWPlaythrough"), &mSoftwarePlaythrough, false);
   int playbackChannels = 0;

   if (mSoftwarePlaythrough)
      playbackChannels = 2;

   // FIXME: TRAP_ERR StartPortAudioStream (a PaError may be present)
   // but StartPortAudioStream function only returns true or false.
   mUsingAlsa = false;
   success = StartPortAudioStream(options, (unsigned int)playbackChannels,
                                  (unsigned int)captureChannels,
                                  captureFormat);

   if (!success) {
      auto msg = XO("Error opening recording device.\nError code: %s")
         .Format( Get()->LastPaErrorString() );
      throw SimpleMessageBoxException(ExceptionType::Internal, msg, XO("Error"), wxT("Error_opening_sound_device"));
      return;
   }

   wxCommandEvent e(EVT_AUDIOIO_MONITOR);
   e.SetEventObject(mOwningProject);
   e.SetInt(true);
   wxTheApp->ProcessEvent(e);

   // FIXME: TRAP_ERR PaErrorCode 'noted' but not reported in StartMonitoring.
   // Now start the PortAudio stream!
   // TODO: ? Factor out and reuse error reporting code from end of 
   // AudioIO::StartStream?
   mLastPaError = Pa_StartStream( mPortStreamV19 );

   // Update UI display only now, after all possibilities for error are past.
   auto pListener = GetListener();
   if ((mLastPaError == paNoError) && pListener) {
      // advertise the chosen I/O sample rate to the UI
      pListener->OnAudioIORate((int)mRate);
   }
}

int AudioIO::StartStream(const TransportTracks &tracks,
                         double t0, double t1,
                         const AudioIOStartStreamOptions &options)
{
   mLostSamples = 0;
   mLostCaptureIntervals.clear();
   mDetectDropouts =
      gPrefs->Read( WarningDialogKey(wxT("DropoutDetected")), true ) != 0;
   auto cleanup = finally ( [this] { ClearRecordingException(); } );

   if( IsBusy() )
      return 0;

   // We just want to set mStreamToken to -1 - this way avoids
   // an extremely rare but possible race condition, if two functions
   // somehow called StartStream at the same time...
   mStreamToken--;
   if (mStreamToken != -1)
      return 0;

   // TODO: we don't really need to close and reopen stream if the
   // format matches; however it's kind of tricky to keep it open...
   //
   //   if (sampleRate == mRate &&
   //       playbackChannels == mNumPlaybackChannels &&
   //       captureChannels == mNumCaptureChannels &&
   //       captureFormat == mCaptureFormat) {

   if (mPortStreamV19) {
      StopStream();
      while(mPortStreamV19)
         std::this_thread::sleep_for(std::chrono::milliseconds(50));
   }

#ifdef __WXGTK__
   // Detect whether ALSA is the chosen host, and do the various involved MIDI
   // timing compensations only then.
   mUsingAlsa = (AudioIOHost.Read() == L"ALSA");
#endif

   gPrefs->Read(wxT("/AudioIO/SWPlaythrough"), &mSoftwarePlaythrough, false);
   gPrefs->Read(wxT("/AudioIO/SoundActivatedRecord"), &mPauseRec, false);
   gPrefs->Read(wxT("/AudioIO/Microfades"), &mbMicroFades, false);
   int silenceLevelDB;
   gPrefs->Read(wxT("/AudioIO/SilenceLevel"), &silenceLevelDB, -50);
   int dBRange = DecibelScaleCutoff.Read();
   if(silenceLevelDB < -dBRange)
   {
      silenceLevelDB = -dBRange + 3;
      // meter range was made smaller than SilenceLevel
      // so set SilenceLevel reasonable

      // PRL:  update prefs, or correct it only in-session?
      // The behavior (as of 2.3.1) was the latter, the code suggested that
      // the intent was the former;  I preserve the behavior, but uncomment
      // this if you disagree.
      // gPrefs->Write(wxT("/AudioIO/SilenceLevel"), silenceLevelDB);
      // gPrefs->Flush();
   }
   mSilenceLevel = DB_TO_LINEAR(silenceLevelDB);  // meter goes -dBRange dB -> 0dB

   // Clamp pre-roll so we don't play before time 0
   const auto preRoll = std::max(0.0, std::min(t0, options.preRoll));
   mRecordingSchedule = {};
   mRecordingSchedule.mPreRoll = preRoll;
   mRecordingSchedule.mLatencyCorrection =
      AudioIOLatencyCorrection.Read() / 1000.0;
   mRecordingSchedule.mDuration = t1 - t0;
   if (options.pCrossfadeData)
      mRecordingSchedule.mCrossfadeData.swap( *options.pCrossfadeData );

   mListener = options.listener;
   mRate    = options.rate;

   mSeek    = 0;
   mLastRecordingOffset = 0;
   mCaptureTracks = tracks.captureTracks;
   mPlaybackTracks = tracks.playbackTracks;
#ifdef EXPERIMENTAL_MIDI_OUT
   mMidiPlaybackTracks = tracks.midiTracks;
#endif

   bool commit = false;
   auto cleanupTracks = finally([&]{
      if (!commit) {
         // Don't keep unnecessary shared pointers to tracks
         mPlaybackTracks.clear();
         mCaptureTracks.clear();
#ifdef EXPERIMENTAL_MIDI_OUT
         mMidiPlaybackTracks.clear();
#endif

         // Don't cause a busy wait in the audio thread after stopping scrubbing
         mPlaybackSchedule.ResetMode();
      }
   });

   mPlaybackBuffers.reset();
   mPlaybackMixers.reset();
   mCaptureBuffers.reset();
   mResample.reset();
   mTimeQueue.mData.reset();

#ifdef EXPERIMENTAL_MIDI_OUT
   streamStartTime = 0;
   streamStartTime = SystemTime(mUsingAlsa);
#endif

   mPlaybackSchedule.Init(
      t0, t1, options, mCaptureTracks.empty() ? nullptr : &mRecordingSchedule );
   const bool scrubbing = mPlaybackSchedule.Interactive();

   unsigned int playbackChannels = 0;
   unsigned int captureChannels = 0;
   sampleFormat captureFormat = floatSample;

   auto pListener = GetListener();

   if (tracks.playbackTracks.size() > 0
#ifdef EXPERIMENTAL_MIDI_OUT
      || tracks.midiTracks.size() > 0
#endif
      )
      playbackChannels = 2;

   if (mSoftwarePlaythrough)
      playbackChannels = 2;

   if (tracks.captureTracks.size() > 0)
   {
      // For capture, every input channel gets its own track
      captureChannels = mCaptureTracks.size();
      // I don't deal with the possibility of the capture tracks
      // having different sample formats, since it will never happen
      // with the current code.  This code wouldn't *break* if this
      // assumption was false, but it would be sub-optimal.  For example,
      // if the first track was 16-bit and the second track was 24-bit,
      // we would set the sound card to capture in 16 bits and the second
      // track wouldn't get the benefit of all 24 bits the card is capable
      // of.
      captureFormat = mCaptureTracks[0]->GetSampleFormat();

      // Tell project that we are about to start recording
      if (pListener)
         pListener->OnAudioIOStartRecording();
   }

   bool successAudio;

   successAudio = StartPortAudioStream(options, playbackChannels,
                                       captureChannels, captureFormat);
#ifdef EXPERIMENTAL_MIDI_OUT

   // TODO: it may be that midi out will not work unless audio in or out is
   // active -- this would be a bug and may require a change in the
   // logic here.

   bool successMidi = true;

   if(!mMidiPlaybackTracks.empty()){
      successMidi = StartPortMidiStream();
   }

   // On the other hand, if MIDI cannot be opened, we will not complain
#endif

   if (!successAudio) {
      if (pListener && captureChannels > 0)
         pListener->OnAudioIOStopRecording();
      mStreamToken = 0;

      return 0;
   }

   if ( ! AllocateBuffers( options, tracks, t0, t1, options.rate, scrubbing ) )
      return 0;

   if (mNumPlaybackChannels > 0)
   {
      auto & em = RealtimeEffectManager::Get();
      // Setup for realtime playback at the rate of the realtime
      // stream, not the rate of the track.
      em.RealtimeInitialize(mRate);

      // The following adds a NEW effect processor for each logical track and the
      // group determination should mimic what is done in audacityAudioCallback()
      // when calling RealtimeProcess().
      int group = 0;
      for (size_t i = 0, cnt = mPlaybackTracks.size(); i < cnt;)
      {
         const WaveTrack *vt = mPlaybackTracks[i].get();

         // TODO: more-than-two-channels
         unsigned chanCnt = TrackList::Channels(vt).size();
         i += chanCnt;

         // Setup for realtime playback at the rate of the realtime
         // stream, not the rate of the track.
         em.RealtimeAddProcessor(group++, std::min(2u, chanCnt), mRate);
      }
   }

   if (options.pStartTime)
   {
      // Calculate the NEW time position
      const auto time = mPlaybackSchedule.ClampTrackTime( *options.pStartTime );

      // Main thread's initialization of mTime
      mPlaybackSchedule.SetTrackTime( time );

      // Reset mixer positions for all playback tracks
      unsigned numMixers = mPlaybackTracks.size();
      for (unsigned ii = 0; ii < numMixers; ++ii)
         mPlaybackMixers[ii]->Reposition( time );
      mPlaybackSchedule.RealTimeInit( time );
   }
   
   // Now that we are done with SetTrackTime():
   mTimeQueue.mLastTime = mPlaybackSchedule.GetTrackTime();
   if (mTimeQueue.mData)
      mTimeQueue.mData[0] = mTimeQueue.mLastTime;
   // else recording only without overdub

#ifdef EXPERIMENTAL_SCRUBBING_SUPPORT
   if (scrubbing)
   {
      const auto &scrubOptions = *options.pScrubbingOptions;
      mScrubState =
         std::make_unique<ScrubState>(
            mPlaybackSchedule.mT0,
            mRate,
            scrubOptions);
      mScrubDuration = 0;
      mSilentScrub = false;
   }
   else
      mScrubState.reset();
#endif

   // We signal the audio thread to call FillBuffers, to prime the RingBuffers
   // so that they will have data in them when the stream starts.  Having the
   // audio thread call FillBuffers here makes the code more predictable, since
   // FillBuffers will ALWAYS get called from the Audio thread.
   mAudioThreadShouldCallFillBuffersOnce = true;

   while( mAudioThreadShouldCallFillBuffersOnce ) {
      auto interval = 50ull;
      if (options.playbackStreamPrimer) {
         interval = options.playbackStreamPrimer();
      }

      std::this_thread::sleep_for(std::chrono::milliseconds(interval));
   }

   if(mNumPlaybackChannels > 0 || mNumCaptureChannels > 0) {

#ifdef REALTIME_ALSA_THREAD
      // PRL: Do this in hope of less thread scheduling jitter in calls to
      // audacityAudioCallback.
      // Not needed to make audio playback work smoothly.
      // But needed in case we also play MIDI, so that the variable "offset"
      // in AudioIO::MidiTime() is a better approximation of the duration
      // between the call of audacityAudioCallback and the actual output of
      // the first audio sample.
      // (Which we should be able to determine from fields of
      // PaStreamCallbackTimeInfo, but that seems not to work as documented with
      // ALSA.)
      if (mUsingAlsa)
         // Perhaps we should do this only if also playing MIDI ?
         PaAlsa_EnableRealtimeScheduling( mPortStreamV19, 1 );
#endif

      //
      // Generate a unique value each time, to be returned to
      // clients accessing the AudioIO API, so they can query if they
      // are the ones who have reserved AudioIO or not.
      //
      // It is important to set this before setting the portaudio stream in
      // motion -- otherwise it may play an unspecified number of leading
      // zeroes.
      mStreamToken = (++mNextStreamToken);

      // This affects the AudioThread (not the portaudio callback).
      // Probably not needed so urgently before portaudio thread start for usual
      // playback, since our ring buffers have been primed already with 4 sec
      // of audio, but then we might be scrubbing, so do it.
      mAudioThreadFillBuffersLoopRunning = true;

      // Now start the PortAudio stream!
      PaError err;
      err = Pa_StartStream( mPortStreamV19 );

      if( err != paNoError )
      {
         mStreamToken = 0;
         mAudioThreadFillBuffersLoopRunning = false;
         if (pListener && mNumCaptureChannels > 0)
            pListener->OnAudioIOStopRecording();
         StartStreamCleanup();
         // PRL: PortAudio error messages are sadly not internationalized
         throw SimpleMessageBoxException(ExceptionType::Internal, StringLiteral(Pa_GetErrorText(err)), StringLiteral("PortAudio Stream Error"));
         return 0;
      }
   }

   // Update UI display only now, after all possibilities for error are past.
   if (pListener) {
      // advertise the chosen I/O sample rate to the UI
      pListener->OnAudioIORate((int)mRate);
   }

   if (mNumPlaybackChannels > 0)
   {
      wxCommandEvent e(EVT_AUDIOIO_PLAYBACK);
      e.SetEventObject(mOwningProject);
      e.SetInt(true);
      wxTheApp->ProcessEvent(e);
   }

   if (mNumCaptureChannels > 0)
   {
      wxCommandEvent e(EVT_AUDIOIO_CAPTURE);
      e.SetEventObject(mOwningProject);
      e.SetInt(true);
      wxTheApp->ProcessEvent(e);
   }

   commit = true;
   return mStreamToken;
}

void AudioIO::DelayActions(bool recording)
{
   mDelayingActions = recording;
}

bool AudioIO::DelayingActions() const
{
   return mDelayingActions || (mPortStreamV19 && mNumCaptureChannels > 0);
}

void AudioIO::CallAfterRecording(PostRecordingAction action)
{
   if (!action)
      return;

   {
      std::lock_guard<std::mutex> guard{ mPostRecordingActionMutex };
      if (mPostRecordingAction) {
         // Enqueue it, even if perhaps not still recording,
         // but it wasn't cleared yet
         mPostRecordingAction = [
            prevAction = std::move(mPostRecordingAction),
            nextAction = std::move(action)
         ]{ prevAction(); nextAction(); };
         return;
      }
      else if (DelayingActions()) {
         mPostRecordingAction = std::move(action);
         return;
      }
   }

   // Don't delay it except until idle time.
   // (Recording might start between now and then, but won't go far before
   // the action is done.  So the system isn't bulletproof yet.)
   wxTheApp->CallAfter(std::move(action));
}

bool AudioIO::AllocateBuffers(
   const AudioIOStartStreamOptions &options,
   const TransportTracks &tracks, double t0, double t1, double sampleRate,
   bool scrubbing )
{
   bool success = false;
   auto cleanup = finally([&]{
      if (!success) StartStreamCleanup( false );
   });

   //
   // The (audio) stream has been opened successfully (assuming we tried
   // to open it). We now proceed to
   // allocate the memory structures the stream will need.
   //

   //
   // The RingBuffer sizes, and the max amount of the buffer to
   // fill at a time, both grow linearly with the number of
   // tracks.  This allows us to scale up to many tracks without
   // killing performance.
   //

   // real playback time to produce with each filling of the buffers
   // by the Audio thread (except at the end of playback):
   // usually, make fillings fewer and longer for less CPU usage.
   // But for useful scrubbing, we can't run too far ahead without checking
   // mouse input, so make fillings more and shorter.
   // What Audio thread produces for playback is then consumed by the PortAudio
   // thread, in many smaller pieces.
   double playbackTime = 4.0;
   if (scrubbing)
      // Specify a very short minimum batch for non-seek scrubbing, to allow
      // more frequent polling of the mouse
      playbackTime =
         lrint(options.pScrubbingOptions->delay * mRate) / mRate;
   
   wxASSERT( playbackTime >= 0 );
   mPlaybackSamplesToCopy = playbackTime * mRate;

   // Capacity of the playback buffer.
   mPlaybackRingBufferSecs = 10.0;

   mCaptureRingBufferSecs =
      4.5 + 0.5 * std::min(size_t(16), mCaptureTracks.size());
   mMinCaptureSecsToCopy =
      0.2 + 0.2 * std::min(size_t(16), mCaptureTracks.size());

   mTimeQueue.mHead = {};
   mTimeQueue.mTail = {};
   bool bDone;
   do
   {
      bDone = true; // assume success
      try
      {
         if( mNumPlaybackChannels > 0 ) {
            // Allocate output buffers.  For every output track we allocate
            // a ring buffer of ten seconds
            auto playbackBufferSize =
               (size_t)lrint(mRate * mPlaybackRingBufferSecs);

            mPlaybackBuffers.reinit(mPlaybackTracks.size());
            mPlaybackMixers.reinit(mPlaybackTracks.size());

            const Mixer::WarpOptions &warpOptions =
#ifdef EXPERIMENTAL_SCRUBBING_SUPPORT
               scrubbing
                  ? Mixer::WarpOptions
                     (ScrubbingOptions::MinAllowedScrubSpeed(),
                      ScrubbingOptions::MaxAllowedScrubSpeed())
                  :
#endif
                    Mixer::WarpOptions(mPlaybackSchedule.mEnvelope);

            mPlaybackQueueMinimum = mPlaybackSamplesToCopy;
            if (scrubbing)
               // Specify enough playback RingBuffer latency so we can refill
               // once every seek stutter without falling behind the demand.
               // (Scrub might switch in and out of seeking with left mouse
               // presses in the ruler)
               mPlaybackQueueMinimum = lrint(
                  2 * options.pScrubbingOptions->minStutterTime * mRate );
            mPlaybackQueueMinimum =
               std::min( mPlaybackQueueMinimum, playbackBufferSize );

            for (unsigned int i = 0; i < mPlaybackTracks.size(); i++)
            {
               // Bug 1763 - We must fade in from zero to avoid a click on starting.
               mPlaybackTracks[i]->SetOldChannelGain(0, 0.0);
               mPlaybackTracks[i]->SetOldChannelGain(1, 0.0);

               mPlaybackBuffers[i] =
                  std::make_unique<RingBuffer>(floatSample, playbackBufferSize);
               const auto timeQueueSize = 1 +
                  (playbackBufferSize + TimeQueueGrainSize - 1)
                     / TimeQueueGrainSize;
               mTimeQueue.mData.reinit( timeQueueSize );
               mTimeQueue.mSize = timeQueueSize;

               // use track time for the end time, not real time!
               WaveTrackConstArray mixTracks;
               mixTracks.push_back(mPlaybackTracks[i]);

               double endTime;
               if (make_iterator_range(tracks.prerollTracks)
                      .contains(mPlaybackTracks[i]))
                  // Stop playing this track after pre-roll
                  endTime = t0;
               else
                  // Pass t1 -- not mT1 as may have been adjusted for latency
                  // -- so that overdub recording stops playing back samples
                  // at the right time, though transport may continue to record
                  endTime = t1;

               mPlaybackMixers[i] = std::make_unique<Mixer>
                  (mixTracks,
                  // Don't throw for read errors, just play silence:
                  false,
                  warpOptions,
                  mPlaybackSchedule.mT0,
                  endTime,
                  1,
                  std::max( mPlaybackSamplesToCopy, mPlaybackQueueMinimum ),
                  false,
                  mRate, floatSample,
                  false, // low quality dithering and resampling
                  nullptr,
                  false // don't apply track gains
               );
            }
         }

         if( mNumCaptureChannels > 0 )
         {
            // Allocate input buffers.  For every input track we allocate
            // a ring buffer of five seconds
            auto captureBufferSize =
               (size_t)(mRate * mCaptureRingBufferSecs + 0.5);

            // In the extraordinarily rare case that we can't even afford
            // 100 samples, just give up.
            if(captureBufferSize < 100)
            {
               throw std::bad_alloc();
            }

            mCaptureBuffers.reinit(mCaptureTracks.size());
            mResample.reinit(mCaptureTracks.size());
            mFactor = sampleRate / mRate;

            for( unsigned int i = 0; i < mCaptureTracks.size(); i++ )
            {
               mCaptureBuffers[i] = std::make_unique<RingBuffer>(
                  mCaptureTracks[i]->GetSampleFormat(), captureBufferSize );
               mResample[i] =
                  std::make_unique<Resample>(true, mFactor, mFactor);
                  // constant rate resampling
            }
         }
      }
      catch(std::bad_alloc&)
      {
         // Oops!  Ran out of memory.  This is pretty rare, so we'll just
         // try deleting everything, halving our buffer size, and try again.
         StartStreamCleanup(true);
         mPlaybackRingBufferSecs *= 0.5;
         mPlaybackSamplesToCopy /= 2;
         mCaptureRingBufferSecs *= 0.5;
         mMinCaptureSecsToCopy *= 0.5;
         bDone = false;

         // In the extraordinarily rare case that we can't even afford 100
         // samples, just give up.
         auto playbackBufferSize =
            (size_t)lrint(mRate * mPlaybackRingBufferSecs);
         if(playbackBufferSize < 100 || mPlaybackSamplesToCopy < 100)
         {
            throw std::bad_alloc();
         }
      }
   } while(!bDone);
   
   success = true;
   return true;
}

void AudioIO::StartStreamCleanup(bool bOnlyBuffers)
{
   if (mNumPlaybackChannels > 0)
   {
      RealtimeEffectManager::Get().RealtimeFinalize();
   }

   mPlaybackBuffers.reset();
   mPlaybackMixers.reset();
   mCaptureBuffers.reset();
   mResample.reset();
   mTimeQueue.mData.reset();

   if(!bOnlyBuffers)
   {
      Pa_AbortStream( mPortStreamV19 );
      Pa_CloseStream( mPortStreamV19 );
      mPortStreamV19 = NULL;
      mStreamToken = 0;
   }

#ifdef EXPERIMENTAL_SCRUBBING_SUPPORT
   mScrubState.reset();
#endif
}

bool AudioIO::IsAvailable(TenacityProject *project) const
{
   return mOwningProject == NULL || mOwningProject == project;
}

void AudioIO::SetMeters()
{
   if (auto pInputMeter = mInputMeter.lock())
      pInputMeter->Reset(mRate, true);
   if (auto pOutputMeter = mOutputMeter.lock())
      pOutputMeter->Reset(mRate, true);

   mUpdateMeters = true;
}

void AudioIO::StopStream()
{
   auto cleanup = finally ( [this] {
      ClearRecordingException();
      mRecordingSchedule.mCrossfadeData.clear(); // free arrays
   } );

   if( mPortStreamV19 == NULL
#ifdef EXPERIMENTAL_MIDI_OUT
       && mMidiStream == NULL
#endif
     )
      return;

   if( Pa_IsStreamStopped( mPortStreamV19 )
#ifdef EXPERIMENTAL_MIDI_OUT
       && !mMidiStreamActive
#endif
     )
      return;

#if (defined(__WXMAC__) || defined(__WXMSW__)) && wxCHECK_VERSION(3,1,0)
   // Re-enable system sleep
   wxPowerResource::Release(wxPOWER_RESOURCE_SCREEN);
#endif
 
   if( mAudioThreadFillBuffersLoopRunning)
   {
      // PortAudio callback can use the information that we are stopping to fade
      // out the audio.  Give PortAudio callback a chance to do so.
      mAudioThreadFillBuffersLoopRunning = false;
      auto latency = static_cast<long>(AudioIOLatencyDuration.Read());
      // If we can gracefully fade out in 200ms, with the faded-out play buffers making it through
      // the sound card, then do so.  If we can't, don't wait around.  Just stop quickly and accept 
      // there will be a click.
      if( mbMicroFades  && (latency < 150 ))
         std::this_thread::sleep_for(std::chrono::milliseconds(latency + 50));
   }

   std::lock_guard<std::mutex> locker(mSuspendAudioThread);

   // No longer need effects processing
   if (mNumPlaybackChannels > 0)
   {
      RealtimeEffectManager::Get().RealtimeFinalize();
   }

   //
   // We got here in one of two ways:
   //
   // 1. The user clicked the stop button and we therefore want to stop
   //    as quickly as possible.  So we use AbortStream().  If this is
   //    the case the portaudio stream is still in the Running state
   //    (see PortAudio state machine docs).
   //
   // 2. The callback told PortAudio to stop the stream since it had
   //    reached the end of the selection.  The UI thread discovered
   //    this by noticing that AudioIO::IsActive() returned false.
   //    IsActive() (which calls Pa_GetStreamActive()) will not return
   //    false until all buffers have finished playing, so we can call
   //    AbortStream without losing any samples.  If this is the case
   //    we are in the "callback finished state" (see PortAudio state
   //    machine docs).
   //
   // The moral of the story: We can call AbortStream safely, without
   // losing samples.
   //
   // DMM: This doesn't seem to be true; it seems to be necessary to
   // call StopStream if the callback brought us here, and AbortStream
   // if the user brought us here.
   //

   mAudioThreadFillBuffersLoopRunning = false;

   // Audacity can deadlock if it tries to update meters while
   // we're stopping PortAudio (because the meter updating code
   // tries to grab a UI mutex while PortAudio tries to join a
   // pthread).  So we tell the callback to stop updating meters,
   // and wait until the callback has left this part of the code
   // if it was already there.
   mUpdateMeters = false;
   while(mUpdatingMeters) {
      ::wxSafeYield();
      std::this_thread::sleep_for(std::chrono::milliseconds(50));
   }

   if (mPortStreamV19) {
      Pa_AbortStream( mPortStreamV19 );
      Pa_CloseStream( mPortStreamV19 );
      mPortStreamV19 = NULL;
   }

#ifdef EXPERIMENTAL_MIDI_OUT
   /* Stop Midi playback */
   if ( mMidiStream ) {

      mMidiStreamActive = false;

#ifdef USE_MIDI_THREAD
      mMidiThreadFillBuffersLoopRunning = false; // stop output to stream
      // but output is in another thread. Wait for output to stop...
      while (mMidiThreadFillBuffersLoopActive) {
         std::this_thread::sleep_for(std::chrono::milliseconds(1));
      }
#endif

      mMidiOutputComplete = true;

      // now we can assume "ownership" of the mMidiStream
      // if output in progress, send all off, etc.
      AllNotesOff();
      // AllNotesOff() should be sufficient to stop everything, but
      // in Linux, if you Pm_Close() immediately, it looks like
      // messages are dropped. ALSA then seems to send All Sound Off
      // and Reset All Controllers messages, but not all synthesizers
      // respond to these messages. This is probably a bug in PortMidi
      // if the All Off messages do not get out, but for security,
      // delay a bit so that messages can be delivered before closing
      // the stream. Add 2ms of "padding" to avoid any rounding errors.
      while (mMaxMidiTimestamp + 2 > MidiTime()) {
          std::this_thread::sleep_for(std::chrono::milliseconds(1)); // deliver the all-off messages
      }
      Pm_Close(mMidiStream);
      mMidiStream = NULL;
      mIterator->end();

      // set in_use flags to false
      int nTracks = mMidiPlaybackTracks.size();
      for (int i = 0; i < nTracks; i++) {
         const auto t = mMidiPlaybackTracks[i].get();
         Alg_seq_ptr seq = &t->GetSeq();
         seq->set_in_use(false);
      }

      mIterator.reset(); // just in case someone tries to reference it
   }
#endif

   auto pListener = GetListener();
   
   // If there's no token, we were just monitoring, so we can
   // skip this next part...
   if (mStreamToken > 0) {
      // In either of the above cases, we want to make sure that any
      // capture data that made it into the PortAudio callback makes it
      // to the target WaveTrack.  To do this, we ask the audio thread to
      // call FillBuffers one last time (it normally would not do so since
      // Pa_GetStreamActive() would now return false
      mAudioThreadShouldCallFillBuffersOnce = true;

      while( mAudioThreadShouldCallFillBuffersOnce )
      {
         // LLL:  Experienced recursive yield here...once.
         wxTheApp->Yield(true); // Pass true for onlyIfNeeded to avoid recursive call error.
         std::this_thread::sleep_for(std::chrono::milliseconds( 50 ));
      }

      //
      // Everything is taken care of.  Now, just free all the resources
      // we allocated in StartStream()
      //

      if (mPlaybackTracks.size() > 0)
      {
         mPlaybackBuffers.reset();
         mPlaybackMixers.reset();
         mTimeQueue.mData.reset();
      }

      //
      // Offset all recorded tracks to account for latency
      //
      if (mCaptureTracks.size() > 0)
      {
         mCaptureBuffers.reset();
         mResample.reset();

         //
         // We only apply latency correction when we actually played back
         // tracks during the recording. If we did not play back tracks,
         // there's nothing we could be out of sync with. This also covers the
         // case that we do not apply latency correction when recording the
         // first track in a project.
         //

         for (unsigned int i = 0; i < mCaptureTracks.size(); i++) {
            // The calls to Flush
            // may cause exceptions because of exhaustion of disk space.
            // Stop those exceptions here, or else they propagate through too
            // many parts of Audacity that are not effects or editing
            // operations.  GuardedCall ensures that the user sees a warning.

            // Also be sure to Flush each track, at the top of the guarded call,
            // relying on the guarantee that the track will be left in a flushed
            // state, though the append buffer may be lost.

            GuardedCall( [&] {
               WaveTrack* track = mCaptureTracks[i].get();

               // use No-fail-guarantee that track is flushed,
               // Partial-guarantee that some initial length of the recording
               // is saved.
               // See comments in FillBuffers().
               track->Flush();
            } );
         }

         
         if (!mLostCaptureIntervals.empty())
         {
            // This scope may combine many splittings of wave tracks
            // into one transaction, lessening the number of checkpoints
            std::optional<TransactionScope> pScope;
            if (mOwningProject) {
               auto &pIO = ProjectFileIO::Get(*mOwningProject);
               pScope.emplace(pIO.GetConnection(), "Dropouts");
            }
            for (auto &interval : mLostCaptureIntervals) {
               auto &start = interval.first;
               auto duration = interval.second;
               for (auto &track : mCaptureTracks) {
                  GuardedCall([&] {
                     track->SyncLockAdjust(start, start + duration);
                  });
               }
            }
            if (pScope)
               pScope->Commit();
         }

         if (pListener)
            pListener->OnCommitRecording();
      }
   }

   if (auto pInputMeter = mInputMeter.lock())
      pInputMeter->Reset(mRate, false);

   if (auto pOutputMeter = mOutputMeter.lock())
      pOutputMeter->Reset(mRate, false);

   mInputMeter.reset();
   mOutputMeter.reset();
   mOwningProject = nullptr;

   if (pListener && mNumCaptureChannels > 0)
      pListener->OnAudioIOStopRecording();

   wxTheApp->CallAfter([this]{
      if (mPortStreamV19 && mNumCaptureChannels > 0)
         // Recording was restarted between StopStream and idle time
         // So the actions can keep waiting
         return;
      // In case some other thread was waiting on the mutex too:
      std::this_thread::yield();
      std::lock_guard<std::mutex> guard{ mPostRecordingActionMutex };
      if (mPostRecordingAction) {
         mPostRecordingAction();
         mPostRecordingAction = {};
      }
      DelayActions(false);
   });

   //
   // Only set token to 0 after we're totally finished with everything
   //
   bool wasMonitoring = mStreamToken == 0;
   mStreamToken = 0;

   if (mNumPlaybackChannels > 0)
   {
      wxCommandEvent e(EVT_AUDIOIO_PLAYBACK);
      e.SetEventObject(mOwningProject);
      e.SetInt(false);
      wxTheApp->ProcessEvent(e);
   }
   
   if (mNumCaptureChannels > 0)
   {
      wxCommandEvent e(wasMonitoring ? EVT_AUDIOIO_MONITOR : EVT_AUDIOIO_CAPTURE);
      e.SetEventObject(mOwningProject);
      e.SetInt(false);
      wxTheApp->ProcessEvent(e);
   }

   mNumCaptureChannels = 0;
   mNumPlaybackChannels = 0;

   mPlaybackTracks.clear();
   mCaptureTracks.clear();
#if defined(EXPERIMENTAL_MIDI_OUT) && defined(USE_MIDI)
   mMidiPlaybackTracks.clear();
#endif

#ifdef EXPERIMENTAL_SCRUBBING_SUPPORT
   mScrubState.reset();
#endif

   if (pListener) {
      // Tell UI to hide sample rate
      pListener->OnAudioIORate(0);
   }

   // Don't cause a busy wait in the audio thread after stopping scrubbing
   mPlaybackSchedule.ResetMode();
}

void AudioIO::SetPaused(bool state)
{
   if (state != mPaused)
   {
      if (state)
      {
         RealtimeEffectManager::Get().RealtimeSuspend();
      }
      else
      {
         RealtimeEffectManager::Get().RealtimeResume();
      }
   }

   mPaused = state;
}

#ifdef EXPERIMENTAL_SCRUBBING_SUPPORT
void AudioIO::UpdateScrub
   (double endTimeOrSpeed, const ScrubbingOptions &options)
{
   if (mScrubState)
      mScrubState->Update(endTimeOrSpeed, options);
}

void AudioIO::StopScrub()
{
   if (mScrubState)
      mScrubState->Stop();
}

#if 0
// Only for DRAG_SCRUB
double AudioIO::GetLastScrubTime() const
{
   if (mScrubState)
      return mScrubState->LastTrackTime();
   else
      return -1.0;
}
#endif

#endif

double AudioIO::GetBestRate(bool capturing, bool playing, double sampleRate)
{
   // Check if we can use the cached value
   if (mCachedBestRateIn != 0.0 && mCachedBestRateIn == sampleRate
      && mCachedBestRatePlaying == playing && mCachedBestRateCapturing == capturing) {
      return mCachedBestRateOut;
   }

   // In order to cache the value, all early returns should instead set retval
   // and jump to finished
   double retval;

   std::vector<long> rates;
   if (capturing) wxLogDebug(wxT("AudioIO::GetBestRate() for capture"));
   if (playing) wxLogDebug(wxT("AudioIO::GetBestRate() for playback"));
   wxLogDebug(wxT("GetBestRate() suggested rate %.0lf Hz"), sampleRate);

   if (capturing && !playing) {
      rates = GetSupportedCaptureRates(-1, sampleRate);
   }
   else if (playing && !capturing) {
      rates = GetSupportedPlaybackRates(-1, sampleRate);
   }
   else {   // we assume capturing and playing - the alternative would be a
            // bit odd
      rates = GetSupportedSampleRates(-1, -1, sampleRate);
   }
   /* rem rates is the array of hardware-supported sample rates (in the current
    * configuration), sampleRate is the Project Rate (desired sample rate) */
   long rate = (long)sampleRate;

   if (make_iterator_range(rates).contains(rate)) {
      wxLogDebug(wxT("GetBestRate() Returning %.0ld Hz"), rate);
      retval = rate;
      goto finished;
      /* the easy case - the suggested rate (project rate) is in the list, and
       * we can just accept that and send back to the caller. This should be
       * the case for most users most of the time (all of the time on
       * Win MME as the OS does resampling) */
   }

   /* if we get here, there is a problem - the project rate isn't supported
    * on our hardware, so we can't us it. Need to come up with an alternative
    * rate to use. The process goes like this:
    * * If there are no rates to pick from, we're stuck and return 0 (error)
    * * If there are some rates, we pick the next one higher than the requested
    *   rate to use.
    * * If there aren't any higher, we use the highest available rate */

   if (rates.empty()) {
      /* we're stuck - there are no supported rates with this hardware. Error */
      wxLogDebug(wxT("GetBestRate() Error - no supported sample rates"));
      retval = 0.0;
      goto finished;
   }
   int i;
   for (i = 0; i < (int)rates.size(); i++)  // for each supported rate
         {
         if (rates[i] > rate) {
            // supported rate is greater than requested rate
            wxLogDebug(wxT("GetBestRate() Returning next higher rate - %.0ld Hz"), rates[i]);
            retval = rates[i];
            goto finished;
         }
         }

   wxLogDebug(wxT("GetBestRate() Returning highest rate - %.0ld Hz"), rates.back());
   retval = rates.back(); // the highest available rate
   goto finished;

finished:
   mCachedBestRateIn = sampleRate;
   mCachedBestRateOut = retval;
   mCachedBestRatePlaying = playing;
   mCachedBestRateCapturing = capturing;
   return retval;
}

double AudioIO::GetStreamTime()
{
   // Track time readout for the main thread

   if( !IsStreamActive() )
      return BAD_STREAM_TIME;

   return mPlaybackSchedule.NormalizeTrackTime();
}


//////////////////////////////////////////////////////////////////////
//
//     Audio Thread Context
//
//////////////////////////////////////////////////////////////////////

/*AudioThread::ExitCode AudioThread::Entry()
{
   AudioIO *gAudioIO;
   while( !TestDestroy() &&
      nullptr != ( gAudioIO = AudioIO::Get() ) )
   {
      using Clock = std::chrono::steady_clock;
      auto loopPassStart = Clock::now();
      const auto interval = ScrubPollInterval_ms;

      // Set LoopActive outside the tests to avoid race condition
      gAudioIO->mAudioThreadFillBuffersLoopActive = true;
      if( gAudioIO->mAudioThreadShouldCallFillBuffersOnce )
      {
         gAudioIO->FillBuffers();
         gAudioIO->mAudioThreadShouldCallFillBuffersOnce = false;
      }
      else if( gAudioIO->mAudioThreadFillBuffersLoopRunning )
      {
         gAudioIO->FillBuffers();
      }
      gAudioIO->mAudioThreadFillBuffersLoopActive = false;

      if ( gAudioIO->mPlaybackSchedule.Interactive() )
         std::this_thread::sleep_until(
            loopPassStart + std::chrono::milliseconds( interval ) );
      else
         std::this_thread::sleep_until(std::chrono::milliseconds(10));
   }

   return 0;
}

#ifdef EXPERIMENTAL_MIDI_OUT
MidiThread::ExitCode MidiThread::Entry()
{
   AudioIO *gAudioIO;
   while( !TestDestroy() &&
      nullptr != ( gAudioIO = AudioIO::Get() ) )
   {
      // Set LoopActive outside the tests to avoid race condition
      gAudioIO->mMidiThreadFillBuffersLoopActive = true;
      if( gAudioIO->mMidiThreadFillBuffersLoopRunning &&
          // mNumFrames signals at least one callback, needed for MidiTime()
          gAudioIO->mNumFrames > 0)
      {
         gAudioIO->FillMidiBuffers();
      }
      gAudioIO->mMidiThreadFillBuffersLoopActive = false;
      Sleep(MIDI_SLEEP);
   }
   return 0;
}
#endif*/

size_t AudioIO::GetCommonlyFreePlayback()
{
   auto commonlyAvail = mPlaybackBuffers[0]->AvailForPut();
   for (unsigned i = 1; i < mPlaybackTracks.size(); ++i)
      commonlyAvail = std::min(commonlyAvail,
         mPlaybackBuffers[i]->AvailForPut());
   // MB: subtract a few samples because the code in FillBuffers has rounding
   // errors
   return commonlyAvail - std::min(size_t(10), commonlyAvail);
}

size_t AudioIO::GetCommonlyAvailCapture()
{
   auto commonlyAvail = mCaptureBuffers[0]->AvailForGet();
   for (unsigned i = 1; i < mCaptureTracks.size(); ++i)
      commonlyAvail = std::min(commonlyAvail,
         mCaptureBuffers[i]->AvailForGet());
   return commonlyAvail;
}

// This method is the data gateway between the audio thread (which
// communicates with the disk) and the PortAudio callback thread
// (which communicates with the audio device).
void AudioIO::FillBuffers()
{
   unsigned int i;

   auto delayedHandler = [this] ( TenacityException * pException ) {
      // In the main thread, stop recording
      // This is one place where the application handles disk
      // exhaustion exceptions from wave track operations, without rolling
      // back to the last pushed undo state.  Instead, partial recording
      // results are pushed as a NEW undo state.  For this reason, as
      // commented elsewhere, we want an exception safety guarantee for
      // the output wave tracks, after the failed append operation, that
      // the tracks remain as they were after the previous successful
      // (block-level) appends.

      // Note that the Flush in StopStream() may throw another exception,
      // but StopStream() contains that exception, and the logic in
      // TenacityException::DelayedHandlerAction prevents redundant message
      // boxes.
      StopStream();
      DefaultDelayedHandlerAction{}( pException );
   };

   if (mPlaybackTracks.size() > 0)
   {
      // Though extremely unlikely, it is possible that some buffers
      // will have more samples available than others.  This could happen
      // if we hit this code during the PortAudio callback.  To keep
      // things simple, we only write as much data as is vacant in
      // ALL buffers, and advance the global time by that much.
      auto nAvailable = GetCommonlyFreePlayback();

      //
      // Don't fill the buffers at all unless we can do the
      // full mMaxPlaybackSecsToCopy.  This improves performance
      // by not always trying to process tiny chunks, eating the
      // CPU unnecessarily.
      //
      // The exception is if we're at the end of the selected
      // region - then we should just fill the buffer.
      //
      // May produce a larger amount when initially priming the buffer, or
      // perhaps again later in play to avoid underfilling the queue and falling
      // behind the real-time demand on the consumer side in the callback.
      auto nReady = GetCommonlyReadyPlayback();
      auto nNeeded =
         mPlaybackQueueMinimum - std::min(mPlaybackQueueMinimum, nReady);

      // wxASSERT( nNeeded <= nAvailable );

      auto realTimeRemaining = mPlaybackSchedule.RealTimeRemaining();
      if (nAvailable >= mPlaybackSamplesToCopy ||
          (mPlaybackSchedule.PlayingStraight() &&
           nAvailable / mRate >= realTimeRemaining))
      {
         // Limit maximum buffer size (increases performance)
         auto available = std::min( nAvailable,
            std::max( nNeeded, mPlaybackSamplesToCopy ) );

         // msmeyer: When playing a very short selection in looped
         // mode, the selection must be copied to the buffer multiple
         // times, to ensure, that the buffer has a reasonable size
         // This is the purpose of this loop.
         // PRL: or, when scrubbing, we may get work repeatedly from the
         // user interface.
         bool done = false;
         do {
            // How many samples to produce for each channel.
            auto frames = available;
            bool progress = true;
            auto toProcess = frames;
#ifdef EXPERIMENTAL_SCRUBBING_SUPPORT
            if (mPlaybackSchedule.Interactive())
               // scrubbing and play-at-speed are not limited by the real time
               // and length accumulators
               toProcess =
               frames = limitSampleBufferSize(frames, mScrubDuration);
            else
#endif
            {
               double deltat = frames / mRate;
               if (deltat > realTimeRemaining)
               {
                  frames = realTimeRemaining * mRate;
                  toProcess = frames;
                  // Don't fall into an infinite loop, if loop-playing a selection
                  // that is so short, it has no samples: detect that case
                  progress =
                     !(mPlaybackSchedule.Looping() &&
                       mPlaybackSchedule.mWarpedTime == 0.0 && frames == 0);
                  mPlaybackSchedule.RealTimeAdvance( realTimeRemaining );
               }
               else
                  mPlaybackSchedule.RealTimeAdvance( deltat );
               realTimeRemaining = mPlaybackSchedule.RealTimeRemaining();
            }

            if (!progress)
               frames = available, toProcess = 0;
#ifdef EXPERIMENTAL_SCRUBBING_SUPPORT
            else if ( mPlaybackSchedule.Interactive() && mSilentScrub)
               toProcess = 0;
#endif

            // Update the time queue.  This must be done before writing to the
            // ring buffers of samples, for proper synchronization with the
            // consumer side in the PortAudio thread, which reads the time
            // queue after reading the sample queues.  The sample queues use
            // atomic variables, the time queue doesn't.
            mTimeQueue.Producer( mPlaybackSchedule, mRate,
               (mPlaybackSchedule.Interactive() ? mScrubSpeed : 1.0),
               frames);

            for (i = 0; i < mPlaybackTracks.size(); i++)
            {
               // The mixer here isn't actually mixing: it's just doing
               // resampling, format conversion, and possibly time track
               // warping
               samplePtr warpedSamples;

               if (frames > 0)
               {
                  size_t processed = 0;
                  if ( toProcess )
                     processed = mPlaybackMixers[i]->Process( toProcess );
                  //wxASSERT(processed <= toProcess);
                  warpedSamples = mPlaybackMixers[i]->GetBuffer();
                  const auto put = mPlaybackBuffers[i]->Put(
                     warpedSamples, floatSample, processed, frames - processed);
                  // wxASSERT(put == frames);
                  // but we can't assert in this thread
                  wxUnusedVar(put);
               }               
            }

            available -= frames;
            wxASSERT(available >= 0);

            switch (mPlaybackSchedule.mPlayMode)
            {
#ifdef EXPERIMENTAL_SCRUBBING_SUPPORT
            case PlaybackSchedule::PLAY_SCRUB:
            case PlaybackSchedule::PLAY_AT_SPEED:
            case PlaybackSchedule::PLAY_KEYBOARD_SCRUB:
            {
               mScrubDuration -= frames;
               wxASSERT(mScrubDuration >= 0);
               done = (available == 0);
               if (!done && mScrubDuration <= 0)
               {
                  sampleCount startSample, endSample;
                  mScrubState->Get(
                     startSample, endSample, available, mScrubDuration);
                  if (mScrubDuration < 0)
                  {
                     // Can't play anything
                     // Stop even if we don't fill up available
                     mScrubDuration = 0;
                     done = true;
                  }
                  else
                  {
                     mSilentScrub = (endSample == startSample);
                     double startTime, endTime;
                     startTime = startSample.as_double() / mRate;
                     endTime = endSample.as_double() / mRate;
                     auto diff = (endSample - startSample).as_long_long();
                     if (mScrubDuration == 0)
                        mScrubSpeed = 0;
                     else
                        mScrubSpeed =
                           double(diff) / mScrubDuration.as_double();
                     if (!mSilentScrub)
                     {
                        for (i = 0; i < mPlaybackTracks.size(); i++) {
                           if (mPlaybackSchedule.mPlayMode == PlaybackSchedule::PLAY_AT_SPEED)
                              mPlaybackMixers[i]->SetSpeedForPlayAtSpeed(mScrubSpeed);
                           else if (mPlaybackSchedule.mPlayMode == PlaybackSchedule::PLAY_KEYBOARD_SCRUB)
                              mPlaybackMixers[i]->SetSpeedForKeyboardScrubbing(mScrubSpeed, startTime);
                           else
                              mPlaybackMixers[i]->SetTimesAndSpeed(
                                 startTime, endTime, fabs( mScrubSpeed ));
                        }
                     }
                     mTimeQueue.mLastTime = startTime;
                  }
               }
            }
               break;
#endif
            case PlaybackSchedule::PLAY_LOOPED:
            {
               done = !progress || (available == 0);
               // msmeyer: If playing looped, check if we are at the end of the buffer
               // and if yes, restart from the beginning.
               if (realTimeRemaining <= 0)
               {
                  for (i = 0; i < mPlaybackTracks.size(); i++)
                     mPlaybackMixers[i]->Restart();
                  mPlaybackSchedule.RealTimeRestart();
                  realTimeRemaining = mPlaybackSchedule.RealTimeRemaining();
               }
            }
               break;
            default:
               done = true;
               break;
            }
         } while (!done);
      }
   }  // end of playback buffering

   if (!mRecordingException &&
       mCaptureTracks.size() > 0)
      GuardedCall( [&] {
         // start record buffering
         const auto avail = GetCommonlyAvailCapture(); // samples
         const auto remainingTime =
            std::max(0.0, mRecordingSchedule.ToConsume());
         // This may be a very big double number:
         const auto remainingSamples = remainingTime * mRate;
         bool latencyCorrected = true;

         double deltat = avail / mRate;

         if (mAudioThreadShouldCallFillBuffersOnce ||
             deltat >= mMinCaptureSecsToCopy)
         {
            // This scope may combine many appendings of wave tracks,
            // and also an autosave, into one transaction,
            // lessening the number of checkpoints
            std::optional<TransactionScope> pScope;
            if (mOwningProject) {
               auto &pIO = ProjectFileIO::Get(*mOwningProject);
               pScope.emplace(pIO.GetConnection(), "Recording");
            }

            bool newBlocks = false;

            // Append captured samples to the end of the WaveTracks.
            // The WaveTracks have their own buffering for efficiency.
            auto numChannels = mCaptureTracks.size();

            for( i = 0; i < numChannels; i++ )
            {
               sampleFormat trackFormat = mCaptureTracks[i]->GetSampleFormat();

               size_t discarded = 0;

               if (!mRecordingSchedule.mLatencyCorrected) {
                  const auto correction = mRecordingSchedule.TotalCorrection();
                  if (correction >= 0) {
                     // Rightward shift
                     // Once only (per track per recording), insert some initial
                     // silence.
                     size_t size = floor( correction * mRate * mFactor);
                     SampleBuffer temp(size, trackFormat);
                     ClearSamples(temp.ptr(), trackFormat, 0, size);
                     mCaptureTracks[i]->Append(temp.ptr(), trackFormat, size, 1);
                  }
                  else {
                     // Leftward shift
                     // discard some samples from the ring buffers.
                     size_t size = floor(
                        mRecordingSchedule.ToDiscard() * mRate );

                     // The ring buffer might have grown concurrently -- don't discard more
                     // than the "avail" value noted above.
                     discarded = mCaptureBuffers[i]->Discard(std::min(avail, size));

                     if (discarded < size)
                        // We need to visit this again to complete the
                        // discarding.
                        latencyCorrected = false;
                  }
               }

               const float *pCrossfadeSrc = nullptr;
               size_t crossfadeStart = 0, totalCrossfadeLength = 0;
               if (i < mRecordingSchedule.mCrossfadeData.size())
               {
                  // Do crossfading
                  // The supplied crossfade samples are at the same rate as the track
                  const auto &data = mRecordingSchedule.mCrossfadeData[i];
                  totalCrossfadeLength = data.size();
                  if (totalCrossfadeLength) {
                     crossfadeStart =
                        floor(mRecordingSchedule.Consumed() * mCaptureTracks[i]->GetRate());
                     if (crossfadeStart < totalCrossfadeLength)
                        pCrossfadeSrc = data.data() + crossfadeStart;
                  }
               }

               wxASSERT(discarded <= avail);
               size_t toGet = avail - discarded;
               SampleBuffer temp;
               size_t size;
               sampleFormat format;
               if( mFactor == 1.0 )
               {
                  // Take captured samples directly
                  size = toGet;
                  if (pCrossfadeSrc)
                     // Change to float for crossfade calculation
                     format = floatSample;
                  else
                     format = trackFormat;
                  temp.Allocate(size, format);
                  const auto got =
                     mCaptureBuffers[i]->Get(temp.ptr(), format, toGet);
                  // wxASSERT(got == toGet);
                  // but we can't assert in this thread
                  wxUnusedVar(got);
                  if (double(size) > remainingSamples)
                     size = floor(remainingSamples);
               }
               else
               {
                  size = lrint(toGet * mFactor);
                  format = floatSample;
                  SampleBuffer temp1(toGet, floatSample);
                  temp.Allocate(size, format);
                  const auto got =
                     mCaptureBuffers[i]->Get(temp1.ptr(), floatSample, toGet);
                  // wxASSERT(got == toGet);
                  // but we can't assert in this thread
                  wxUnusedVar(got);
                  /* we are re-sampling on the fly. The last resampling call
                   * must flush any samples left in the rate conversion buffer
                   * so that they get recorded
                   */
                  if (toGet > 0 ) {
                     if (double(toGet) > remainingSamples)
                        toGet = floor(remainingSamples);
                     const auto results =
                     mResample[i]->Process(mFactor, (float *)temp1.ptr(), toGet,
                                           !IsStreamActive(), (float *)temp.ptr(), size);
                     size = results.second;
                  }
               }

               if (pCrossfadeSrc) {
                  wxASSERT(format == floatSample);
                  size_t crossfadeLength = std::min(size, totalCrossfadeLength - crossfadeStart);
                  if (crossfadeLength) {
                     auto ratio = double(crossfadeStart) / totalCrossfadeLength;
                     auto ratioStep = 1.0 / totalCrossfadeLength;
                     auto pCrossfadeDst = (float*)temp.ptr();

                     // Crossfade loop here
                     for (size_t ii = 0; ii < crossfadeLength; ++ii) {
                        *pCrossfadeDst = ratio * *pCrossfadeDst + (1.0 - ratio) * *pCrossfadeSrc;
                        ++pCrossfadeSrc, ++pCrossfadeDst;
                        ratio += ratioStep;
                     }
                  }
               }

               // Now append
               // see comment in second handler about guarantee
               newBlocks = mCaptureTracks[i]->Append(temp.ptr(), format, size, 1)
                  || newBlocks;
            } // end loop over capture channels

            // Now update the recording schedule position
            mRecordingSchedule.mPosition += avail / mRate;
            mRecordingSchedule.mLatencyCorrected = latencyCorrected;

            auto pListener = GetListener();
            if (pListener && newBlocks)
               pListener->OnAudioIONewBlocks(&mCaptureTracks);

            if (pScope)
               pScope->Commit();
         }
         // end of record buffering
      },
      // handler
      [this] ( TenacityException *pException ) {
         if ( pException ) {
            // So that we don't attempt to fill the recording buffer again
            // before the main thread stops recording
            SetRecordingException();
            return ;
         }
         else
            // Don't want to intercept other exceptions (?)
            throw;
      },
      delayedHandler
   );
}

//////////////////////////////////////////////////////////////////////
//
//    PortAudio callback thread context
//
//////////////////////////////////////////////////////////////////////

int audacityAudioCallback(const void *inputBuffer, void *outputBuffer,
                          unsigned long framesPerBuffer,
                          const PaStreamCallbackTimeInfo *timeInfo,
                          const PaStreamCallbackFlags statusFlags, void *userData )
{
   auto gAudioIO = AudioIO::Get();
   return gAudioIO->AudioCallback(
      inputBuffer, outputBuffer, framesPerBuffer,
      timeInfo, statusFlags, userData);
}

#if 0
// Record the reported latency from PortAudio.
// TODO: Don't recalculate this with every callback?
// 01/21/2009:  Disabled until a better solution presents itself.
void OldCodeToCalculateLatency()
{
   // As of 06/17/2006, portaudio v19 returns inputBufferAdcTime set to
   // zero.  It is being worked on, but for now we just can't do much
   // but follow the leader.
   //
   // 08/27/2006: too inconsistent for now...just leave it a zero.
   //
   // 04/16/2008: Looks like si->inputLatency comes back with something useful though.
   // This rearranged logic uses si->inputLatency, but if PortAudio fixes inputBufferAdcTime,
   // this code won't have to be modified to use it.
   // Also avoids setting mLastRecordingOffset except when simultaneously playing and recording.
   //
   if (numCaptureChannels > 0 && numPlaybackChannels > 0) // simultaneously playing and recording
   {
      if (timeInfo->inputBufferAdcTime > 0)
         mLastRecordingOffset = timeInfo->inputBufferAdcTime - timeInfo->outputBufferDacTime;
      else if (mLastRecordingOffset == 0.0)
      {
         const PaStreamInfo* si = Pa_GetStreamInfo( mPortStreamV19 );
         mLastRecordingOffset = -si->inputLatency;
      }
   }
}
#endif

void AudioIO::TimeQueue::Producer(
   const PlaybackSchedule &schedule, double rate, double scrubSpeed,
   size_t nSamples )
{
   if ( ! mData )
      // Recording only.  Don't fill the queue.
      return;

   // Don't check available space:  assume it is enough because of coordination
   // with RingBuffer.
   auto index = mTail.mIndex;
   auto time = mLastTime;
   auto remainder = mTail.mRemainder;
   auto space = TimeQueueGrainSize - remainder;

   while ( nSamples >= space ) {
      time = schedule.AdvancedTrackTime( time, space / rate, scrubSpeed );
      index = (index + 1) % mSize;
      mData[ index ] = time;
      nSamples -= space;
      remainder = 0;
      space = TimeQueueGrainSize;
   }

   // Last odd lot
   if ( nSamples > 0 )
      time = schedule.AdvancedTrackTime( time, nSamples / rate, scrubSpeed );

   mLastTime = time;
   mTail.mRemainder = remainder + nSamples;
   mTail.mIndex = index;
}

double AudioIO::TimeQueue::Consumer( size_t nSamples, double rate )
{
   if ( ! mData ) {
      // Recording only.  No scrub or playback time warp.  Don't use the queue.
      return ( mLastTime += nSamples / rate );
   }

   // Don't check available space:  assume it is enough because of coordination
   // with RingBuffer.
   auto remainder = mHead.mRemainder;
   auto space = TimeQueueGrainSize - remainder;
   if ( nSamples >= space ) {
      remainder = 0,
      mHead.mIndex = (mHead.mIndex + 1) % mSize,
      nSamples -= space;
      if ( nSamples >= TimeQueueGrainSize )
         mHead.mIndex =
            (mHead.mIndex + ( nSamples / TimeQueueGrainSize ) ) % mSize,
         nSamples %= TimeQueueGrainSize;
   }
   mHead.mRemainder = remainder + nSamples;
   return mData[ mHead.mIndex ];
}

bool AudioIO::IsCapturing() const
{
   // Includes a test of mTime, used in the main thread
   return IsStreamActive() &&
      GetNumCaptureChannels() > 0 &&
      mPlaybackSchedule.GetTrackTime() >=
         mPlaybackSchedule.mT0 + mRecordingSchedule.mPreRoll;
}
