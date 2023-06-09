/**********************************************************************

 Audacity: A Digital Audio Editor

 NoteTrackSliderHandles.h

 Paul Licameli split from TrackPanel.cpp

 **********************************************************************/

#ifndef __AUDACITY_NOTE_TRACK_SLIDER_HANDLES__
#define __AUDACITY_NOTE_TRACK_SLIDER_HANDLES__

#ifdef EXPERIMENTAL_MIDI_OUT

#include "../../../ui/SliderHandle.h"

class NoteTrack;
class wxMouseState;

class VelocitySliderHandle final : public SliderHandle
{
   VelocitySliderHandle(const VelocitySliderHandle&) = delete;

   std::shared_ptr<NoteTrack> GetNoteTrack() const;

public:
   explicit VelocitySliderHandle
      ( SliderFn sliderFn, const wxRect &rect,
        const std::shared_ptr<Track> &pTrack );

   VelocitySliderHandle &operator=(const VelocitySliderHandle&) = default;

   virtual ~VelocitySliderHandle();

protected:
   float GetValue() override;
   Result SetValue
   (TenacityProject *pProject, float newValue) override;
   Result CommitChanges
   (const wxMouseEvent &event, TenacityProject *pProject) override;

   TranslatableString Tip(
      const wxMouseState &state, TenacityProject &) const override;

   bool StopsOnKeystroke () override { return true; }

public:
   static UIHandlePtr HitTest
      (std::weak_ptr<VelocitySliderHandle> &holder,
       const wxMouseState &state, const wxRect &rect,
       const std::shared_ptr<Track> &pTrack);
};

#endif

#endif
