/**********************************************************************

  Audacity: A Digital Audio Editor

  MixerBoard.h

  Vaughan Johnson, January 2007

**********************************************************************/

#ifndef __AUDACITY_MIXER_BOARD__
#define __AUDACITY_MIXER_BOARD__

#include <wx/frame.h> // to inherit
#include <wx/scrolwin.h> // to inherit

#include "widgets/ASlider.h" // to inherit
#include "commands/CommandManagerWindowClasses.h"

// Tenacity library
#include <lib-preferences/Prefs.h>

class wxArrayString;
class wxBitmapButton;
class wxImage;
class wxMemoryDC;
class AButton;
struct TrackListEvent;

// containment hierarchy:
//    MixerBoardFrame -> MixerBoard -> MixerBoardScrolledWindow -> MixerChannel(s)


// MixerTrackSlider is a subclass just to override OnMouseEvent,
// so we can know when adjustment ends, so we can PushState only then.
class MixerTrackSlider final : public ASlider
{
public:
   MixerTrackSlider(wxWindow * parent,
                     wxWindowID id,
                     const TranslatableString &name,
                     const wxPoint & pos,
                     const wxSize & size,
                     const ASlider::Options &options = ASlider::Options{});
   virtual ~MixerTrackSlider() {}

   void OnMouseEvent(wxMouseEvent & event);

   void OnFocus(wxFocusEvent &event);
   void OnCaptureKey(wxCommandEvent& event);

protected:
   bool mIsPan;

public:
    DECLARE_EVENT_TABLE()
};


class TenacityProject;
class MeterPanel;
class MixerBoard;

class Track;
class PlayableTrack;

class WaveTrack;
class AuStaticText;

class MixerChannel final : public wxPanelWrapper
{
public:
   MixerChannel(wxWindow* parent,
                     MixerBoard* grandParent, TenacityProject* project,
                     const std::shared_ptr<PlayableTrack> &pTrack,
                     const wxPoint& pos = wxDefaultPosition,
                     const wxSize& size = wxDefaultSize);
   virtual ~MixerChannel() {}

   WaveTrack *GetWave() const;
   WaveTrack *GetRight() const;

   //void UpdatePrefs();

   void HandleResize(); // For wxSizeEvents, update gain slider and meter.

   void HandleSliderGain(const bool bWantPushState = false);
   void HandleSliderPan(const bool bWantPushState = false);

   void ResetMeter(const bool bResetClipping);

   void UpdateForStateChange();
   void UpdateMeter(const double t0, const double t1);

private:
   wxColour GetTrackColor();

   // event handlers
   void HandleSelect(bool bShiftDown, bool bControlDown);

   void OnKeyEvent(wxKeyEvent& event);
   void OnMouseEvent(wxMouseEvent& event);
   void OnPaint(wxPaintEvent& evt);

   void OnSlider_Gain(wxCommandEvent& event);
   void OnSlider_Pan(wxCommandEvent& event);
   void OnButton_Mute(wxCommandEvent& event);
   void OnButton_Solo(wxCommandEvent& event);
   //v void OnSliderScroll_Gain(wxScrollEvent& event);


public:
   std::shared_ptr<PlayableTrack>   mTrack;

private:
   MixerBoard* mMixerBoard;
   TenacityProject* mProject;

   // controls
   AuStaticText* mStaticText_TrackName;
   AButton* mToggleButton_Mute;
   AButton* mToggleButton_Solo;
   MixerTrackSlider* mSlider_Pan;
   MixerTrackSlider* mSlider_Gain;
   wxWeakRef<MeterPanel> mMeter;

public:
   DECLARE_EVENT_TABLE()
};

// wxScrolledWindow ignores mouse clicks in client area,
// but they don't get passed to Mixerboard.
// We need to catch them to deselect all track clusters.
class MixerBoardScrolledWindow final : public wxScrolledWindow
{
public:
   MixerBoardScrolledWindow(TenacityProject* project,
                              MixerBoard* parent, wxWindowID id = -1,
                              const wxPoint& pos = wxDefaultPosition,
                              const wxSize& size = wxDefaultSize,
                              long style = wxHSCROLL | wxVSCROLL);
   virtual ~MixerBoardScrolledWindow();

private:
   void OnMouseEvent(wxMouseEvent& event);

private:
   MixerBoard* mMixerBoard;
   TenacityProject* mProject;

public:
   DECLARE_EVENT_TABLE()
};


class MixerBoardFrame;
class TrackList;

class MixerBoard final : public wxWindow, private PrefsListener
{
   friend class MixerBoardFrame;

public:
   MixerBoard(TenacityProject* pProject,
               wxFrame* parent,
               const wxPoint& pos = wxDefaultPosition,
               const wxSize& size = wxDefaultSize);

   void UpdatePrefs() override;

   // Add clusters for any tracks we're not yet showing.
   // Update pointers for tracks we're already showing.
   void UpdateTrackClusters();

   int GetTrackClustersWidth();

   bool HasSolo();

   MixerChannel* AddChannel(const std::shared_ptr<PlayableTrack>& track, size_t index, int height);
   void RefreshTrackClusters(bool bEraseBackground = true);
   void ResizeTrackClusters();

   void UpdateMeters(const double t1, const bool bLoopedPlay);

   void UpdateWidth();

private:
   void ResetMeters(const bool bResetClipping);   
   void RemoveTrackCluster(size_t nIndex);
   void MakeButtonBitmap( wxMemoryDC & dc, wxBitmap & bitmap,
      wxRect & bev, const TranslatableString & str, bool up );
   void CreateMuteSoloImages();
   int FindMixerChannel(const PlayableTrack* pTrack,
                              MixerChannel** hMixerChannel) const;

   // event handlers
   void OnPaint(wxPaintEvent& evt);
   void OnSize(wxSizeEvent &evt);
   void OnTimer(wxCommandEvent &event);
   void OnTrackSetChanged(wxEvent &event);
   void OnTrackChanged(TrackListEvent &event);
   void OnStartStop(wxCommandEvent &event);

public:
   // mute & solo button images: Create once and store on MixerBoard for use in all MixerChannels.
   std::unique_ptr<wxImage> mImageMuteUp, mImageMuteOver, mImageMuteDown,
      mImageMuteDownWhileSolo, // the one actually alternate image
      mImageMuteDisabled, mImageSoloUp, mImageSoloOver, mImageSoloDown, mImageSoloDisabled;

   int mMuteSoloWidth;

private:
   // Track clusters are maintained in the same order as the WaveTracks.
   std::vector<MixerChannel*> mMixerChannels;

   wxButton*                  mAddChannelButton;
   TenacityProject*           mProject;
   MixerBoardScrolledWindow*  mScrolledWindow; // Holds the MixerChannels and handles scrolling.
   double                     mPrevT1;
   TrackList*                 mTracks;
   bool                       mUpToDate{ false };

public:
   DECLARE_EVENT_TABLE()
};


class MixerBoardFrame final
   : public wxFrame
   , public TopLevelKeystrokeHandlingWindow
{
public:
   MixerBoardFrame(TenacityProject* parent);
   virtual ~MixerBoardFrame();

   void Recreate(TenacityProject *pProject);

private:
   // event handlers
   void OnCloseWindow(wxCloseEvent &WXUNUSED(event));
   void OnMaximize(wxMaximizeEvent &event);
   void OnSize(wxSizeEvent &evt);
   void OnKeyEvent(wxKeyEvent &evt);

   void SetWindowTitle();

   TenacityProject *mProject;
public:
   MixerBoard* mMixerBoard;

public:
   DECLARE_EVENT_TABLE()
};

#endif // __AUDACITY_MIXER_BOARD__


