/**********************************************************************

  Tenacity: A Digital Audio Editor

  HelpMenus.cpp

**********************************************************************/

// Tenacity libraries
#include <lib-audio-devices/AudioIOBase.h>
#include <lib-files/FileNames.h>
#include <lib-preferences/Prefs.h>
#include <lib-project/Project.h>

#include <wx/app.h>
#include <wx/bmpbuttn.h>
#include <wx/textctrl.h>
#include <wx/frame.h>

#include "../AboutDialog.h"
#include "../CommonCommandFlags.h"

#include "../HelpText.h"
#include "../LogWindow.h"
#include "../Menus.h"
#include "../ProjectSelectionManager.h"
#include "../ProjectWindows.h"
#include "../SelectFile.h"
#include "../shuttle/ShuttleGui.h"
#include "../SplashDialog.h"
#include "../theme/Theme.h"
#include "../commands/CommandContext.h"
#include "../commands/CommandManager.h"
#include "../prefs/PrefsDialog.h"
#include "../theme/AllThemeResources.h"
#include "../widgets/AudacityMessageBox.h"

// private helper classes and functions
namespace {

void ShowDiagnostics(
   TenacityProject &project, const wxString &info,
   const TranslatableString &description, const wxString &defaultPath,
   bool fixedWidth = false)
{
   auto &window = GetProjectFrame( project );
   wxDialogWrapper dlg( &window, wxID_ANY, description);
   dlg.SetName();
   ShuttleGui S(&dlg, eIsCreating);

   wxTextCtrl *text;
   S.StartVerticalLay();
   {
      text = S.Id(wxID_STATIC)
         .Style(wxTE_MULTILINE | wxTE_READONLY | wxTE_RICH)
         .AddTextWindow("");

      wxButton *save = safenew wxButton(S.GetParent(), wxID_OK, _("&Save"));
      S.AddStandardButtons(eCancelButton, save);
   }
   S.EndVerticalLay();

   if (fixedWidth) {
      auto style = text->GetDefaultStyle();
      style.SetFontFamily( wxFONTFAMILY_TELETYPE );
      text->SetDefaultStyle(style);
   }

   *text << info;

   dlg.SetSize(350, 450);

   if (dlg.ShowModal() == wxID_OK)
   {
      const auto fileDialogTitle = XO("Save %s").Format( description );
      wxString fName = SelectFile(FileNames::Operation::Export,
         fileDialogTitle,
         wxEmptyString,
         defaultPath,
         wxT("txt"),
         { FileNames::TextFiles },
         wxFD_SAVE | wxFD_OVERWRITE_PROMPT | wxRESIZE_BORDER,
         &window);
      if (!fName.empty())
      {
         if (!text->SaveFile(fName))
         {
            AudacityMessageBox(
               XO("Unable to save %s").Format( description ),
               fileDialogTitle);
         }
      }
   }
}

/** @brief Class which makes a dialog for displaying quick fixes to common issues.
 *
 * This class originated with the 'Stuck in a mode' problem, where far too many
 * users get into a mode without realising, and don't know how to get out.
 * It is a band-aid, and we should do more towards a full and proper solution
 * where there are fewer special modes, and they don't persist.
 */
class QuickFixDialog : public wxDialogWrapper
{
public:
   using PrefSetter = std::function< void() > ;

   QuickFixDialog(wxWindow * pParent, TenacityProject &project);
   void Populate();
   void PopulateOrExchange(ShuttleGui & S);
   void AddStuck( ShuttleGui & S, bool & bBool,
      const PrefSetter &prefSetter,
      const TranslatableString &Prompt, const ManualPageID &Help );

   void OnOk(wxCommandEvent &event);
   void OnCancel(wxCommandEvent &event);
   void OnFix(const PrefSetter &setter, wxWindowID id);

   TenacityProject &mProject;

   int mItem;
   bool mbSyncLocked;
   bool mbInSnapTo;
   bool mbSoundActivated;
   DECLARE_EVENT_TABLE()
};


#define FixButtonID           7001
#define HelpButtonID          7011

BEGIN_EVENT_TABLE(QuickFixDialog, wxDialogWrapper)
   EVT_BUTTON(wxID_OK,                                            QuickFixDialog::OnOk)
   EVT_BUTTON(wxID_CANCEL,                                        QuickFixDialog::OnCancel)
END_EVENT_TABLE();

QuickFixDialog::QuickFixDialog(wxWindow * pParent, TenacityProject &project) :
      wxDialogWrapper(pParent, wxID_ANY, XO("Do you have these problems?"),
            wxDefaultPosition, wxDefaultSize,
            wxDEFAULT_DIALOG_STYLE )
      , mProject{ project }
{
   const long SNAP_OFF = 0;

   gPrefs->Read(wxT("/GUI/SyncLockTracks"), &mbSyncLocked, false);
   mbInSnapTo = gPrefs->Read(wxT("/SnapTo"), SNAP_OFF) !=0;
   gPrefs->Read(wxT("/AudioIO/SoundActivatedRecord"), &mbSoundActivated, false);

   ShuttleGui S(this, eIsCreating);
   PopulateOrExchange(S);

   Fit();
   auto sz = GetSize();
   SetMinSize( sz );
   SetMaxSize( sz );

   // The close button has the cancel id and acts exactly the same as cancel.
   wxButton * pWin = (wxButton*)FindWindowById( wxID_CANCEL );
   if( pWin )
      pWin->SetFocus( );
   Center();
}

void QuickFixDialog::AddStuck( ShuttleGui & S, bool & bBool,
   const PrefSetter &prefSetter,
   const TranslatableString &Prompt, const ManualPageID &Help )
{
   mItem++;
   wxWindowID id = FixButtonID + mItem;
   if( !bBool)
      return;

   S
      .AddFixedText( Prompt );

   S
      .Id( id )
      .AddButton( XXO("Fix") )
         ->Bind( wxEVT_BUTTON, [this, prefSetter, id](wxCommandEvent&){
            OnFix( prefSetter, id );
         } );
}

void QuickFixDialog::PopulateOrExchange(ShuttleGui & S)
{

   S.StartVerticalLay(1);
   S.StartStatic( XO("Quick Fixes"));

   // These aren't all possible modes one can be stuck in, but they are some of them.
   bool bStuckInMode = mbSyncLocked || mbInSnapTo || mbSoundActivated;

   if( !bStuckInMode ){
      SetLabel(XO("Nothing to do"));
      S.AddFixedText(XO("No quick, easily fixed problems were found"));
   }
   else {
      S.StartMultiColumn(3, wxALIGN_CENTER);
      {
         mItem = -1;

         auto defaultAction =
         [](TenacityProject *pProject, const wxString &path){ return
            [pProject, path]{
               gPrefs->Write(path, 0);
               gPrefs->Flush();
               // This is overkill (aka slow), as all preferences are
               // reloaded and all
               // toolbars recreated.
               // Overkill probably doesn't matter, as this command is
               // infrequently used.
               DoReloadPreferences( *pProject );
            };
         };

         // Use # in the URLs to ensure we go to the online version of help.
         // Local help may well not be installed.
         auto pProject = &mProject;
         AddStuck( S, mbSyncLocked,
            defaultAction( pProject, "/GUI/SyncLockTracks" ),
            XO("Clocks on the Tracks"), "Quick_Fix#sync_lock" );
         AddStuck( S, mbInSnapTo,
            [pProject] {
               gPrefs->Write( "/SnapTo", 0 );
               gPrefs->Flush();
               // Sadly SnapTo has to be handled specially,
               // as it is not part of the standard
               // preference dialogs.
               ProjectSelectionManager::Get( *pProject ).AS_SetSnapTo( 0 );
            },
            XO("Can't select precisely"), "Quick_Fix#snap_to" );
         AddStuck( S, mbSoundActivated,
            defaultAction( pProject, "/AudioIO/SoundActivatedRecord" ),
            XO("Recording stops and starts"),
            "Quick_Fix#sound_activated_recording" );
      }
      S.EndMultiColumn();
   }
   S.EndStatic();

   S.StartHorizontalLay(wxALIGN_CENTER_HORIZONTAL, 0);
      S.AddStandardButtons(eCloseButton);
   S.EndHorizontalLay();

   S.EndVerticalLay();
}

void QuickFixDialog::OnOk(wxCommandEvent &event)
{
   (void)event;// Compiler food
   EndModal(wxID_OK);
}

void QuickFixDialog::OnCancel(wxCommandEvent &event)
{
   (void)event;// Compiler food
   EndModal(wxID_CANCEL);
}

void QuickFixDialog::OnFix(const PrefSetter &setter, wxWindowID id)
{
   if ( setter )
      setter();
   
   // Change the label after doing the fix, as the fix may take a second or two.
   auto pBtn = FindWindow(id);
   if( pBtn )
      pBtn->SetLabel( _("Fixed") );

   // The close button has the cancel id and acts exactly the same as cancel.
   wxButton * pWin = (wxButton*)FindWindowById( wxID_CANCEL );
   if( pWin )
      pWin->SetFocus( );
}

}

namespace HelpActions {

// exported helper functions

// Menu handler functions

struct Handler : CommandHandlerObject {

void OnQuickFix(const CommandContext &context)
{
   auto &project = context.project;
   QuickFixDialog dlg( &GetProjectFrame( project ), project );
   dlg.ShowModal();
}

void OnQuickHelp(const CommandContext &context)
{
   OpenInDefaultBrowser(L"https://tenacityaudio.org/#community-buttons");
}

void OnManual(const CommandContext &context)
{
   OpenInDefaultBrowser(L"https://tenacityaudio.org/docs");
}

void OnAudioDeviceInfo(const CommandContext &context)
{
   auto &project = context.project;
   auto gAudioIO = AudioIOBase::Get();
   wxString info = gAudioIO->GetDeviceInfo();
   ShowDiagnostics( project, info,
      XO("Audio Device Info"), wxT("deviceinfo.txt") );
}

#ifdef EXPERIMENTAL_MIDI_OUT
void OnMidiDeviceInfo(const CommandContext &context)
{
   auto &project = context.project;
   auto gAudioIO = AudioIOBase::Get();
   wxString info = gAudioIO->GetMidiDeviceInfo();
   ShowDiagnostics( project, info,
      XO("MIDI Device Info"), wxT("midideviceinfo.txt") );
}
#endif

void OnShowLog( const CommandContext &context )
{
   LogWindow::Show();
}

#ifdef IS_ALPHA
void OnSegfault(const CommandContext &)
{
   unsigned *p = nullptr;
   *p = 0xDEADBEEF;
}
   
void OnException(const CommandContext &)
{
   // Throw an exception that can be caught only as (...)
   // The intent is to exercise detection of unhandled exceptions by the
   // crash reporter
   struct Unique{};
   throw Unique{};
}
   
void OnAssertion(const CommandContext &)
{
   // We don't use assert() much directly, but Breakpad does detect it
   // This may crash the program only in debug builds
   // See also wxSetAssertHandler, and wxApp::OnAssertFailure()
   assert(false);
}
#endif

void OnMenuTree(const CommandContext &context)
{
   auto &project = context.project;
   
   using namespace MenuTable;
   struct MyVisitor : ToolbarMenuVisitor
   {
      using ToolbarMenuVisitor::ToolbarMenuVisitor;

      enum : unsigned { TAB = 3 };
      void DoBeginGroup( GroupItem &item, const Path& ) override
      {
         if ( dynamic_cast<MenuItem*>( &item ) ) {
            Indent();
            // using GET for alpha only diagnostic tool
            info += item.name.GET();
            Return();
            indentation = wxString{ ' ', TAB * ++level };
         }
      }

      void DoEndGroup( GroupItem &item, const Path& ) override
      {
         if ( dynamic_cast<MenuItem*>( &item ) )
            indentation = wxString{ ' ', TAB * --level };
      }

      void DoVisit( SingleItem &item, const Path& ) override
      {
         // using GET for alpha only diagnostic tool
         Indent();
         info += item.name.GET();
         Return();
      }

      void DoSeparator() override
      {
         static const wxString separatorName{ '=', 20 };
         Indent();
         info += separatorName;
         Return();
      }

      void Indent() { info += indentation; }
      void Return() { info += '\n'; }

      unsigned level{};
      wxString indentation;
      wxString info;
   } visitor{ project };

   MenuManager::Visit( visitor );

   ShowDiagnostics( project, visitor.info,
      Verbatim("Menu Tree"), wxT("menutree.txt"), true );
}

void OnAbout(const CommandContext &context)
{
#ifdef __WXMAC__
   // Modeless dialog, consistent with other Mac applications
   // Simulate the application Exit menu item
   wxCommandEvent evt{ wxEVT_MENU, wxID_ABOUT };
   wxTheApp->AddPendingEvent( evt );
#else
   auto &project = context.project;
   auto &window = GetProjectFrame( project );

   // Windows and Linux still modal.
   AboutDialog dlog( &window );
   dlog.ShowModal();
#endif
}

}; // struct Handler

} // namespace

static CommandHandlerObject &findCommandHandler(TenacityProject &) {
   // Handler is not stateful.  Doesn't need a factory registered with
   // TenacityProject.
   static HelpActions::Handler instance;
   return instance;
};

// Menu definitions

#define FN(X) (& HelpActions::Handler :: X)

namespace {
using namespace MenuTable;

// Now here we are to the horrid part: a VERY long, complex function...
// Note: you might not see any 'Check for updates...' function in the
// menu because I removed it
BaseItemSharedPtr HelpMenu()
{
   static BaseItemSharedPtr menu{
   ( FinderScope{ findCommandHandler },
    Menu(
        wxT("Help"),
        XXO("&Help"),
        Section( "Basic",
                // QuickFix menu item not in Audacity 2.3.1 whilst we discuss further.
                #ifdef EXPERIMENTAL_DA
                  // DA: Has QuickFix menu item.
                  Command( wxT("QuickFix"), XXO("&Quick Fix..."), FN(OnQuickFix), AlwaysEnabledFlag ),

                  // DA: 'Getting Started' rather than 'Quick Help'.
                  Command( wxT("QuickHelp"), XXO("&Getting Started"), FN(OnQuickHelp), AlwaysEnabledFlag ),

                  // DA: Emphasise it is the Audacity Manual (No separate DA manual).
                  Command( wxT("Manual"), XXO("Tenacity &Manual"), FN(OnManual), AlwaysEnabledFlag )
                #else
                  Command( wxT("QuickHelp"), XXO("&Quick Help..."), FN(OnQuickHelp), AlwaysEnabledFlag ),
                  Command( wxT("Manual"), XXO("&Manual..."), FN(OnManual), AlwaysEnabledFlag )
                #endif
               ),

              #ifdef __WXMAC__
                Items
              #else
                Section
              #endif
                    ("Other", Menu( wxT("Diagnostics"), XXO("&Diagnostics"),
                     Command( wxT("DeviceInfo"), XXO("Au&dio Device Info..."), FN(OnAudioDeviceInfo), AudioIONotBusyFlag() ),

              #ifdef EXPERIMENTAL_MIDI_OUT
                     Command( wxT("MidiDeviceInfo"), XXO("&MIDI Device Info..."), FN(OnMidiDeviceInfo), AudioIONotBusyFlag() ),
              #endif
                     Command( wxT("Log"), XXO("Show &Log..."), FN(OnShowLog), AlwaysEnabledFlag )

      #ifdef IS_ALPHA
                     , // we continue from up above to add more Command()s
                     // alpha-only items don't need to internationalize, so use
                     // Verbatim for labels

                     Command( wxT("RaiseSegfault"), Verbatim("Test segfault report"), FN(OnSegfault), AlwaysEnabledFlag ),

                     Command( wxT("ThrowException"), Verbatim("Test exception report"), FN(OnException), AlwaysEnabledFlag ),

                     Command( wxT("ViolateAssertion"), Verbatim("Test assertion report"), FN(OnAssertion), AlwaysEnabledFlag ),

                     // Menu explorer.  Perhaps this should become a macro command
                     Command( wxT("MenuTree"), Verbatim("Menu Tree..."), FN(OnMenuTree), AlwaysEnabledFlag )
      #endif
         ) // end of this Section()
      #ifndef __WXMAC__
         ),

         Section( "",
      #else
      ,
      #endif
              Command( wxT("About"), XXO("&About Tenacity..."), FN(OnAbout), AlwaysEnabledFlag )
             )
        )
     ) };
   return menu;
}

AttachedItem sAttachment1{
   wxT(""),
   Shared( HelpMenu() )
};

}

#undef FN
