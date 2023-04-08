/**********************************************************************

  Audacity: A Digital Audio Editor

  AudacityMessageBox.cpp

  Paul Licameli split this out of ErrorDialog.cpp

**********************************************************************/

#include "AudacityMessageBox.h"

// Tenacity libraries
#include <lib-strings/Internat.h>

#include <wx/textctrl.h>

#include "wxPanelWrapper.h"
#include "../shuttle/ShuttleGui.h"

void ShowInfoDialog( wxWindow *parent,
                     const TranslatableString &dlogTitle,
                     const TranslatableString &shortMsg,
                     const wxString &message,
                     const int xSize, const int ySize)
{
    wxDialogWrapper dlog(
        parent,
        wxID_ANY,
        dlogTitle,
        wxDefaultPosition, wxDefaultSize,
        wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER | wxMAXIMIZE_BOX /*| wxDEFAULT_FRAME_STYLE */
    );

    dlog.SetName();
    ShuttleGui S(&dlog, eIsCreating);

    S.StartVerticalLay(1);
    {
        S.AddTitle( shortMsg );
        S.Style( wxTE_MULTILINE | wxTE_READONLY | wxTE_RICH | wxTE_RICH2 |
                wxTE_AUTO_URL | wxTE_NOHIDESEL | wxHSCROLL )
            .AddTextWindow(message);

        S.SetBorder( 0 );
        S.StartHorizontalLay(wxALIGN_CENTER_HORIZONTAL, 0);
            S.AddStandardButtons(eOkButton);
        S.EndHorizontalLay();
    }
    S.EndVerticalLay();

    // Smallest size is half default size.  Seems reasonable.
    dlog.SetMinSize( wxSize(xSize/2, ySize/2) );
    dlog.SetSize( wxSize(xSize, ySize) );
    dlog.Center();
    dlog.ShowModal();
}
