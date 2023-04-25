/**********************************************************************

  Audacity: A Digital Audio Editor

  AudacityMessageBox.h

  Paul Licameli split this out of ErrorDialog.h

**********************************************************************/

#ifndef __AUDACITY_MESSAGE_BOX__
#define __AUDACITY_MESSAGE_BOX__

#include <wx/msgdlg.h>

// Tenacity libraries
#include <lib-strings/Internat.h>

extern TENACITY_DLL_API TranslatableString AudacityMessageBoxCaptionStr();

// Do not use wxMessageBox!!  Its default window title does not translate!
inline int AudacityMessageBox(const TranslatableString& message,
   const TranslatableString& caption = XO("Message"),
   long style = wxOK | wxCENTRE,
   wxWindow *parent = NULL,
   int x = wxDefaultCoord, int y = wxDefaultCoord)
{
   return ::wxMessageBox(message.Translation(), caption.Translation(),
      style, parent, x, y);
}

/// Mostly we use this so that we have the code for resizability
/// in one place.  Other considerations like screen readers are also
/// handled by having the code in one place.
void ShowInfoDialog( wxWindow *parent,
                     const TranslatableString &dlogTitle,
                     const TranslatableString &shortMsg,
                     const wxString &message,
                     const int xSize, const int ySize);

#endif
