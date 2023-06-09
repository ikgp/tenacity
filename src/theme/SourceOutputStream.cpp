/***********************************************************************

  Tenacity: A Digital Audio Editor

  SourceOutputStream.cpp

  Avery King split from Theme.cpp

  This file is licensed under the wxWidgets license, see License.txt

*//********************************************************************/

#include "SourceOutputStream.h"

/// Opens the file and also adds a standard comment at the start of it.
int SourceOutputStream::OpenFile(const FilePath & Filename)
{
   nBytes = 0;
   bool bOk;
   bOk = File.Open( Filename, wxFile::write );

   if( bOk )
   {
      File.Write( wxT("///   @file ThemeAsCeeCode.h\n") );
      File.Write( wxT("///   @brief This file was Auto-Generated by Tenacity.\n") );
      File.Write( wxT("///\n") );
      File.Write( wxT("///   It is included by Theme.cpp.\n") );
      File.Write( wxT("///   Only check this into Git if you've read and understood the guidelines!\n\n") );
   }

   return bOk;
}

/// This is the 'callback' function called with each write of PNG data
/// to the stream.  This is where we conveet to text and add commas.
size_t SourceOutputStream::OnSysWrite(const void *buffer, size_t bufsize)
{
   wxString Temp;
   for (int i = 0; i < (int)bufsize ; i++)
   {
      // Write one byte with a comma
      Temp = wxString::Format( wxT("%i,"),(int)(((unsigned char*)buffer)[i]) );
      File.Write( Temp );
      nBytes++;
      // New line if more than 20 bytes written since last time.
      if( (nBytes % 20) == 0 )
      {
         File.Write( wxT("\n   "));
      }
   }

   return bufsize;
}

/// Destructor.  We close our text stream in here.
SourceOutputStream::~SourceOutputStream()
{
   File.Write( wxT("\n") );
   File.Close();
}
