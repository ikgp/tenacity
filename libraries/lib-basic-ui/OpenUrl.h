/*!********************************************************************

  Tenacity

  @file OpenUrl.h
  @brief Abstractions for URL handling.

  Avery King

**********************************************************************/

#include <string>

#pragma once

namespace GenericUI
{

/// @brief Abstract class for URL handling services (e.g., opening URLs in the
/// user's default browser).
class UrlServices
{
    public:
        virtual void DoOpenUrl(const std::string url) = 0;
};

/// @brief Installs a URL service handler.
/// @param services The URL services handler to install.
/// @return The previously installed instance
UrlServices* BASIC_UI_API InstallUrlServices(UrlServices* services);

/// @brief Abstraction to open a URL in the user's default browser.
/// @param url The URL to open.
void BASIC_UI_API OpenUrl(const std::string url);

} // namespace GenericUI
