/*!********************************************************************

  Tenacity

  @file OpenUrl.cpp
  @brief Abstractions for URL handling.

  Avery King

**********************************************************************/

#include "OpenUrl.h"

namespace
{

GenericUI::UrlServices* urlServicesHandler = nullptr;

}

namespace GenericUI
{

UrlServices* InstallUrlServices(UrlServices* services)
{
    auto previousHandler = urlServicesHandler;
    urlServicesHandler = services;
    return previousHandler;
}

void OpenUrl(const std::string url)
{
    urlServicesHandler->DoOpenUrl(url);
}

} // namespace GenericUI
