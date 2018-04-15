#   Download qualtrics data into R
#    Copyright (C) 2018 Jasper Ginn

#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.

#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.

#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>

# Loads qualtRics credentials automatically when package is loaded
# and ".qualtRics.yml" file is present in working directory. User
# needs to have #qualtRics API key and root url stored in a configuration
# file in working directory. For an example of a configuration file,
# execute "qualtRicsCo#nfigFile()". See:
# https://github.com/ropensci/qualtRics/blob/master/README.md#using-a-configuration-file # nolint


.onLoad <- function(libname = find.package("qualtRics"), pkgname="qualtRics") {

  # If base url and api token loaded at R startup
  if(all(Sys.getenv("QUALTRICS_API_KEY") != "" & Sys.getenv("QUALTRICS_ROOT_URL") != "")) {
    if(!file.exists(".qualtRics.yml")) {
      packageStartupMessage("Found qualtrics api token & qualtrics base url in .Rprofile. Using these credentials.\n")
    }
  }

  # Override other options if .qualtRics.yml exists
  if(file.exists(".qualtRics.yml")) {
    # Erase these values
    Sys.setenv("QUALTRICS_ROOT_URL" = "")
    Sys.setenv("QUALTRICS_API_KEY" = "")
    # load 'registeroptions()'
    suppressWarnings(registerOptions())
  }

  # Retrieve data using keyringr
  get_os <- function(){
    sysinf <- Sys.info()
    if (!is.null(sysinf)){
      os <- sysinf['sysname']
      if (os == 'Darwin')
        os <- "osx"
    } else { ## mystery machine
      os <- .Platform$OS.type
      if (grepl("^darwin", R.version$os))
        os <- "osx"
      if (grepl("linux-gnu", R.version$os))
        os <- "linux"
    }
    tolower(os)
  }

  # Get OS
  os <- get_os()
  # Api & root url
  api_token_kc <- switch(
    os,
    "osx" = keyringr::decrypt_kc_pw("qualtrics_api_token"),
    "linux" = keyringr::decrypt_gk_pw("qualtrics_api_token")
  )
  base_url_kc <- switch(
    os,
    "osx" = keyringr::decrypt_kc_pw("qualtrics_base_url"),
    "linux" = keyringr::decrypt_gk_pw("qualtrics_base_url")
  )

  # If both exist, register
  if(!is.null(api_token_kc) & !is.null(base_url_kc)) {
    packageStartupMessage("Found a qualtrics base url & api token in the keychain. Using these credentials. You may need to authenticate.")
    registerOptions(api_token = api_token_kc, base_url = base_url_kc)
  }

  # Set internal qualtRics settings
  options(
    "QUALTRICS_INTERNAL_SETTINGS" = list("question_types_supported" =
                                           list("type"=c("MC"),
                                                "selector"=c("SAVR"),
                                                "subSelector"=c("TX"))
                                         )
  )

}

# On unload
.onUnload <- function(libname = find.package("qualtRics"), pkgname="qualtRics") {

  # If user unloads/detaches package make sure that these values are erased
  Sys.setenv("QUALTRICS_ROOT_URL" = "")
  Sys.setenv("QUALTRICS_API_KEY" = "")

}
