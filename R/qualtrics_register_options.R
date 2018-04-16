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
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' Register Qualtrics API Key, Base Url and Other Options
#'
#' This function registers the user's qualtrics API key, base url and other options for the remainder of the R session. This function only needs to be called once (at the beginning of each R session). You may also use a configuration file. See \code{\link{qualtRicsConfigFile}} or \url{https://github.com/ropensci/qualtRics/blob/master/README.md#using-a-configuration-file}
#' . Note that you must pass both an api token and a base url if you call this function for the first time in a session and you're not using a config file. Thereafter, you can pass these options individually.
#'
#' @param verbose Logical. If TRUE, verbose messages will be printed to the R console. Defaults to TRUE.
#' @param useLabels Logical. TRUE to export survey responses as Choice Text or FALSE to export survey responses as values.
#' @param convertVariables Logical. If TRUE, then the \code{\link[qualtRics]{getSurvey}} function will convert certain question types (e.g. multiple choice) to proper data type in R. Defaults to TRUE.
#' @param useLocalTime Logical. Use local timezone to determine response date values? Defaults to FALSE. See \url{https://api.qualtrics.com/docs/dates-and-times} for more information.
#' @param dateWarning Logical. Once per session, qualtRics will emit a warning about date conversion for surveys. You can turn this warning off by changing the flag to FALSE. Defaults to TRUE.
#' @param ... Either one or both of 'api_token' and 'base_url' to register the Qualtrics api key and institution-specific root url manually. (see example). See also \code{\link{qualtRicsConfigFile}} for an explanation of the root_url and api_token parameters.
#'
#' @seealso See \url{https://github.com/ropensci/qualtRics/blob/master/README.md#using-a-configuration-file} for more information about the qualtRics configuration file. See: \url{https://api.qualtrics.com/docs/authentication} to find your Qualtrics API key and \url{https://api.qualtrics.com/docs/root-url} for more information about the institution-specific root url.
#'
#' @author Jasper Ginn
#' @importFrom yaml yaml.load_file
#' @importFrom assertthat assert_that
#' @importFrom assertthat is.flag
#' @export
#' @examples
#' \dontrun{
#' # Register your Qualtrics credentials if you haven't already
#' # Note that you need to pass both the 'api_token' and 'base_url'
#' # parameters if you call this function for the first time.
#' registerOptions(api_token = "<YOUR-API-TOKEN>",
#'                 base_url = "<YOUR-ROOT-URL>")
#' # Register a different root url
#' registerOptions(base_url = "<YOUR-OTHER-ROOT-URL>")
#' # Retrieve a list of surveys
#' surveys <- getSurveys()
#' # Retrieve a single survey
#' mysurvey <- getSurvey(surveyID = surveys$id[6],
#'                       saveDir = tempdir(),
#'                       verbose = TRUE)
#' # You can use the same parameters as those found in the qualtrics API documentation
#' # Found here: https://api.qualtrics.com/docs/csv
#' mysurvey <- getSurvey(surveyID = surveys$id[6],
#'                       saveDir = tempdir(),
#'                       startDate = "2017-01-01",
#'                       endDate = "2017-01-31",
#'                       limit = 100,
#'                       seenUnansweredRecode = "UNANS")
#' }
#'

qualtrics_register_options <- function(verbose=TRUE,
                                       useLabels=TRUE,
                                       convertVariables=TRUE,
                                       useLocalTime=FALSE,
                                       dateWarning=TRUE,
                                       api_key = NULL,
                                       base_url = NULL,
                                       ...) {

  # START UP: CHECK ARGUMENTS PASSED BY USER ----

  # Parse call
  user_call <- names(vapply(match.call(), deparse, "character"))[-1]
  # Check for base url and api key
  if(length(user_call) == 0) {
    api_key <- NULL
    root_url <- NULL
  }
  if(!"api_key" %in% user_call) {
    api_key <- NULL
  }
  if(!("base_url" %in% user_call | "root_url" %in% user_call)) {
    root_url <- NULL
  }

  opts <- list(...)
  # Check if deprecated params passed
  if("root_url" %in% names(opts)) {
    # Cannot pass both root url and base url
    if(!is.null(base_url)) {
      stop("Cannot pass both base_url and root_url. Please use 'base_url' only.")
    }
    warning("'root_url' is deprecated and will be removed in qualtRics 4.0. Please use 'base_url' instead.")
    # Save to new param
    root_url <- opts$root_url
  }
  # If not NULL
  if(!is.null(base_url)) {
    root_url <- base_url
  }

  # If purge all credentials
  if("reset_credentials" %in% names(opts)) {

    assertthat::is.flag(opts$reset_credentials)

    if(opts$reset_credentials) {

      # Reset credentials
      Sys.setenv("QUALTRICS_API_KEY" = "")
      Sys.setenv("QUALTRICS_ROOT_URL" = "")

    }

  }

  # REGISTER BASE URL AND API TOKEN -----

  # Both api key and base url are already passed (e.g. via .Rprofile)
  api_key_present <- ifelse(Sys.getenv("QUALTRICS_API_KEY") != "", TRUE, FALSE)
  base_url_present <- ifelse(Sys.getenv("QUALTRICS_ROOT_URL") != "", TRUE, FALSE)

  # If both are present and either or both API key & base url are passed
  if(all(c(api_key_present, base_url_present))) {
    if(!is.null(api_key)) Sys.setenv("QUALTRICS_API_KEY" = api_key)
    if(!is.null(root_url)) Sys.setenv("QUALTRICS_ROOT_URL" = root_url)
  }
  # Else, check if registered in keychain
  if(!all(c(api_key_present, base_url_present))) {

    # Option 1: check if user passed to function
    if(!is.null(root_url) & !is.null(api_key)) {

      Sys.setenv("QUALTRICS_API_KEY" = api_key)
      Sys.setenv("QUALTRICS_ROOT_URL" = root_url)

    } else {

    # Option 2: check keychain

      message("Found qualtrics credentials in keychain. Using these credentials.")

      cred <- qualtrics_helper_keychain_credentials()

      # If api key or root url not NULL, update
      if(!is.null(api_key)) cred$key <- api_key
      if(!is.null(root_url)) cred$url <- root_url

      # If credentials not empty, register
      if(!length(cred$key) == 0 | !length(cred$url) == 0) {

        Sys.setenv("QUALTRICS_API_KEY" = cred$key)
        Sys.setenv("QUALTRICS_ROOT_URL" = cred$url)

      } else {

    # Option 3: check local file if exists

        conf_file_exists <- file.exists(".qualtRics.yml")

        if(conf_file_exists) {

           cred <- qualtrics_helper_read_config_file()

           # If api key or root url not NULL, update
           if(!is.null(api_key)) cred$key <- api_key
           if(!is.null(root_url)) cred$url <- root_url

           # Set
           Sys.setenv("QUALTRICS_API_KEY" = cred$key)
           Sys.setenv("QUALTRICS_ROOT_URL" = cred$url)

           # Set options
           verbose <- cred$verbose
           useLabels <- cred$useLabels
           dateWarning <- cred$dateWarning
           useLocalTime <- cred$useLocalTime
           convertVariables <- cred$convertVariables

        }

      }

    }

  }

  # IF STILL NOT BOTH SET THROW ERROR ----

  assertthat::assert_that(Sys.getenv("QUALTRICS_ROOT_URL") != "", msg="'root_url' parameter must either be specified in the .qualtRics.yml configuration file\nor passed to the 'registerOptions' function. To view an example of a configuration file, execute\n'qualtRicsConfigFile()'.") # nolint
  assertthat::assert_that(Sys.getenv("QUALTRICS_API_KEY") != "", msg="'api_token' parameter must either be specified in the .qualtRics.yml configuration file\nor passed to the 'registerOptions' function. To view an example of a configuration file, execute\n'qualtRicsConfigFile()'.") # nolint


  # SET OTHER OPTIONS -----

  # Check options
  assertthat::assert_that(assertthat::is.flag(verbose),
                          msg=paste0("'verbose' must be either TRUE or FALSE.")) # nolint
  assertthat::assert_that(assertthat::is.flag(convertVariables),
                          msg=paste0("'convertvariables' must be either TRUE or FALSE.")) # nolint
  assertthat::assert_that(assertthat::is.flag(useLabels),
                          msg=paste0("'uselabels' must be either TRUE or FALSE.")) # nolint
  assertthat::assert_that(assertthat::is.flag(useLabels),
                          msg=paste0("'uselabels' must be either TRUE or FALSE.")) # nolint
  assertthat::assert_that(assertthat::is.flag(dateWarning),
                          msg=paste0("'dateWarning' must be either TRUE or FALSE.")) # nolint

  # Set options
  options(
    "QUALTRICS_VERBOSE" = verbose,
    "QUALTRICS_USELABELS" = useLabels,
    "QUALTRICS_CONVERTVARIABLES" = convertVariables,
    "QUALTRICS_USELOCALTIME" = useLocalTime
  )

  # Set warning
  invisible(ifelse(dateWarning == FALSE,
                   Sys.setenv("QUALTRICS_WARNING_DATE_GIVEN"=TRUE),
                   TRUE))

}

# Helper function. Retrieves API credentials from keychain
qualtrics_helper_keychain_credentials <- function() {

  # Helper function to determine type of OS
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

  # Return
  return(
    list(
      "key" = api_token_kc,
      "url" = base_url_kc
    )
  )

}

# Helper function. Reads .qualtRics.yml config file
qualtrics_helper_read_config_file <- function() {

  # Load file
  cred <- yaml::yaml.load_file(".qualtRics.yml")

  # Assert that names are "api_token" and "root_url"
  assertthat::assert_that((all(c("api_token", "root_url") %in% names(cred))) |
                            (all(c("api_token", "base_url") %in% names(cred))),
                          msg="Either the 'api_token' or 'base_url' arguments are missing in your .qualtRics.yml\nconfiguration file. Execute 'qualtRicsConfigFile()' to view an example of the configuration file.\nExecute 'file.edit('.qualtRics.yml')' to edit your configuration file.") # nolint

  # If verbose, print message
  if(verbose) message(paste0("Found a .qualtRics.yml configuration file in ",
                             getwd(), ". Using these credentials.")) # nolint

  # Set vars
  api_token <- cred$api_token
  # Check if deprecated params passed
  if(any("root_url" %in% names(cred))) {
    message("\nWarning: 'root_url' is deprecated and will be removed in qualtRics 4.0. Please use 'base_url' instead.")
    # Save to new param
    root_url <- cred$root_url
  } else if("base_url" %in% names(cred)) {
    root_url <- cred$base_url
  }

  # Set optional vars
  if("verbose" %in% names(cred)) {
    verbose <- cred$verbose
    assertthat::assert_that(assertthat::is.flag(verbose),
                            msg=paste0("'verbose' must be either TRUE or FALSE but is ",
                                       as.character(verbose), " in your config file.")) # nolint
  }

  # If 'convertStandardColumns' is found in credentials then emit a warning
  if('convertstandardcolumns' %in% names(cred) & !'convertvariables' %in% names(cred)) { # nolint
    message("'convertstandardcolumns' has been deprecated and will be ignored. Please replace it\nwith 'convertvariables' in your '.qualtRics.yml' file. Visit <https://github.com/ropensci/qualtRics>\nfor more information.") # nolint
    convertVariables <- TRUE
  } else if(all(c('convertstandardcolumns', 'convertvariables') %in% names(cred))) { # nolint
    message("'convertstandardcolumns' has been deprecated and will be ignored. Please remove it\nfrom your '.qualtRics.yml' file. Visit <https://github.com/ropensci/qualtRics> for\nmore information.") # nolint
    convertVariables <- cred$convertvariables
  } else {
    convertVariables <- cred$convertvariables
  }

  # Check if variables are correct types
  assertthat::assert_that(assertthat::is.flag(convertVariables),
                          msg=paste0("'convertvariables' must be either TRUE or FALSE but is ", # nolint
                                     as.character(convertVariables),
                                     " in your config file."))

  if("uselabels" %in% names(cred)) {
    useLabels <- cred$uselabels
    assertthat::assert_that(assertthat::is.flag(useLabels),
                            msg=paste0("'uselabels' must be either TRUE or FALSE but is ",
                                       as.character(useLabels), " in your config file.")) # nolint
  }

  if("uselocaltime" %in% names(cred)) {
    useLocalTime <- cred$uselocaltime
    assertthat::assert_that(assertthat::is.flag(useLocalTime),
                            msg=paste0("'useLocalTime' must be either TRUE or FALSE but is ",
                            as.character(useLocalTime), " in your config file.")) # nolint
  }

  if("datewarning" %in% names(cred)) {
    dateWarning <- cred$datewarning
    assertthat::assert_that(assertthat::is.flag(dateWarning),
                            msg=paste0("'dateWarning' must be either TRUE or FALSE but is ",
                                       as.character(dateWarning), " in your config file.")) # nolint
  }

  return(
    list(
      key = api_token,
      url = root_url,
      verbose = verbose,
      useLabels = useLabels,
      dateWarning = dateWarning,
      useLocalTime = useLocalTime,
      convertVariables = convertVariables
    )
  )

}
