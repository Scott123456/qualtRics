# These are deprecated and will be removed from version 4.0 onwards

#' @rdname qualtrics_register_options
#' @export
registerOptions <- function(verbose=TRUE,
                            useLabels=TRUE,
                            convertVariables=TRUE,
                            useLocalTime=FALSE,
                            dateWarning=TRUE,
                            ...) {
  # Warning message
  warning("'registerOptions' is deprecated and will be removed in version 4.0 of qualtRics.\nPlease use 'qualtrics_register_options' instead.\n")
  # Call new function
  qualtrics_register_options(verbose = verbose,
                             useLabels = useLabels,
                             convertVariables = convertVariables,
                             useLocalTime = useLocalTime,
                             dateWarning = dateWarning,
                             ...)
}

#' @rdname qualtrics_get_survey
#' @export
getSurvey <- function(surveyID,
                      lastResponseId=NULL,
                      startDate=NULL,
                      endDate=NULL,
                      seenUnansweredRecode=NULL,
                      limit = NULL,
                      includedQuestionIds = NULL,
                      saveDir=NULL,
                      forceRequest=FALSE,
                      ...) {
  # Warning message
  warning("'getSurvey' is deprecated and will be removed in version 4.0 of qualtRics.\nPlease use 'qualtrics_get_survey' instead.\n")
  # Call new function
  qualtrics_get_survey(surveyID = surveyID,
                       lastResponseId=lastResponseId,
                       startDate=startDate,
                       endDate=endDate,
                       seenUnansweredRecode=seenUnansweredRecode,
                       limit = limit,
                       includedQuestionIds = includedQuestionIds,
                       saveDir = saveDir,
                       forceRequest = forceRequest,
                       ...)
}

#' @rdname qualtrics_get_surveys
#' @export
getSurveys <- function() {
  # Warning message
  warning("'getSurveys' is deprecated and will be removed in version 4.0 of qualtRics.\nPlease use 'qualtrics_get_surveys' instead.\n")
  # Call new function
  qualtrics_get_surveys()
}

#' @rdname qualtrics_read_survey
#' @export
readSurvey <- function(file_name,
                       stripHTML = TRUE,
                       legacyFormat = FALSE) {
  # Warning message
  warning("'readSurvey' is deprecated and will be removed in version 4.0 of qualtRics.\nPlease use 'qualtrics_read_survey' instead.\n")
  # Call new function
  qualtrics_read_survey(file_name = file_name,
                        stripHTML = stripHTML,
                        legacyFormat = legacyFormat)
}

#' @rdname qualtrics_config_file
#' @export
qualtRicsConfigFile <- function(api_token = NULL,
                                base_url=NULL,
                                verbose=TRUE,
                                useLabels=TRUE,
                                convertVariables=TRUE,
                                useLocalTime=FALSE,
                                dateWarning=TRUE,
                                root_url = NULL) {
  # Warning message
  warning("'qualtRicsConfigFile' is deprecated and will be removed in version 4.0 of qualtRics.\nPlease use 'qualtrics_config_file' instead.\n")
  # Call new function
  qualtrics_config_file(api_token = api_token,
                        base_url=base_url,
                        verbose=verbose,
                        useLabels=useLabels,
                        convertVariables=convertVariables,
                        useLocalTime=useLocalTime,
                        dateWarning=dateWarning,
                        root_url = root_url)
}

#' @rdname qualtrics_get_metadata
#' @export
metadata <- function(surveyID) {

  # Warning message
  warning("'metadata' is deprecated and will be removed in version 4.0 of qualtRics.\nPlease use 'qualtrics_get_metadata' instead.\n")

  # Call new function
  qualtrics_get_metadata(surveyID)

}
