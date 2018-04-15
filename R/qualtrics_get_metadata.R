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

#' Download Metadata for a Survey
#'
#' Using this function, you can retrieve metadata about your survey. This information includes question metadata (type, options, choices etc), number of responses, general metadata, survey flow etc.
#'
#' @param surveyID String. Unique ID for the survey you want to download. Returned as 'id' by the \link[qualtRics]{getSurveys} function.
#'
#' @author Jasper Ginn
#' @importFrom assertthat assert_that
#' @export
#' @examples
#' \dontrun{
#' # Register your Qualtrics credentials if you haven't already
#' # Note that you need to pass both the 'api_token' and 'root_url'
#' # parameters if you call this function for the first time.
#' registerOptions(api_token = "<YOUR-API-TOKEN>",
#'                 base_url = "<YOUR-ROOT-URL>")
#' # Get an overview of surveys
#' surveys <- getSurveys()
#' # Get metadata for a survey
#' md <- metadata(surveyID = surveys$id[6])
#' # Get metadata with specific elements
#' md_specific <- metadata(surveyID= id, get=list(questions = FALSE, flow = TRUE))
#' # Get specific question metadata
#' question_specific <- metadata(surveyID=id,
#'                               get=list(questions = TRUE),
#'                                        questions = c("Q1", "Q2"))
#'
#' # Example of a metadata file
#' file <- system.file("extdata", "metadata.rds", package = "qualtRics")
#' # Load
#' metadata_ex <- readRDS(file=file)
#' }
#'

qualtrics_get_metadata <- function(surveyID) {

  # OPTIONS AND PREP ----

  # Check params
  cp <- checkParams()

  # QUERY API ----

  # Function-specific API stuff
  root_url <- appendRootUrl(Sys.getenv("QUALTRICS_ROOT_URL"), "surveys")
  # Append survey ID
  root_url <- paste0(root_url, surveyID)
  # Send GET request to list all surveys
  resp <- qualtricsApiRequest("GET", root_url)

  # TO CLASS AND RETURN ----

  QualtricsMetadata(resp)


}
