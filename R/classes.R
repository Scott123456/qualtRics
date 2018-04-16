# Helpers and classes

# CLASSES ----

# ------------------------------------- #
# QualtricsMetadata class & constructor #
# ------------------------------------- #

.QualtricsMetadata <- setClass(
  # Name
  "QualtricsMetadata",
  # Slots
  slots = c(
    survey_id = "character",
    survey_name = "character",
    owner = "list",
    is_active = "logical",
    created = "character", # to date time object
    modified = "character", # to date time object
    expires = "list", # to date time object
    questions = "list",
    export_column_map = "list",
    blocks = "list",
    flow = "list",
    embedded_data = "list",
    comments = "list",
    loop_and_merge = "list",
    response_counts = "data.frame"
  )
)

# Constructor
QualtricsMetadata <- function(data) {

  # Get data needed to populate class

  # SURVEY ID ----
  survey_id <- data$result$id
  # SURVEY NAME ----
  survey_name <- data$result$name
  # OWNER & ORG ID ----
  owner <- list(
    "owner_id" = data$result$ownerId,
    "organization_id" = data$result$organizationId
  )
  # IS ACTIVE? ----
  is_active <- data$result$isActive
  # CREATED ----
  created <- data$result$creationDate
  # MODIFIED ----
  modified <- data$result$lastModifiedDate
  # EXPIRES ----
  expires <- data$result$expiration
  # QUESTIONS ----
  questions <- data$result$questions
  # Get names
  nams <- names(questions)
  # Assign class
  questions_classed <- lapply(nams, function(x) {

    # Data & name
    tmp_data <- questions[[x]]
    identifier <- x

    # Assign class
    qualtrics_helper_assign_question_class(tmp_data, identifier)

  })
  # EXPORT COLUMN MAP ----
  export_column_map <- data$result$exportColumnMap
  # BLOCKS ----
  blocks <- data$result$blocks
  # FLOW ----
  flow <- data$result$flow
  # EMBEDDED DATA ----
  embedded_data <- data$result$embeddedData
  # COMMENTS ----
  comments <- data$result$comments
  # LOOP AND MERGE ----
  loop_and_merge <- data$result$loopAndMerge
  # RESPONSE COUNTS ----
  response_counts <- as.data.frame(data$result$responseCounts)


  # Build object and return
  .QualtricsMetadata(survey_id = survey_id,
                     survey_name = survey_name,
                     owner = owner,
                     is_active = is_active,
                     created = created,
                     modified = modified,
                     expires = expires,
                     questions = questions_classed,
                     export_column_map = export_column_map,
                     blocks = blocks,
                     flow = flow,
                     embedded_data = embedded_data,
                     comments = comments,
                     loop_and_merge = loop_and_merge,
                     response_counts = response_counts)

}

# ------------------------------------------- #
# QualtricsSurveyQuestion class & constructor #
# ------------------------------------------- #

# Qualtrics survey question class
.QualtricsSurveyQuestion <- setClass(
  # Name
  "QualtricsSurveyQuestion",
  # data
  slots = c(
    name = "character",
    identifier = "character",
    type = "character",
    selectors = "list",
    question_text = "character",
    question_label = "character",
    validation = "list"
  )
)

# Constructor
QualtricsSurveyQuestion <- function(data, identifier) {

  # Collect data

  # NAME OF SURVEY ----
  name <- ifelse(is.null(data$questionName),
                 "", data$questionName)
  # IDENTIFIER ----
  identifier <- ifelse(is.null(identifier), "", identifier)
  # TYPE ----
  type <- ifelse(is.null(data$questionType$type),
                 "", data$questionType$type)
  # SELECTORS ----
  selectors <- list(
    selector = ifelse(is.null(data$questionType$selector),
                      "", data$questionType$selector),
    sub_selector = ifelse(is.null(data$questionType$subSelector),
                          "", data$questionType$subSelector)
  )
  # QUESTION TEXT ----
  question_text <- ifelse(is.null(data$questionText),
                          "", data$questionText)
  # QUESTION LABEL ----
  question_label <- ifelse(is.null(data$questionLabel),
                           "", data$questionLabel)
  # VALIDATION ----
  validation <- ifelse(is.null(data$validation), list(), data$validation)

  # To class
  cl <- .QualtricsSurveyQuestion(name = name,
                                 identifier = identifier,
                                 type = type,
                                 selectors = selectors,
                                 question_text = question_text,
                                 question_label = question_label,
                                 validation = validation)

  # Return
  return(cl)
}

# ---------------------------------- #
# MultipleChoice class & constructor #
# ---------------------------------- #

# Multiple choice
.MultipleChoice <- setClass(
  # Name
  "MultipleChoice",
  # data
  slots = c(
    choices = "character",
    descriptions = "character",
    choice_texts = "character",
    image_descriptions = "character",
    variable_names = "character",
    analyze = "logical"
  ),
  # Inherits
  contains = "QualtricsSurveyQuestion"
)

# Constructor
MultipleChoice <- function(data, identifier) {

  # Call QualtricsSurveyQuestion constructor
  QSQ <- QualtricsSurveyQuestion(data, identifier)

  # Prep data for MC constructor

  # CHOICES ----
  choices <- names(data$choices)
  # DESCRIPTIONS ----
  descriptions <- vapply(data$choices, function(x) x$description,
                         "character")
  # CHOICE TEXTS ----
  choice_texts <- vapply(data$choices, function(x) x$choiceText,
                         "character")
  # IMAGE DESCRIPTIONS ----
  image_descriptions <- tryCatch({
    vapply(data$choices, function(x) x$imageDescription, "character")
  }, error = function(e) {
    ""
  })
  # VARIABLE NAMES ----
  variable_names <- tryCatch({
    vapply(data$choices, function(x) x$variableName, "character")
  }, error = function(e) {
    ""
  })
  # ANALYZE ----
  analyze <- vapply(data$choice, function(x) x$analyze,
                    TRUE)


  # Create an MC object
  .MultipleChoice(QSQ,
                  choices = choices,
                  descriptions = descriptions,
                  choice_texts = choice_texts,
                  image_descriptions = image_descriptions,
                  variable_names = variable_names,
                  analyze=analyze)

}

# ------------------------------------- #
# SurveyInstruction class & constructor #
# ------------------------------------- #

# Survey instruction
.SurveyInstruction <- setClass(
  # Name
  "SurveyInstruction",
  # Inherits
  contains = "QualtricsSurveyQuestion"
)

# Constructor
SurveyInstruction <- function(data, identifier) {

  # Call QualtricsSurveyQuestion constructor
  QSQ <- QualtricsSurveyQuestion(data, identifier)

  # Create an SI object
  .SurveyInstruction(QSQ)

}


# METHODS ----

# ----------------------------------- #
# QualtricsMetadata methods & helpers #
# ----------------------------------- #

# Getters
setGeneric("questions", function(x) setGeneric("questions"))
setMethod("questions", "QualtricsMetadata", function(x) x@questions)

# Print method
setMethod("show", signature = "QualtricsMetadata",
          function(object) {
            mes <- paste0(
              is(object)[[1]], " object.\n\n",
              " Survey name: \t", slot(object, "survey_name"), "\n",
              " Active: \t",
              ifelse(slot(object, "is_active") == TRUE, "active", "inactive"),"\n",
              " Questions: \t", length(slot(object, "questions")),"\n",
              " Responses: \t", object@response_counts$auditable
            )
            cat(mes)
          })

# Plot method
setMethod(f = "plot",
          signature = "QualtricsMetadata",
          definition = function(x, y, type = c("questions", "flow", "columns"),
                                ...) {

            # Type
            type <- match.arg(type)
            # If flow or columns
            if(type == "flow" | type == "columns") {
              warning("Not yet implemented\n")
              return(NULL)
            }

            # Plot
            object <- x
            # Questions
            q <- questions(object)
            # Types
            types <- unlist(lapply(q, function(x) type(x)))
            # To data frame
            df <- dplyr::data_frame(
              type = types
            ) %>%
              dplyr::group_by(type) %>%
              dplyr::tally() %>%
              dplyr::arrange(dplyr::desc(n))
            # Plot
            ggplot(df, aes(x = type, y= n)) +
              geom_bar(stat = "identity") +
              theme_bw()

          })

# Helper function. Basically a switch to assign proper question class.
# Called from QualtricsMetadata constructor
qualtrics_helper_assign_question_class <- function(question_data, identifier) {

  # Get type
  type <- question_data$questionType$type

  #browser()
  # Switch
  switch(
    type,
    "MC" = MultipleChoice(question_data, identifier = identifier),
    "DB" = SurveyInstruction(question_data, identifier = identifier),
    QualtricsSurveyQuestion(question_data, identifier)
  )

}

# ----------------------------------------- #
# QualtricsSurveyQuestion methods & helpers #
# ----------------------------------------- #

# Getter methods
setGeneric("type", function(x) setGeneric("type"))
setMethod("type", "QualtricsSurveyQuestion", function(x) x@type)

# Show method
setMethod("show", "QualtricsSurveyQuestion",
          function(object) {
            msg <- paste0(
              is(object)[[1]], " object.\n\n",
              " Name: \t\t\t", object@name, "\n",
              " Identifier: \t\t", object@identifier, "\n",
              " Type: \t\t\t", object@type, "\n",
              " Question text: \t", object@question_text, "\n"
            )
            cat(msg)
          })


