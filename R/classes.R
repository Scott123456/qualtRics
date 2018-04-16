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

# ----------------------------------- #
# DescriptiveText class & constructor #
# ----------------------------------- #

# Survey instruction
.DescriptiveText <- setClass(
  # Name
  "DescriptiveText",
  # Inherits
  contains = "QualtricsSurveyQuestion"
)

# Constructor
DescriptiveText <- function(data, identifier) {

  # Call QualtricsSurveyQuestion constructor
  QSQ <- QualtricsSurveyQuestion(data, identifier)

  # Create an SI object
  .DescriptiveText(QSQ)

}

# ------------------------------- #
# TextGraphic class & constructor #
# ------------------------------- #

# Survey instruction
.TextGraphic <- setClass(
  # Name
  "TextGraphic",
  # Inherits
  contains = c("DescriptiveText", "QualtricsSurveyQuestion")
)

# Constructor
TextGraphic <- function(data, identifier) {

  # Call QualtricsSurveyQuestion constructor
  QSQ <- QualtricsSurveyQuestion(data, identifier)

  # Create an object
  .TextGraphic(QSQ)

}

# ------------------------------- #
# MatrixTable class & constructor #
# ------------------------------- #

# Survey instruction
.MatrixTable <- setClass(
  # Name
  "MatrixTable",
  # Inherits
  contains = "QualtricsSurveyQuestion"
)

# Constructor
MatrixTable <- function(data, identifier) {

  # Call QualtricsSurveyQuestion constructor
  QSQ <- QualtricsSurveyQuestion(data, identifier)

  # Create an object
  .MatrixTable(QSQ)

}

# ----------------------------- #
# TextEntry class & constructor #
# ----------------------------- #

# Survey instruction
.TextEntry <- setClass(
  # Name
  "TextEntry",
  # Inherits
  contains = "QualtricsSurveyQuestion"
)

# Constructor
TextEntry <- function(data, identifier) {

  # Call QualtricsSurveyQuestion constructor
  QSQ <- QualtricsSurveyQuestion(data, identifier)

  # Create an object
  .TextEntry(QSQ)

}

# -------------------------- #
# Slider class & constructor #
# -------------------------- #

# Survey instruction
.Slider <- setClass(
  # Name
  "Slider",
  # Inherits
  contains = "QualtricsSurveyQuestion"
)

# Constructor
Slider <- function(data, identifier) {

  # Call QualtricsSurveyQuestion constructor
  QSQ <- QualtricsSurveyQuestion(data, identifier)

  # Create an object
  .Slider(QSQ)

}

# ----------------------------- #
# RankOrder class & constructor #
# ----------------------------- #

.RankOrder <- setClass(
  # Name
  "RankOrder",
  # Inherits
  contains = "QualtricsSurveyQuestion"
)

# Constructor
RankOrder <- function(data, identifier) {

  # Call QualtricsSurveyQuestion constructor
  QSQ <- QualtricsSurveyQuestion(data, identifier)

  # Create an object
  .RankOrder(QSQ)

}

# ------------------------------ #
# SideBySide class & constructor #
# ------------------------------ #

.SideBySide <- setClass(
  # Name
  "SideBySide",
  # Inherits
  contains = "QualtricsSurveyQuestion"
)

# Constructor
SideBySide <- function(data, identifier) {

  # Call QualtricsSurveyQuestion constructor
  QSQ <- QualtricsSurveyQuestion(data, identifier)

  # Create an object
  .SideBySide(QSQ)

}

# ------------------------------- #
# ConstantSum class & constructor #
# ------------------------------- #

.ConstantSum <- setClass(
  # Name
  "ConstantSum",
  # Inherits
  contains = "QualtricsSurveyQuestion"
)

# Constructor
ConstantSum <- function(data, identifier) {

  # Call QualtricsSurveyQuestion constructor
  QSQ <- QualtricsSurveyQuestion(data, identifier)

  # Create an object
  .ConstantSum(QSQ)

}

# -------------------------------- #
# GroupAndRank class & constructor #
# -------------------------------- #

.GroupAndRank <- setClass(
  # Name
  "GroupAndRank",
  # Inherits
  contains = "QualtricsSurveyQuestion"
)

# Constructor
GroupAndRank <- function(data, identifier) {

  # Call QualtricsSurveyQuestion constructor
  QSQ <- QualtricsSurveyQuestion(data, identifier)

  # Create an object
  .GroupAndRank(QSQ)

}

# --------------------------- #
# HotSpot class & constructor #
# --------------------------- #

.HotSpot <- setClass(
  # Name
  "HotSpot",
  # Inherits
  contains = "QualtricsSurveyQuestion"
)

# Constructor
HotSpot <- function(data, identifier) {

  # Call QualtricsSurveyQuestion constructor
  QSQ <- QualtricsSurveyQuestion(data, identifier)

  # Create an object
  .HotSpot(QSQ)

}

# --------------------------- #
# HeatMap class & constructor #
# --------------------------- #

.HeatMap <- setClass(
  # Name
  "HeatMap",
  # Inherits
  contains = "QualtricsSurveyQuestion"
)

# Constructor
HeatMap <- function(data, identifier) {

  # Call QualtricsSurveyQuestion constructor
  QSQ <- QualtricsSurveyQuestion(data, identifier)

  # Create an object
  .HeatMap(QSQ)

}

# ----------------------------- #
# DrillDown class & constructor #
# ----------------------------- #

.DrillDown <- setClass(
  # Name
  "DrillDown",
  # Inherits
  contains = "QualtricsSurveyQuestion"
)

# Constructor
DrillDown <- function(data, identifier) {

  # Call QualtricsSurveyQuestion constructor
  QSQ <- QualtricsSurveyQuestion(data, identifier)

  # Create an object
  .DrillDown(QSQ)

}

# ------------------------------------ #
# NetPromoterScore class & constructor #
# ------------------------------------ #

.NetPromoterScore <- setClass(
  # Name
  "NetPromoterScore",
  # Inherits
  contains = c("MultipleChoice", "QualtricsSurveyQuestion")
)

# Constructor
NetPromoterScore <- function(data, identifier) {

  # Call QualtricsSurveyQuestion constructor
  QSQ <- QualtricsSurveyQuestion(data, identifier)

  # Create an object
  .NetPromoterScore(QSQ)

}

# ----------------------------- #
# HighLight class & constructor #
# ----------------------------- #

.HighLight <- setClass(
  # Name
  "HighLight",
  # Inherits
  contains = "QualtricsSurveyQuestion"
)

# Constructor
HighLight <- function(data, identifier) {

  # Call QualtricsSurveyQuestion constructor
  QSQ <- QualtricsSurveyQuestion(data, identifier)

  # Create an object
  .HighLight(QSQ)

}

# ----------------------------- #
# Signature class & constructor #
# ----------------------------- #

.Signature <- setClass(
  # Name
  "Signature",
  # Inherits
  contains = "QualtricsSurveyQuestion"
)

# Constructor
Signature <- function(data, identifier) {

  # Call QualtricsSurveyQuestion constructor
  QSQ <- QualtricsSurveyQuestion(data, identifier)

  # Create an object
  .Signature(QSQ)

}

# ----------------------------- #
# Timer class & constructor #
# ----------------------------- #

.Timer <- setClass(
  # Name
  "Timer",
  # Inherits
  contains = "QualtricsSurveyQuestion"
)

# Constructor
Timer <- function(data, identifier) {

  # Call QualtricsSurveyQuestion constructor
  QSQ <- QualtricsSurveyQuestion(data, identifier)

  # Create an object
  .Timer(QSQ)

}

# ---------------------------- #
# MetaInfo class & constructor #
# ---------------------------- #

.MetaInfo <- setClass(
  # Name
  "MetaInfo",
  # Inherits
  contains = "QualtricsSurveyQuestion"
)

# Constructor
MetaInfo <- function(data, identifier) {

  # Call QualtricsSurveyQuestion constructor
  QSQ <- QualtricsSurveyQuestion(data, identifier)

  # Create an object
  .MetaInfo(QSQ)

}

# ----------------------------- #
# Captcha class & constructor #
# ----------------------------- #

.Captcha <- setClass(
  # Name
  "Captcha",
  # Inherits
  contains = "QualtricsSurveyQuestion"
)

# Constructor
Captcha <- function(data, identifier) {

  # Call QualtricsSurveyQuestion constructor
  QSQ <- QualtricsSurveyQuestion(data, identifier)

  # Create an object
  .Captcha(QSQ)

}
