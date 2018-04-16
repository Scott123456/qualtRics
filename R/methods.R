# Methods here

# METHODS & HELPERS ----

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
  # Get selectors
  selector <- question_data$questionType$selector
  sub_selector <- question_data$questionType$subSelector

  # Switch
  switch(
    type,
    "MC" = switch(
      selector,
      "NPS" = NetPromoterScore(question_data, identifier = identifier),
      MultipleChoice(question_data, identifier = identifier)
    ),
    "DB" = switch(
      selector,
      "TB" = DescriptiveText(question_data, identifier = identifier),
      "GRB" = TextGraphic(question_data, identifier = identifier)
    ),
    "Matrix" = MatrixTable(question_data, identifier = identifier),
    "TE" = TextEntry(question_data, identifier = identifier),
    "Slider" = Slider(question_data, identifier = identifier),
    "RO" = RankOrder(question_data, identifier = identifier),
    "SBS" = SideBySide(question_data, identifier = identifier),
    "CS" = ConstantSum(question_data, identifier = identifier),
    "PGR" = GroupAndRank(question_data, identifier = identifier),
    "HotSpot" = HotSpot(question_data, identifier = identifier),
    "HeatMap" = HeatMap(question_data, identifier = identifier),
    "DD" = DrillDown(question_data, identifier = identifier),
    "HL" = HighLight(question_data, identifier = identifier),
    "Draw" = Signature(question_data, identifier = identifier),
    "Timing" = Timer(question_data, identifier = identifier),
    "Meta" = MetaInfo(question_data, identifier = identifier),
    "Captcha" = Captcha(question_data, identifier = identifier),
    # Else category
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


