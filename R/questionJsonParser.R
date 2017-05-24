#' Removes JSON format from question
#'
#' @param question A item character question object with JSON format.
#' @return Item question without JSON format.
#' @examples
#' question <- paste0('{"jsonTypeDefinition":"gameSelector","type":"OpenAnswer",',
#'                    '"question":{"mediaType":"maximizedText","content":"0 x 1"}}')
#' content <- questionJsonParser(question)$content
#' type <- questionJsonParser(question)$mediaType
#' @importFrom rjson fromJSON
#' @importFrom RJSONIO toJSON
#' @export
questionJsonParser <- function(question) {
  # return quesiton if it does not contain JSON
  if (!(TRUE %in% grepl("jsonTypeDefinition", question)
        | TRUE %in% grepl("maximizedTextAnswerOptions", question)
        | TRUE %in% grepl("answerOptions", question))) {
    return(question)
  }
  questionParsed <- tryCatch({
    rjson::fromJSON(as.character(question))$question
  },
  error = function(cond) {
    # Choose a return value in case of error
    return(RJSONIO::fromJSON(as.character(question), simplify = FALSE)$question)
  })
  return(list(content = questionParsed$content,
              mediaType = questionParsed$mediaType))
}
