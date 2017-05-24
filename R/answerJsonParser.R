#' Removes JSON format from answer options
#'
#' @param answerOption A item character answer option object with JSON format.
#' @return Item answer without JSON format.
#' @examples
#' answerOption <- paste0("{\"layout\":\"Numpad\",\"mediaType\":\"",
#'                    "maximizedTextAnswerOptions\",\"answerOptions\":[\"0\"]}")
#' options <- answerJsonParser(answerOption)
#' @importFrom rjson fromJSON
#' @export
answerJsonParser <- function(answerOption) {
  # return quesiton if it does not contain JSON
  if (!(TRUE %in% grepl("jsonTypeDefinition", answerOption)
        | TRUE %in% grepl("maximizedTextAnswerOptions", answerOption)
        | TRUE %in% grepl("answerOptions", answerOption))) {
    return(answerOption)
  }
  answerOptionParsed <- rjson::fromJSON(answerOption)
  if (length(answerOptionParsed$answerOptions) == 1 |
      class(answerOptionParsed$answerOptions) == "character") {
    return(as.character(answerOptionParsed$answerOptions))
  }
  answerOutput <- as.character()
  for (a in 1:length(answerOptionParsed$answerOptions)) {
    answerOutput[a] <- answerOptionParsed$answerOptions[[a]]$answerOption
  }
  return(answerOutput)
}
