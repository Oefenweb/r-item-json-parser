#' Removes JSON format from question
#'
#' @param question A item character question object with JSON format.
#' @return Item question without JSON format.
#' @examples
#' question <- paste0('{"jsonTypeDefinition":"gameSelector","type":"OpenAnswer",',
#'                    '"question":{"mediaType":"maximizedText","content":"0 x 1"}}')
#' content <- questionJsonParser(question)$content
#' type <- questionJsonParser(question)$mediaType
#' @export

questionJsonParser <- function(question) {
  questionParsed <- rjson::fromJSON(as.character(question))$question
  return(list(content = questionParsed$content,
              mediaType = questionParsed$mediaType))
}
