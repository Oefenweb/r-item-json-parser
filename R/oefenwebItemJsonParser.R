#' oefenwebItemJsonParser: A package to remove JSON format
#'
#' The oefenwebItemJsonParser package provides functionality to remove JSON format
#' from Oefenweb items with the itemJsonParser function. The parser can be applied
#' to a full dataset of items to remove JSON format from the item questions and item
#'  answer options. In addition, JSON can also be removed from only the questions
#'  or only the answer options with the itemJsonParser. The functions questionJsonParser and
#'  answerJsonParser are normally not needed as itemJsonParser can remove the JSON.
#'  The questionJsonParser is only important in situation in which mediaType or other
#'  JSON parameter are necessary.
#'
#'
#' @section oefenwebItemJsonParser functions: The oefenwebItemJsonParser package
#' contains the main function itemJsonParser
#' which removes JSON format from an item dataset or form a question or
#' answer options vector. The package also contains a function called questionJsonParser
#' and answerJsonParser to remove JSON format of a single question or answer options resepctively.
#'
#' @docType package
#' @name oefenwebItemJsonParser
NULL
