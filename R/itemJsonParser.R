#' Removes JSON format from item datafile.
#'
#' @param items A Oefenweb item dataframe or a Oefenweb item question or Ofenweb
#' answer options vector.
#' @param withFeedback Some games (as "meten") contain feedback in the question structure.
#'  If withFeedback is TRUE, an extra feedback column is added to the output data.frame.
#' @examples
#' question <- paste0('{"jsonTypeDefinition":"gameSelector","type":"OpenAnswer",',
#'                    '"question":{"mediaType":"maximizedText","content":"0 x 1"}}')
#' questionWithoutJSON <- itemJsonParser(question)
#' @return Input item dataframe or vector without JSON format.
#' @importFrom RJSONIO toJSON
#' @importFrom rjson fromJSON
#' @export
itemJsonParser <- function(items,
                           withFeedback = FALSE) {

  # Boolean initialization
  dfBo <- FALSE
  correctAnsIndex <- FALSE

  # Return data frame if there are not questions or answer options in string
  if (!(TRUE %in% grepl("jsonTypeDefinition", items)
        | TRUE %in% grepl("maximizedTextAnswerOptions", items)
        | TRUE %in% grepl("answerOptions", items))) {
    return(items)
  }

  # Determine the strings that have to be transformed
  transVec <- as.numeric()
  # test if
  if ("question" %in% colnames(items) &
      "answer_options" %in% colnames(items)) {
    transVec <- rbind(data.frame(trans = items$question,
                                 stringsAsFactors = FALSE),
                      data.frame(trans = items$answer_options,
                                 stringsAsFactors = FALSE))
    transVec <- cbind(transVec, data.frame(c(rep("question",
                                                 length(items$question)),
                                             rep("answerOption",
                                                 length(items$answer_options))),
                                           stringsAsFactors = FALSE))
    colnames(transVec)[2] <- "type"
    transVec$nr <- rep(c(1:nrow(items)), 2)
    dfBo <- TRUE
    if ("correct_answer" %in% colnames(items)) {
      correctAnswerNum <- suppressWarnings(as.numeric(gsub("\\]",
                                                           "",
                                                           gsub("\\[",
                                                                "",
                                                      items$correct_answer))))
      if (!any(is.na(correctAnswerNum))) {
        if (all(correctAnswerNum == 0)) {
          correctAnsIndex <- TRUE
        }
      }
    }
  } else {
    transVec <- data.frame(trans = items, stringsAsFactors = FALSE)
  }

  questVec <- as.numeric()
  ansVec <- as.numeric()
  corVec <- as.numeric()
  typeVec <- as.numeric()
  feedbackVec <- as.numeric()
  # Loop through strings to transform them
  for (i in 1:dim(transVec)[1]) {
    if (!is.na(transVec$trans[i]) & grepl("jsonTypeDefinition", transVec$trans[i])) {
      # because rjson fails on shaper domain id 56
      jsonTypes <- tryCatch({
          names(rjson::fromJSON(transVec$trans[i]))
        },
        error = function(cond) {
          # Choose a return value in case of error
          return(names(RJSONIO::fromJSON(transVec$trans[i])))
        })
    } else {
      jsonTypes <- ""
    }
    if (grepl("jsonTypeDefinition", transVec$trans[i]) &
        grepl("question", transVec$trans[i])) {
      questContent <- questionJsonParser(transVec$trans[i])$content
      if (length(questContent) == 1 | class(questContent) == "character") {
        questContent <- paste(as.character(questContent), collapse = " ")
      } else if (any(grepl("Content", names(questContent)))) {
        questContent <- questContent[[grep("Content", names(questContent))]]
      } else if (any(grepl("Text", names(questContent)))) {
        questContent <- questContent[[grep("Text", names(questContent))]]
      } else if (mode(questContent) == "list") {
        content <- questContent[[1]]
        if (length(questContent) > 1) {
          if ("imageContent" %in% names(questContent[[2]])) {
            secContent <- questContent[[2]]$imageContent
          } else {
            secContent <- questContent[[2]][1]
          }
        }
        questContent <- paste0(content, " (", secContent, ")")
        if ("spokenText" %in% as.character(sapply(questionJsonParser(transVec$trans[i])$content,
                                                  function(x) names(x)))) {
          questContent <- paste(sapply(questionJsonParser(transVec$trans[i])$content,
                                       function(x) x[["spokenText"]]), collapse = " - ")
        }
      }

      questVec <- c(questVec, questContent)
      typeVec <- c(typeVec, questionJsonParser(transVec$trans[i])$mediaType)
    } else {
      if (grepl("answerOptions", transVec$trans[i])) {
        answerOptions <- answerJsonParser(transVec$trans[i])
        ansVec <- c(ansVec, paste(answerOptions, collapse = ";"))

        if (dfBo & correctAnsIndex) {
          correctAnswerChr <- items$correct_answer[transVec$nr[i]]
          correctAnswerChr <- gsub("\\]", "", gsub("\\[", "", correctAnswerChr))
          correctAnswerInd <- suppressWarnings(as.numeric(correctAnswerChr))
          corVec <- c(corVec, answerOptions[correctAnswerInd + 1])
        }
      } else {
        if (dfBo) {
          if (transVec$type[i] == "question") {
            questVec <- c(questVec, as.character(transVec$trans[i]))
            typeVec <- c(typeVec, "")
          } else if (transVec$type[i] == "answerOption") {
            ansVec <- c(ansVec, as.character(transVec$trans[i]))

            corVec <- c(corVec, as.character(transVec$trans[i]))
          }
        } else {
          questVec <- c(questVec, as.character(transVec$trans[i]))
          typeVec <- c(typeVec, "")
          ansVec <- c(ansVec, as.character(transVec$trans[i]))
        }
      }
    }
    if ("feedback" %in% jsonTypes) {
      feedbackVec <- c(feedbackVec,
                       rjson::fromJSON(transVec$trans[i])$feedback$content)
    }
  }

  # for regenvolgorde, replace \times, \space and $
  if (length(questVec) > 0) {
    for (i in 1:length(questVec)) {
      if (!is.na(typeVec[i])) {
        if (typeVec[i] == "mathLatex") {
          Q <- questVec[i]
          if (grepl("\\\\space", Q) | # nolint
              grepl("\\\\times", Q) |# nolint
              grepl("\\$", Q)) {
            questVec[i] <- gsub("\\$", "",
                                gsub("\\\\times", "x", # nolint
                                     gsub("\\\\space", " ", Q))) # nolint
          }
        }
      }
    }
  }

  # Select data that has to be returned
  if (dfBo) {
    items$question <- questVec
    items$answer_options <- ansVec
    if (length(corVec) & correctAnsIndex) {
      items$correct_answer <- corVec
    }
    if (withFeedback & length(feedbackVec) == nrow(items)) {
      items$feedback <- feedbackVec
    }
    return(items)
  } else {
    if (length(questVec) > 0) {
      return(questVec)
    }
    if (length(ansVec) > 0) {
      return(ansVec)
    }
  }
}
