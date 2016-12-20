test_that("test if questionJsonParser works", {
  question <- paste0("{\"jsonTypeDefinition\":\"gameSelector\",\"type\":\"",
                     "MultipleChoice\",\"question\":{\"mediaType\":",
                     "\"maximizedText\",\"content\":\"880 + 15\"}}")
  expect_equal(questionJsonParser(question)$content, "880 + 15")
})

test_that("test if answerJsonParser works", {
  answer <- paste0("{\"layout\":\"CssHorizontal\",\"mediaType\":",
                   "\"maximizedTextAnswerOptions\",\"answerOptions\"",
                   ":[\"391\",\"841\",\"390\",\"381\",\"319\",\"346\"]}")
  expect_equal(answerJsonParser(answer), c("391", "841", "390", "381", "319", "346"))
})
