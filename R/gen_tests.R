#' Generates unit test code for an R function.
#'
#' @param prompt A character string specifying the prompt for the Gemini model.  If NULL, a default prompt is used.
#'
#' @return
#' #' A character string containing the generated unit test code.
#'
#' @export
gen_tests <- function(prompt = NULL) {
  if (is.null(prompt)) {
    prompt <-
      paste0(
        "Generate unit test code for an R function.",
        "\n answer must contain only R codes not ```r and ```"
      )
  }

  context <- getActiveDocumentContext()
  selectedCode <- context$selection[[1]]$text

  description <- gemini(
    prompt = paste0(
      prompt,
      "\n---",
      selectedCode
    )
  )

  executeCommand("activateConsole")
  insertText(text = description)
}
