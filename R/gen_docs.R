#' @title Generate Roxygen Documentation
#'
#' @description Generates Roxygen2 documentation for an R function based on the currently selected code.
#'
#' @param prompt  A character string specifying additional instructions for the LLM.  Defaults to a prompt requesting Roxygen2 documentation without the original code.
#'
#' @return
#' A character string containing the generated Roxygen2 documentation.
#' @importFrom rstudioapi getActiveDocumentContext executeCommand insertText
#' @export
gen_docs <- function(prompt = NULL) {
  if (is.null(prompt)) {
    prompt <-
      paste0(
        "Generate Roxygen documentation for an R function.",
        "\nReturn value should start with #' only",
        "\nAlso not include original input code in return value.",
        "\nDo not include ```r and ```"
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
