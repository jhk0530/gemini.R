#' @title Generate Roxygen Documentation
#'
#' @description Generates Roxygen2 documentation for an R function based on the currently selected code.
#'
#' @param prompt  A character string specifying additional instructions for the LLM.  Defaults to a prompt requesting Roxygen2 documentation without the original code.
#'
#' @return Invisibly returns the generated documentation string, but primarily inserts the text into the RStudio console.
#' @examples
#' \dontrun{
#' # Select your function code in the editor, then run:
#' gen_docs()
#'
#' # For custom instructions:
#' gen_docs("Generate minimal Roxygen docs for this function")
#' }
#'
#' @importFrom rstudioapi getActiveDocumentContext executeCommand insertText
#' @importFrom cli cli_alert_danger cli_status cli_status_clear
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

  # Check if code is selected
  if (nchar(trim(selectedCode)) == 0) {
    cli_alert_danger("No code selected. Please select an R function to document.")
    return(invisible(NULL))
  }

  # Show status message
  sb <- cli_status("Generating Roxygen documentation...")

  # API call and error handling
  description <- tryCatch(
    {
      result <- gemini(
        prompt = paste0(
          prompt,
          "\n---",
          selectedCode
        )
      )

      if (is.null(result) || length(result) == 0 || nchar(trim(result)) == 0) {
        cli_status_clear(id = sb)
        cli_alert_danger("Failed to generate documentation or received empty response.")
        return(invisible(NULL))
      }

      result
    },
    error = function(e) {
      cli_status_clear(id = sb)
      cli_alert_danger(paste0("Error generating documentation: ", e$message))
      return(invisible(NULL))
    }
  )

  # Early exit if error occurred
  if (is.null(description)) {
    return(invisible(NULL))
  }

  cli_status_clear(id = sb)

  # Insert result into console if available
  executeCommand("activateConsole")
  insertText(text = description)

  # Return generated documentation invisibly
  return(invisible(description))
}
