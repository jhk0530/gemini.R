#' @title Store API key in local environment file
#' @description Saves the API key to a local .Renviron file for persistent access across R sessions
#'
#' @param api_key The API key to store
#' @param overwrite Whether to overwrite the existing API key if already present in .Renviron (default: TRUE)
#' @param install_message Whether to display a message about how to use the API (default: TRUE)
#'
#' @return No return value, called for side effects.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' setEnv("your_api_key")
#' }
#'
#' @seealso \code{\link{setAPI}} which sets the API key for the current session only
#'
#' @importFrom cli cli_alert_success cli_alert_info cli_alert_warning cli_div cli_end cli_alert
#'
setEnv <- function(api_key, overwrite = TRUE, install_message = TRUE) {
  if (is.null(api_key) || api_key == "") {
    cli_alert_danger("API key cannot be empty")
    return(invisible(NULL))
  }

  # Path to .Renviron file
  home <- Sys.getenv("HOME")
  renviron_path <- file.path(home, ".Renviron")

  # Check if .Renviron exists
  if (!file.exists(renviron_path)) {
    file.create(renviron_path)
    cli_alert_info("Created .Renviron file at {.path {renviron_path}}")
  }

  # Read existing .Renviron content
  existing_content <- readLines(renviron_path, warn = FALSE)

  # Check if GEMINI_API_KEY is already set
  gemini_line_index <- grep("^GEMINI_API_KEY=", existing_content)

  if (length(gemini_line_index) > 0) {
    if (!overwrite) {
      cli_alert_warning("GEMINI_API_KEY already exists in .Renviron. Use overwrite = TRUE to replace it.")
      return(invisible(NULL))
    } else {
      # Replace existing entry
      existing_content[gemini_line_index] <- paste0("GEMINI_API_KEY=", api_key)
      cli_alert_info("Replacing existing GEMINI_API_KEY in .Renviron")
    }
  } else {
    # Append new entry
    existing_content <- c(existing_content, paste0("GEMINI_API_KEY=", api_key))
  }

  # Write back to .Renviron
  writeLines(existing_content, renviron_path)

  # Show last 4 characters of API key for confirmation
  last <- substr(api_key, nchar(api_key) - 3, nchar(api_key))

  cli_div(theme = list(span.str = list("background-color" = "blue")))
  cli_alert_success("API key {.str ...{last}} has been saved to {.path {renviron_path}}")
  cli_end()

  # Remind to restart R session
  cli_alert_info("Please restart your R session for the changes to take effect.")
  cli_alert_info("After restarting, the API key will be automatically available in all R sessions.")

  # Show usage example if requested
  if (install_message) {
    cli_alert("You may restart R and try {.run gemini_chat('What is CRAN?')}")
  }

  return(invisible(NULL))
}
