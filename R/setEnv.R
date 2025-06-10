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
#' @importFrom cli cli_alert_success cli_alert_info cli_alert_warning cli_div cli_end cli_alert cli_alert_danger
#'
setEnv <- function(api_key, overwrite = TRUE, install_message = TRUE) {
  # 1. Improved API key validation
  if (is.null(api_key) || !is.character(api_key) || nchar(api_key) == 0) {
    cli_alert_danger("API key must be a non-empty string.")
    return(invisible(NULL))
  }
  
  # Check typical API key length (Google API keys are usually 39 characters)
  if (nchar(api_key) < 10) {
    cli_alert_warning("API key seems too short. Please verify your key.")
  }

  # 3. Improved platform compatibility
  home <- normalizePath("~", winslash = "/", mustWork = FALSE)
  renviron_path <- file.path(home, ".Renviron")

  # 4. Improved error handling
  # Add tryCatch when creating .Renviron file
  if (!file.exists(renviron_path)) {
    result <- tryCatch({
      file.create(renviron_path)
    }, error = function(e) {
      cli_alert_danger(paste("Failed to create .Renviron file:", e$message))
      return(FALSE)
    })
    
    if (result == FALSE) {
      return(invisible(NULL))
    }
    
    cli_alert_info("Created .Renviron file at {.path {renviron_path}}")
  }

  # Add tryCatch when reading .Renviron file
  existing_content <- tryCatch({
    readLines(renviron_path, warn = FALSE)
  }, error = function(e) {
    cli_alert_danger(paste("Failed to read .Renviron file:", e$message))
    return(NULL)
  })
  
  if (is.null(existing_content)) {
    return(invisible(NULL))
  }

  # Keep the existing code
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

  # Add tryCatch when writing to file
  result <- tryCatch({
    writeLines(existing_content, renviron_path)
    TRUE
  }, error = function(e) {
    cli_alert_danger(paste("Failed to write to .Renviron file:", e$message))
    return(FALSE)
  })
  
  if (result == FALSE) {
    return(invisible(NULL))
  }

  # 2. Improved API key masking
  last_chars <- 4
  if (nchar(api_key) > last_chars) {
    last <- substr(api_key, nchar(api_key) - (last_chars - 1), nchar(api_key))
  } else {
    # If the API key is too short, show only the last character
    last <- substr(api_key, nchar(api_key), nchar(api_key))
  }

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
