#' @title Validate Gemini API parameters
#' @description Helper function to validate parameters for Gemini API calls
#' @param prompt The prompt text to validate
#' @param model The model name to validate
#' @param temperature The temperature value to validate
#' @param topP The topP value to validate
#' @param topK The topK value to validate
#' @param seed The seed value to validate
#' @param api_key Whether to check for API key (TRUE/FALSE)
#' @param tokens The tokens object for vertex API (optional, provide NULL if not applicable)
#' @return TRUE if all validations pass, otherwise the function stops execution with an error message
#' @keywords internal
#' @importFrom cli cli_alert_danger

validate_params <- function(prompt, model, temperature = 1, topP = 0.95,
                            topK = 40, seed = 1234, api_key = TRUE, tokens = NULL) {
  # Validate prompt
  if (is.null(prompt)) {
    cli_alert_danger("{.arg prompt} must not be NULL")
    return(FALSE)
  }

  if (!is.character(prompt)) {
    cli_alert_danger("{.arg prompt} must be given as a STRING")
    return(FALSE)
  }

  # if api_key, check whether model is NULL
  if (api_key && is.null(model)) {
    cli_alert_danger("{.arg model} must not be NULL")
    return(FALSE)
  }

  # API Key validation
  if (api_key && Sys.getenv("GEMINI_API_KEY") == "") {
    cli_alert_danger("Please set the {.envvar GEMINI_API_KEY} with {.fn setAPI} function.")
    return(FALSE)
  }

  # Tokens validation for vertex API - when API key is not used
  if (!api_key && is.null(tokens)) {
    cli_alert_danger("{.arg tokens} must not be NULL. Use token.vertex() function to generate tokens.")
    return(FALSE)
  }

  # Parameter validations
  if (temperature < 0 | temperature > 2) {
    cli_alert_danger("Error: Parameter 'temperature' must be between 0 and 2")
    return(FALSE)
  }

  if (topP < 0 | topP > 1) {
    cli_alert_danger("Error: Parameter 'topP' must be between 0 and 1")
    return(FALSE)
  }

  if (topK < 0 | topK > 100) {
    cli_alert_danger("Error: Parameter 'topK' must be between 0 and 100")
    return(FALSE)
  }

  if (!is.numeric(seed) || seed %% 1 != 0) {
    cli_alert_danger("Error: Parameter 'seed' must be an integer")
    return(FALSE)
  }

  # All validations passed
  return(TRUE)
}

#' @title Trim whitespace from string
#' @description Removes leading and trailing whitespace from a string
#' @param x Character string to trim
#' @return Character string with leading and trailing whitespace removed
#' @keywords internal
trim <- function(x) {
  gsub("^\\s+|\\s+$", "", x)
}
