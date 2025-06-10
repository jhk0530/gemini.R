#' @title Set Gemini API Key
#' @description Sets the Gemini API key as an environment variable for use in API calls.
#' @param api_key A character string containing your Gemini API key.
#' @return No return value, called for side effects.
#' @export
#' @examples
#' \dontrun{
#' setAPI("YOUR_API_KEY")
#' }
#' @seealso https://makersuite.google.com/app/apikey
#' @note Please be aware you have to agree to the terms of service of the API provider.
#'   Any app that uses the API key is subject to the terms of service.
#'   Also, please be aware that the API key is a sensitive information.
#' @importFrom cli cli_alert_info cli_div cli_end cli_alert cli_alert_danger

setAPI <- function(api_key) {
  # Validate API key
  if (is.null(api_key) || !is.character(api_key) || nchar(api_key) == 0) {
    cli_alert_danger("API key must be a non-empty string.")
    return(invisible())
  }
  
  # Check typical API key length (Google API keys are usually 39 characters)
  if (nchar(api_key) < 10) {
    cli_alert_danger("API key seems too short. Please verify your key.")
    return(invisible())
  }
  
  # Safely display only the last part of the API key
  last_chars <- 4
  if (nchar(api_key) > last_chars) {
    last <- substr(api_key, nchar(api_key) - (last_chars - 1), nchar(api_key))
  } else {
    # If the API key is too short, show only the last character
    last <- substr(api_key, nchar(api_key), nchar(api_key))
  }
  
  # Set environment variable
  Sys.setenv(GEMINI_API_KEY = api_key)

  # Provide user feedback
  cli_div(theme = list(span.str = list("background-color" = "blue")))
  cli_alert_info("API key {.str ...{last}} is set.")
  cli_end()

  cli_alert("You may try {.run gemini_chat('What is CRAN?')}")
  
  return(invisible())
}
