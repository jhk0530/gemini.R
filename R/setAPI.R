#' @title Set API key
#' @description Set API key as an environment variable
#' @param api_key The API key to set
#' @return No return value, called for side effects.
#' @export
#' @examples
#' \dontrun{
#' setAPI("my_api_key")
#' }
#' @seealso https://makersuite.google.com/app/apikey
#' @importFrom cli cli_alert_info cli_div cli_end cli_alert
#' @keywords internal

setAPI <- function(api_key) {
  last <- substr(api_key, nchar(api_key) - 3, nchar(api_key))
  Sys.setenv(GEMINI_API_KEY = api_key)

  cli_div(theme = list(span.str = list("background-color" = "blue")))
  cli_alert_info("API key {.str ...{last}} is set.")
  cli_end()

  cli_alert("You may try {.run gemini_chat('What is CRAN?')}")
  return(NULL)
}
