#' @title Generate text from text with Gemini
#' @description Generate text from text with Gemini
#' @param prompt The prompt to generate text from
#'
#' @return Generated text
#' @export
#' @examples
#' library(gemini.R)
#' setAPI("YOUR_API_KEY")
#' gemini("Explain dplyr's mutate function")
#'
#' @importFrom httr POST content content_type_json
#' @importFrom cli cli_alert_danger cli_status_clear cli_status
#'
#' @seealso https://ai.google.dev/docs/gemini_api_overview#text_input
#'

gemini <- function(prompt) {
  if (is.null(prompt)) {
    cli_alert_danger("{.arg prompt} must not NULL")
    return(NULL)
  }

  if (!is.character(prompt)) {
    cli_alert_danger("{.arg prompt} must be given as a STRING")
    return(NULL)
  }

  if (Sys.getenv("GEMINI_API_KEY") == "") {
    cli_alert_danger("Please set the {.envvar GEMINI_API_KEY} with {.fn setAPI} function.")
    return(NULL)
  }

  model_query <- "gemini-pro:generateContent"

  url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_query)
  api_key <- Sys.getenv("GEMINI_API_KEY")

  sb <- cli_status("Gemini is answering...")

  req <- request(url) %>%
    req_url_query(key = api_key) %>%
    req_headers("Content-Type" = "application/json") %>%
    req_body_json(list(
      contents = list(
        parts = list(
          list(text = prompt)
        )
      ),
      generationConfig = list(
        temperature = 0.5,
        maxOutputTokens = 1024
      )
    ))

  resp <- req_perform(req)

  cli_status_clear(id = sb)

  candidates <- resp_body_json(resp)$candidates
  outputs <- unlist(lapply(candidates, function(candidate) candidate$content$parts))

  return(outputs)
}
