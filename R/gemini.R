#' @title Generate text from text with Gemini
#' @description Generate text from text with Gemini
#' @param prompt The prompt to generate text from
#' @param model The model to use. Options are '1.5-flash', '1.5-pro', '1.0-pro' and '2.0-flash-exp'. Default is '1.5-flash'
#'              see https://ai.google.dev/gemini-api/docs/models/gemini
#' @param temperature The temperature to use. Default is 0.5 value should be between 0 and 2
#'              see https://ai.google.dev/gemini-api/docs/models/generative-models#model-parameters
#' @param maxOutputTokens The maximum number of tokens to generate.
#'              Default is 1024 and 100 tokens correspond to roughly 60-80 words.
#'
#' @return Generated text
#' @export
#' @examples
#' \dontrun{
#' library(gemini.R)
#' setAPI("YOUR_API_KEY")
#' gemini("Explain dplyr's mutate function")
#' }
#' @importFrom httr2 request req_url_query req_headers req_body_json req_perform resp_body_json
#' @importFrom cli cli_alert_danger cli_status_clear cli_status
#'
#' @seealso https://ai.google.dev/docs/gemini_api_overview#text_input
#'

gemini <- function(prompt, model = "1.5-flash", temperature = 0.5, maxOutputTokens = 1024) {
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

  if (!(model %in% c("1.5-flash", "1.5-pro", "1.0-pro", "2.0-flash-exp"))) {
    cli_alert_danger("Error: Parameter 'model' must be one of '1.5-flash', '1.5-pro', '1.0-pro', '2.0-flash-exp")
    return(NULL)
  }

  if (temperature < 0 | temperature > 2) {
    cli_alert_danger("Error: Parameter 'temperature' must be between 0 and 2")
    return(NULL)
  }

  if (model == "2.0-flash-exp") {
    # exp is included, so remove -latest tag
    model_query <- paste0("gemini-", model, ":generateContent")
  } else {
    model_query <- paste0("gemini-", model, "-latest:generateContent")
  }

  url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_query)

  api_key <- Sys.getenv("GEMINI_API_KEY")

  sb <- cli_status("Gemini is answering...")

  req <- request(url) |>
    req_url_query(key = api_key) |>
    req_headers("Content-Type" = "application/json") |>
    req_body_json(list(
      contents = list(
        parts = list(
          list(text = prompt)
        )
      ),
      generationConfig = list(
        temperature = temperature,
        maxOutputTokens = maxOutputTokens
      )
    ))

  resp <- req_perform(req)

  cli_status_clear(id = sb)

  candidates <- resp_body_json(resp)$candidates
  outputs <- unlist(lapply(candidates, function(candidate) candidate$content$parts))

  return(outputs)
}

#' @title Generate text from text with Gemini Vertex API
#' @description Generate text from text with Gemini Vertex API
#'
#' @param prompt A character string containing the prompt for the Gemini model.
#' @param tokens A list containing the API URL and key from token.vertex() function.
#'
#' @examples
#' \dontrun{
#' # token should be created before this. using the token.vertex() function
#' prompt <- "What is sachins Jersey number?"
#' gemini.vertex(prompt, tokens)
#' }
#'
#' @seealso https://ai.google.dev/docs/gemini_api_overview#text_input
#' @return #' A character string containing the generated text.
#' @importFrom httr2 request req_headers req_body_json req_perform resp_body_json
#'
#' @export

gemini.vertex <- function(prompt = NULL, tokens = NULL){
  if(is.null(prompt)){
    cli_alert_danger("{.arg prompt} must not NULL")
    return(NULL)
  }

  if (!is.character(prompt)) {
    cli_alert_danger("{.arg prompt} must be given as a STRING")
    return(NULL)
  }

  request_body <- list(
    contents = list(
      list(
        role = "user",
        parts = list(
          list(text = prompt)
        )
      )
    )
  )

  sb <- cli_status("Gemini is answering...")

  response <- request(tokens$url) |>
    req_headers(
      "Authorization" = paste0("Bearer ", tokens$key),
      "Content-Type" = "application/json"
    ) |>
    req_body_json(request_body) |>
    req_perform() |>
    resp_body_json()

  cli_status_clear(id = sb)

  return(response$candidates[[1]]$content$parts[[1]]$text)
}
