#' @title Multi-turn conversations (chat)
#' @description Generate text from text with Gemini
#' @param prompt The prompt to generate text from
#' @param history history object to keep track of the conversation
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
#'
#' chats <- gemini_chat("Pretend you're a snowman and stay in character for each")
#' print(chats$outputs)
#'
#' chats <- gemini_chat("What's your favorite season of the year?", chats$history)
#' print(chats$outputs)
#'
#' chats <- gemini_chat("How do you think about summer?", chats$history)
#' print(chats$outputs)
#' }
#' @importFrom httr2 request req_url_query req_headers req_body_json req_perform resp_body_json
#' @importFrom cli cli_alert_danger cli_status_clear cli_status
#' @seealso https://ai.google.dev/docs/gemini_api_overview#chat
#'

gemini_chat <- function(prompt, history = list(), model = "1.5-flash", temperature = 0.5, maxOutputTokens = 1024) {
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

  if (!(model %in% c("1.5-flash", "1.5-pro", "1.0-pro"))) {
    cli_alert_danger("Error: Parameter 'a' must be one of '1.5-flash', '1.5-pro', '1.0-pro', '2.0-flash-exp'")
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

  history <- history |>
    addHistory(role = "user", item = prompt)

  url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_query)
  api_key <- Sys.getenv("GEMINI_API_KEY")

  sb <- cli_status("Gemini is answering...")
  req <- request(url) |>
    req_url_query(key = api_key) |>
    req_headers("Content-Type" = "application/json") |>
    req_body_json(list(
      contents = history,
      generationConfig = list(
        temperature = temperature,
        maxOutputTokens = maxOutputTokens
      )
    ))

  resp <- req_perform(req)
  cli_status_clear(id = sb)

  candidates <- resp_body_json(resp)$candidates
  outputs <- unlist(lapply(candidates, function(candidate) candidate$content$parts))

  history <- history |>
    addHistory(role = "model", item = outputs[[1]])

  return(list(outputs = outputs, history = history))
}

#' @title Add history for chating context
#' @description Add history for chating context
#' @param history The history of chat
#' @param role The role of chat: "user" or "model"
#' @param item The item of chat: "prompt" or "output"
#' @return The history of chat
#'

addHistory <- function(history, role = NULL, item = NULL) {
  if (is.null(role)) {
    cli_alert_danger("provide role")
    return(NULL)
  }
  if (is.null(item)) {
    cli_alert_danger("provide item")
    return(NULL)
  }
  history[[length(history) + 1]] <-
    list(
      role = role,
      parts = list(
        list(text = item)
      )
    )
  return(history)
}
