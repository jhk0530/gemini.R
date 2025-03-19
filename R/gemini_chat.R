#' @title Multi-turn conversations (chat)
#' @description Generate text from text with Gemini
#' @param prompt The prompt to generate text from
#' @param history history object to keep track of the conversation
#' @param model The model to use. Options are "2.0-flash", "2.0-flash-lite", "2.0-pro-exp-02-05". Default is '2.0-flash'
#'              see https://ai.google.dev/gemini-api/docs/models/gemini
#' @param temperature The temperature to use. Default is 1 value should be between 0 and 2
#'              see https://ai.google.dev/gemini-api/docs/models/generative-models#model-parameters
#' @param maxOutputTokens The maximum number of tokens to generate.
#'              Default is 8192 and 100 tokens correspond to roughly 60-80 words.
#' @param topK The top-k value to use. Default is 40 value should be between 0 and 100
#'              see https://ai.google.dev/gemini-api/docs/models/generative-models#model-parameters
#' @param topP The top-p value to use. Default is 0.95 value should be between 0 and 1
#'              see https://ai.google.dev/gemini-api/docs/models/generative-models#model-parameters
#' @param seed The seed to use. Default is 1234 value should be integer
#'              see https://ai.google.dev/gemini-api/docs/models/generative-models#model-parameters
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

gemini_chat <- function(prompt, history = list(), model = "2.0-flash", temperature = 1,
                        maxOutputTokens = 8192, topK = 40, topP = 0.95, seed = 1234) {
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

  # Model

  supported_models <- c("2.0-flash", "2.0-flash-lite", "2.0-pro-exp-02-05")

  if (!(model %in% supported_models)) {
    cli_alert_danger("Error: Parameter 'model' must be one of '2.0-flash', '2.0-flash-lite', '2.0-pro-exp-02-05'")
    return(NULL)
  }


  model_query <- paste0("gemini-", model, ":generateContent")


  if (temperature < 0 | temperature > 2) {
    cli_alert_danger("Error: Parameter 'temperature' must be between 0 and 2")
    return(NULL)
  }

  if (topP < 0 | topP > 1) {
    cli_alert_danger("Error: Parameter 'topP' must be between 0 and 1")
    return(NULL)
  }

  if (topK < 0 | topK > 100) {
    cli_alert_danger("Error: Parameter 'topK' must be between 0 and 100")
    return(NULL)
  }

  if (!is.numeric(seed) || seed %% 1 != 0) {
    cli_alert_danger("Error: Parameter 'seed' must be an integer")
    return(NULL)
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
        maxOutputTokens = maxOutputTokens,
        topP = topP,
        topK = topK,
        seed = seed
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
