#' @title Multi-turn conversations (chat)
#' @description Generate text from text with Gemini
#' @param prompt The prompt to generate text from
#'
#' @return Generated text
#' @export
#' @examples
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
#'
#' @importFrom httr POST content content_type_json
#'
#' @seealso https://ai.google.dev/docs/gemini_api_overview#chat
#'
#'
gemini_chat <- function(prompt, history = list()) {
  if (Sys.getenv("GEMINI_API_KEY") == "") {
    cat("Please set the GEMINI_API_KEY environment variable with setAPI function.\n")
    return(NULL)
  }

  model_query <- "gemini-pro:generateContent"

  history <- history |>
    addHistory(role = "user", item = prompt)
  response <- POST(
    url = paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_query),
    query = list(key = Sys.getenv("GEMINI_API_KEY")),
    content_type_json(),
    encode = "json",
    body = list(
      contents = history,
      generationConfig = list(
        temperature = 0.5,
        maxOutputTokens = 1024
      )
    )
  )

  candidates <- content(response)$candidates
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
addHistory <- function(history, role, item) {
  history[[length(history) + 1]] <-
    list(
      role = role,
      parts = list(
        list(text = item)
      )
    )
  return(history)
}
