#' @title Multi-turn conversations (chat)
#' @description Generate text from text with Gemini
#' @param prompt The prompt to generate text from
#' @param history history object to keep track of the conversation
#' @param model The model to use. Options are "2.0-flash", "2.0-flash-lite", "2.5-pro-exp-03-25". Default is '2.0-flash'
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
  # 1. validate_params 함수 사용하여 파라미터 검증
  if (!validate_params(prompt, model, temperature, topP, topK, seed, api_key = TRUE)) {
    return(NULL)
  }

  history <- history |>
    addHistory(role = "user", item = prompt)

  model_query <- paste0("gemini-", model, ":generateContent")
  url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_query)
  api_key <- Sys.getenv("GEMINI_API_KEY")

  sb <- cli_status("Gemini is answering...")
  
  # 2. generation_config를 별도 리스트로 구성
  generation_config <- list(
    temperature = temperature,
    maxOutputTokens = maxOutputTokens,
    topP = topP,
    topK = topK,
    seed = seed
  )
  
  # 요청 본문을 별도의 변수로 생성
  request_body <- list(
    contents = history,
    generationConfig = generation_config
  )
  
  req <- request(url) |>
    req_url_query(key = api_key) |>
    req_headers("Content-Type" = "application/json") |>
    req_body_json(request_body)

  resp <- req_perform(req)
  
  # 3. 상태 코드 검증 추가
  if (resp$status_code != 200) {
    cli_status_clear(id = sb)
    cli_alert_danger(paste0("Error in generate request: Status code ", resp$status_code))
    return(NULL)
  }
  
  cli_status_clear(id = sb)

  candidates <- resp_body_json(resp)$candidates
  outputs <- unlist(lapply(candidates, function(candidate) candidate$content$parts))
  
  # 4. 응답 처리 개선 - 여러 응답을 처리할 수 있도록 수정
  if (length(outputs) > 0) {
    # 첫 번째 응답을 기록
    history <- history |>
      addHistory(role = "model", item = outputs[[1]])
  }

  return(list(outputs = outputs, history = history))
}

#' @title Add history for chating context
#' @description Add history for chating context
#' @param history The history of chat
#' @param role The role of chat: "user" or "model"
#' @param item The item of chat: "prompt" or "output"
#' @return The history of chat
#' @keywords internal
#' @importFrom cli cli_alert_danger
#'

addHistory <- function(history, role = NULL, item = NULL) {
  # 5. addHistory 함수 개선 - 오류 메시지 개선
  if (is.null(role)) {
    cli_alert_danger("{.arg role} must not be NULL")
    return(NULL)
  }
  if (is.null(item)) {
    cli_alert_danger("{.arg item} must not be NULL")
    return(NULL)
  }
  
  # 역할 검증 추가
  valid_roles <- c("user", "model")
  if (!(role %in% valid_roles)) {
    cli_alert_danger(paste0("Invalid role: '", role, "'. Must be one of: ", paste(valid_roles, collapse=", ")))
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
