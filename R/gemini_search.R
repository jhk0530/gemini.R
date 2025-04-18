#' @title Generate text with real-time information using Google Search (Grounding)
#' @description Generate text responses that include up-to-date information from Google Search
#' @param prompt The prompt or question requiring real-time information
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
#' @return Generated text with real-time information from Google Search
#' @export
#' @examples
#' \dontrun{
#' library(gemini.R)
#' setAPI("YOUR_API_KEY")
#' gemini_search("What is the current Google stock price?")
#' }
#' @importFrom httr2 request req_url_query req_headers req_body_raw req_perform resp_body_json resp_body_string
#' @importFrom cli cli_alert_danger cli_status_clear cli_status
#'
#' @seealso https://ai.google.dev/docs/search_retrieval

gemini_search <- function(prompt, temperature = 1, maxOutputTokens = 8192, topK = 40, topP = 0.95, seed = 1234) {
  # 1. 파라미터 검증을 validate_params 함수로 대체
  # 모델은 항상 "2.0-flash"로 고정되므로 "fixed_model"이라는 특수값을 사용
  if (!validate_params(prompt, "fixed_model", temperature, topP, topK, seed, api_key = TRUE)) {
    return(NULL)
  }

  # Fix Model as 2.0 flash
  # see this: https://ai.google.dev/gemini-api/docs/grounding?lang=rest#configure-search
  model_query <- paste0("gemini-2.0-flash:generateContent")

  # Create API URL
  url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_query)
  api_key <- Sys.getenv("GEMINI_API_KEY")

  # Show status while processing
  sb <- cli_status("Gemini is searching...")

  # 2. generation_config를 별도 변수로 구성
  generation_config <- list(
    temperature = temperature,
    maxOutputTokens = maxOutputTokens,
    topP = topP,
    topK = topK,
    seed = seed
  )

  # 3. JSON 요청 본문 구성
  # req_body_json이 작동하지 않아 req_body_raw를 사용해야 하지만,
  # 구성을 먼저 생성한 후 JSON으로 변환하여 사용
  request_structure <- list(
    contents = list(
      list(
        parts = list(
          list(text = prompt)
        )
      )
    ),
    generationConfig = generation_config,
    tools = list(
      list(google_search = list())
    )
  )
  
  # JSON 문자열로 변환
  request_json <- jsonlite::toJSON(request_structure, auto_unbox = TRUE)
  
  # Make the request
    ## IMPORTANT: req_body_json not working with tools option.
  resp <- request(url) |>
    req_url_query(key = api_key) |>
    req_headers("Content-Type" = "application/json") |>
    req_body_raw(
      paste0('{
      "contents": [{"parts": [{"text": "', prompt, '"}]}],
      "generationConfig": {
        "temperature": ', temperature, ',
        "maxOutputTokens": ', maxOutputTokens, ',
        "topP": ', topP, ',
        "topK": ', topK, ',
        "seed": ', seed, '
      },
      "tools": [{"google_search": {}}]
  }')
) |>
    req_perform()
    
  # 4. 상태 코드 검증 추가
  if (resp$status_code != 200) {
    cli_status_clear(id = sb)
    cli_alert_danger(paste0("Error in search request: Status code ", resp$status_code))
    return(NULL)
  }

  # Clear status indicator
  cli_status_clear(id = sb)

  # Process the response
  result <- resp_body_json(resp)

  # 5. 응답 처리 방식 통일
  candidates <- result$candidates
  
  # Extract the response text - 특수한 경우에도 대응하기 위해 기존 방식 유지
  if (!is.null(candidates) && length(candidates) > 0 &&
      !is.null(candidates[[1]]$content$parts) && length(candidates[[1]]$content$parts) > 0) {
      
    # 다른 함수들과 일관된 방식으로 처리
    outputs <- unlist(lapply(candidates, function(candidate) candidate$content$parts))
    return(outputs)
  } else {
    cli_alert_danger("No valid response received or empty response.")
    return(NULL)
  }
}
