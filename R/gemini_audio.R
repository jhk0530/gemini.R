#' @title Analyze audio using Gemini
#' @description This function sends audio to the Gemini API and returns a text description.
#'
#' @param audio Path to the audio file (default: uses a sample file). Must be an MP3.
#' @param prompt A string describing what to do with the audio.
#' @param model The model to use. Options are "2.0-flash", "2.0-flash-lite", "2.5-pro-exp-03-25". Default is '2.0-flash'
#'              see https://ai.google.dev/gemini-api/docs/models/gemini
#' @param temperature The temperature to use. Default is 1 value should be between 0 and 2
#'            see https://ai.google.dev/gemini-api/docs/models/generative-models#model-parameters
#' @param maxOutputTokens The maximum number of tokens to generate.
#'              Default is 8192 and 100 tokens correspond to roughly 60-80 words.
#' @param topK The top-k value to use. Default is 40 value should be between 0 and 100
#'              see https://ai.google.dev/gemini-api/docs/models/generative-models#model-parameters
#' @param topP The top-p value to use. Default is 0.95 value should be between 0 and 1
#'              see https://ai.google.dev/gemini-api/docs/models/generative-models#model-parameters
#' @param seed The seed to use. Default is 1234 value should be integer
#'              see https://ai.google.dev/gemini-api/docs/models/generative-models#model-parameters
#'
#' @return A character vector containing the Gemini API's response.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(gemini.R)
#' setAPI("YOUR_API_KEY")
#' gemini_audio(audio = "YOUR_AUDIO_FILE")
#' }
#'
#' @importFrom tools file_ext
#' @importFrom cli cli_alert_danger cli_status cli_status_clear cli_alert_warning cli_alert_info
#' @importFrom httr2 request req_url_query req_headers req_body_json req_perform resp_body_json req_method req_body_file resp_header
#'
#'
gemini_audio <- function(audio = NULL, prompt = "Describe this audio", model = "2.0-flash",
                         temperature = 1, maxOutputTokens = 8192,
                         topK = 40, topP = 0.95, seed = 1234) {
  # 1. validate_params 함수를 사용하여 기본 파라미터 검증
  if (!validate_params(prompt, model, temperature, topP, topK, seed, api_key = TRUE)) {
    return(NULL)
  }

  if (missing(audio)) {
    audio <- system.file("docs/reference/helloworld.mp3", package = "gemini.R")
  }
  # "Hello World" made from https://ttsmaker.com/

  ## TEMPORARY FILE UPLOAD VIA FILE API
  api_key <- Sys.getenv("GEMINI_API_KEY")
  file_url <- "https://generativelanguage.googleapis.com/upload/v1beta/files"

  ext <- tolower(tools::file_ext(audio))

  if(ext == ""){
    cli_alert_warning("File extension not found. Please check the file path.")
    return(NULL)
  }

  # 2. 지원되지 않는 파일 확장자에 대한 에러 처리
  supported_extensions <- c("mp3", "wav", "aiff", "aac", "ogg", "flac")
  if (!(ext %in% supported_extensions)) {
    cli_alert_danger(paste0("Unsupported file extension: '", ext, "'. Currently supported extensions are: ", 
                          paste(supported_extensions, collapse=", ")))
    cli_alert_info("Please submit an issue at https://github.com/jhk0530/gemini.R/issues for additional file format support.")
    return(NULL)
  }

  # 파일 타입 매핑
  mime_type <- paste0("audio/", ext)

  # 특수 케이스만 오버라이드: 아직 정의 되지 않음
  # special_cases <- list(mp3 = "audio/mpeg")
  #if (!is.null(special_cases[[ext]])) {
  #  mime_type <- special_cases[[ext]]
  #}
  
  num_bytes <- file.info(audio)$size

  # 파일 업로드 준비
  resumable_request <-
    request(file_url) |>
    req_url_query(key = api_key) |>
    req_method("POST") |>
    req_headers(
      "X-Goog-Upload-Protocol" = "resumable",
      "X-Goog-Upload-Command" = "start",
      "X-Goog-Upload-Header-Content-Length" = as.character(num_bytes),
      "X-Goog-Upload-Header-Content-Type" = mime_type,
      "Content-Type" = "application/json"
    ) |>
    req_body_json(list(
      file = list(display_name = "AUDIO")
    ))

  resp <- resumable_request |>
    req_perform()

  # 4. 상태 코드 검증 추가
  if (resp$status_code != 200) {
    cli_alert_danger(paste0("Error in file resumable request: Status code ", resp$status_code))
    return(NULL)
  }

  # 3. 상태 표시 메시지 수정
  sb <- cli_status("Uploading audio file to Gemini...")
  upload_url <- resp_header(resp, "X-Goog-Upload-URL")
  upload_resp <- request(upload_url) |>
    req_method("POST") |>
    req_headers(
      `Content-Length` = as.character(num_bytes),
      `X-Goog-Upload-Offset` = "0",
      `X-Goog-Upload-Command` = "upload, finalize"
    ) |>
    req_body_file(audio) |>
    req_perform()

  if (upload_resp$status_code != 200) {
    cli_status_clear(id = sb)
    cli_alert_danger(paste0("Error in upload request: Status code ", upload_resp$status_code))
    return(NULL)
  }

  file_info <- upload_resp |>
    resp_body_json()
  file_uri <- file_info$file$uri

  cli_status_clear(id = sb)
  # prompt

  # Add this line to define model_query
  model_query <- paste0("gemini-", model, ":generateContent")
  
  url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_query)

  sb <- cli_status("Gemini is analyzing audio...")

  # 5. generation_config를 별도 리스트로 구성
  generation_config <- list(
    temperature = temperature,
    maxOutputTokens = maxOutputTokens,
    topP = topP,
    topK = topK,
    seed = seed
  )
  
  # 요청 본문도 별도 리스트로 구성
  request_body <- list(
    contents = list(
      parts = list(
        list(text = prompt),
        list(file_data = list(mime_type = mime_type, file_uri = file_uri))
      )
    ),
    generationConfig = generation_config
  )

  generate_req <- request(url) |>
    req_url_query(key = api_key) |>
    req_method("POST") |>
    req_headers("Content-Type" = "application/json") |>
    req_body_json(request_body)

  generate_resp <- req_perform(generate_req)

  if (generate_resp$status_code != 200) {
    cli_status_clear(id = sb)
    cli_alert_danger(paste0("Error in generate request: Status code ", generate_resp$status_code))
    return(NULL)
  }

  cli_status_clear(id = sb)

  candidates <- resp_body_json(generate_resp)$candidates

  outputs <- unlist(lapply(candidates, function(candidate) candidate$content$parts))
  return(outputs)
}

#' @title Analyze Audio using Gemini Vertex API
#' @description This function sends audio to the Gemini API and returns a text description.
#'
#' @param audio Path to the audio file (character string). only supports "mp3".
#' @param prompt A prompt to guide the Gemini API's analysis (character string, defaults to "Describe this audio").
#' @param tokens A list containing the API URL and key from token.vertex() function.
#' @param temperature The temperature to use. Default is 1 value should be between 0 and 2
#'            see https://ai.google.dev/gemini-api/docs/models/generative-models#model-parameters
#' @param maxOutputTokens The maximum number of tokens to generate.
#'              Default is 8192 and 100 tokens correspond to roughly 60-80 words.
#' @param topK The top-k value to use. Default is 40 value should be between 0 and 100
#'              see https://ai.google.dev/gemini-api/docs/models/generative-models#model-parameters
#' @param topP The top-p value to use. Default is 0.95 value should be between 0 and 1
#'              see https://ai.google.dev/gemini-api/docs/models/generative-models#model-parameters
#' @param seed The seed to use. Default is 1234 value should be integer
#'              see https://ai.google.dev/gemini-api/docs/models/generative-models#model-parameters
#'
#' @return A character vector containing the Gemini API's description of the audio.
#'
#' @importFrom httr2 request req_headers req_body_json req_perform resp_body_json
#' @importFrom cli cli_alert_danger cli_status cli_status_clear
#'
#' @export
gemini_audio.vertex <- function(audio = NULL, prompt = "Describe this audio", tokens = NULL,
                                temperature = 1, maxOutputTokens = 8192,
                                topK = 40, topP = 0.95, seed = 1234) {
  # 1. validate_params 함수 사용하여 파라미터 검증
  if (!validate_params(prompt, NULL, temperature, topP, topK, seed, api_key = FALSE, tokens = tokens)) {
    return(NULL)
  }

  # 오디오 파일 검증
  if (is.null(audio)) {
    cli_alert_danger("{.arg audio} must not be NULL")
    return(NULL)
  }

  # 3. 상태 표시 메시지 수정
  sb <- cli_status("Gemini Vertex is analyzing audio...")

  # 5. generation_config를 별도 리스트로 구성
  generation_config <- list(
    temperature = temperature,
    maxOutputTokens = maxOutputTokens,
    topP = topP,
    topK = topK,
    seed = seed
  )
  
  # 요청 본문도 별도 리스트로 구성
  request_body <- list(
    contents = list(
      list(
        role = "user",
        parts = list(
          list(
            file_data = list(
              mime_type = "audio/mp3",
              file_uri = audio
            )
          ),
          list(
            text = prompt
          )
        )
      )
    ),
    generationConfig = generation_config
  )

  generate_req <- request(tokens$url) |>
    req_headers(
      "Authorization" = paste0("Bearer ", tokens$key),
      "Content-Type" = "application/json"
    ) |>
    req_body_json(request_body) |>
    req_perform()

  # 4. 상태 코드 검증 추가
  if (generate_req$status_code != 200) {
    cli_status_clear(id = sb)
    cli_alert_danger(paste0("Error in generate request: Status code ", generate_req$status_code))
    return(NULL)
  }

  cli_status_clear(id = sb)

  response <- resp_body_json(generate_req)
  candidates <- response$candidates

  outputs <- unlist(lapply(candidates, function(candidate) candidate$content$parts))
  return(outputs)
}
