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
#' @importFrom cli cli_alert_danger cli_status cli_status_clear cli_alert_warning
#' @importFrom httr2 request req_url_query req_headers req_body_json req_perform resp_body_json req_method req_body_file resp_header
#'
#'
gemini_audio <- function(audio = NULL, prompt = "Describe this audio", model = "2.0-flash",
                         temperature = 1, maxOutputTokens = 8192,
                         topK = 40, topP = 0.95, seed = 1234) {
  if (is.null(prompt)) {
    cli_alert_danger("{.arg prompt} must not NULL")
    return(NULL)
  }

  if (missing(audio)) {
    audio <- system.file("docs/reference/helloworld.mp3", package = "gemini.R")
  }
  # "Hello World" made from https://ttsmaker.com/

  if (!is.character(prompt)) {
    cli_alert_danger("{.arg prompt} must be given as a STRING")
    return(NULL)
  }

  if (Sys.getenv("GEMINI_API_KEY") == "") {
    cli_alert_danger("Please set the {.envvar GEMINI_API_KEY} with {.fn setAPI} function.")
    return(NULL)
  }

  # Model
  supported_models <- c("2.0-flash", "2.0-flash-lite", "2.5-pro-exp-03-25")

  if (!(model %in% supported_models)) {
    cli_alert_danger("Error: Parameter 'model' must be one of '2.0-flash', '2.0-flash-lite', '2.5-pro-exp-03-25'")
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

  ## TEMPORARY FILE UPLOAD VIA FILE API
  api_key <- Sys.getenv("GEMINI_API_KEY")
  file_url <- "https://generativelanguage.googleapis.com/upload/v1beta/files"

  ext <- tolower(tools::file_ext(audio))

  if(ext == ""){
    cli_alert_warning("File extension not found. Please check the file path.")
    return(NULL)
  }

  if (ext == "mp3") {
    mime_type <- "audio/mp3"
  } else if (ext == "wav") {
    mime_type <- "audio/wav"
  } else if (ext == "aiff") {
    mime_type <- "audio/aiff"
  } else if (ext == "aac") {
    mime_type <- "audio/aac"
  } else if (ext == "ogg") {
    mime_type <- "audio/ogg"
  } else if (ext == "flac") {
    mime_type <- "audio/flac"
  }

  num_bytes <- file.info(audio)$size

  # status bar
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

  if (resp$status_code != 200) {
    stop("Error in file resumable request")
  }

  sb <- cli_status("Uploading audio file")
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

  if (resp$status_code != 200) {
    stop("Error in upload request")
  }

  file_info <- upload_resp |>
    resp_body_json()
  file_uri <- file_info$file$uri

  cli_status_clear(id = sb)
  # prompt

  url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_query)

  sb <- cli_status("Gemini is answering...")

  generate_req <- request(url) |>
    req_url_query(key = api_key) |>
    req_method("POST") |>
    req_headers("Content-Type" = "application/json") |>
    req_body_json(list(
      contents = list(
        parts = list(
          list(text = prompt),
          list(file_data = list(mime_type = mime_type, file_uri = file_uri))
        )
      ),
      generationConfig = list(
        temperature = temperature,
        maxOutputTokens = maxOutputTokens,
        topP = topP,
        topK = topK,
        seed = seed
      )
    ))

  generate_resp <- req_perform(generate_req)

  if (generate_resp$status_code != 200) {
    stop("Error in generate request")
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
  if (is.null(prompt)) {
    cli_alert_danger("{.arg prompt} must not NULL")
    return(NULL)
  }

  if (!is.character(prompt)) {
    cli_alert_danger("{.arg prompt} must be given as a STRING")
    return(NULL)
  }

  if (is.null(tokens)) {
    cli_alert_danger("{.arg tokens} must not be NULL. Use token.vertex() function to generate tokens.")
    return(NULL)
  }

  if (is.null(audio)) {
    cli_alert_danger("{.arg audio} must not be NULL")
    return(NULL)
  }

  # Parameters validation
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

  sb <- cli_status("Gemini is answering...")

  generate_req <- request(tokens$url) |>
    req_headers(
      "Authorization" = paste0("Bearer ", tokens$key),
      "Content-Type" = "application/json"
    ) |>
    req_body_json(list(
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
      generationConfig = list(
        temperature = temperature,
        maxOutputTokens = maxOutputTokens,
        topP = topP,
        topK = topK,
        seed = seed
      )
    )) |>
    req_perform()

  cli_status_clear(id = sb)

  response <- resp_body_json(generate_req)
  candidates <- response$candidates

  outputs <- unlist(lapply(candidates, function(candidate) candidate$content$parts))
  return(outputs)
}
