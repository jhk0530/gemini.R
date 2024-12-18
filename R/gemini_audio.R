#' @title Analyze audio using Gemini
#' @description This function sends audio to the Gemini API and returns a text description.
#'
#' @param audio Path to the audio file (default: uses a sample file).  Must be an MP3.
#' @param prompt A string describing what to do with the audio.
#' @param model The Gemini model to use ("1.5-flash" or "1.5-pro"). Defaults to "1.5-flash".
#' @param temperature Controls the randomness of the generated text (0-2).  Defaults to 0.5.
#' @param maxOutputTokens The maximum number of tokens in the generated text. Defaults to 1024.
#'
#' @return A character vector containing the Gemini API's response.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(gemini.R)
#' setAPI("YOUR_API_KEY")
#' gemini_image(audio = system.file("docs/reference/helloworld.mp3", package = "gemini.R"))
#' }
#'
#' @importFrom cli cli_alert_danger
#' @importFrom httr2 request req_url_query req_headers req_body_json req_perform resp_body_json req_method req_body_file resp_header
#'
#'
gemini_audio <- function(audio = NULL, prompt = "Describe this audio", model = "1.5-flash", temperature = 0.5, maxOutputTokens = 1024) {

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

  # VARIOUS TYPE NOT SUPPORTED YET
  # if (!(type %in% c("mp3"))) {
  #   cli_alert_danger("Error: Parameter 'type' must be one of 'mp3'")
  #   return(NULL)
  # }

  if (!(model %in% c("1.5-flash", "1.5-pro"))) {
    cli_alert_danger("Error: Parameter 'model' must be one of '1.5-flash', '1.5-pro'")
    return(NULL)
  }

  if (temperature < 0 | temperature > 2) {
    cli_alert_danger("Error: Parameter 'temperature' must be between 0 and 2")
    return(NULL)
  }

  ## TEMPORARY FILE UPLOAD VIA FILE API
  api_key <- Sys.getenv("GEMINI_API_KEY")
  file_url <- "https://generativelanguage.googleapis.com/upload/v1beta/files" # Google API 기본 URL

  # WAV - audio/wav
  # MP3 - audio/mp3
  # AIFF - audio/aiff
  # AAC - audio/aac
  # OGG Vorbis - audio/ogg
  # FLAC - audio/flac

  mime_type <- "audio/mp3" # mp3 only
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
    req_body_file(audio) |> # 바이너리 데이터 업로드
    req_perform()

  if (resp$status_code != 200) {
    stop("Error in upload request")
  }

  file_info <- upload_resp |>
    resp_body_json()
  file_uri <- file_info$file$uri

  cli_status_clear(id = sb)
  # prompt

  model_query <- paste0("gemini-", model, "-latest:generateContent")

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
        maxOutputTokens = maxOutputTokens
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
