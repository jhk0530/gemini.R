#' @title Generate text from text and image with Gemini
#' @description Generate text from text and image with Gemini
#' @param prompt The prompt to generate text, Default is "Explain this image"
#' @param image The image to generate text
#' @param model The model to use. Options are '1.5-flash', '1.5-pro'. Default is '1.5-flash'
#'             see https://ai.google.dev/gemini-api/docs/models/gemini
#' @param temperature The temperature to use. Default is 0.5 value should be between 0 and 2
#'            see https://ai.google.dev/gemini-api/docs/models/generative-models#model-parameters
#' @param maxOutputTokens The maximum number of tokens to generate.
#'              Default is 1024 and 100 tokens correspond to roughly 60-80 words.
#' @param type The type of image. Options are 'png', 'jpeg', 'webp', 'heic', 'heif'. Default is 'png'
#'
#' @return Generated text
#' @export
#' @examples
#' \dontrun{
#' library(gemini.R)
#' setAPI("YOUR_API_KEY")
#' gemini_image(image = system.file("docs/reference/figures/image.png", package = "gemini.R"))
#' }
#'
#' @importFrom httr2 request req_url_query req_headers req_body_json req_perform resp_body_json
#' @importFrom base64enc base64encode
#' @importFrom cli cli_alert_danger cli_status_clear cli_status
#'
#' @seealso https://ai.google.dev/docs/gemini_api_overview#text_image_input
#'

gemini_image <- function(image = NULL, prompt = "Explain this image", model = "1.5-flash", temperature = 0.5, maxOutputTokens = 1024, type = "png") {
  if (is.null(prompt)) {
    cli_alert_danger("{.arg prompt} must not NULL")
    return(NULL)
  }

  if (missing(image)) {
    image <- system.file("docs/reference/figures/image.png", package = "gemini.R")
  }

  if (!is.character(prompt)) {
    cli_alert_danger("{.arg prompt} must be given as a STRING")
    return(NULL)
  }

  if (Sys.getenv("GEMINI_API_KEY") == "") {
    cli_alert_danger("Please set the {.envvar GEMINI_API_KEY} with {.fn setAPI} function.")
    return(NULL)
  }

  if (!(type %in% c("png", "jpeg", "webp", "heic", "heif"))) {
    cli_alert_danger("Error: Parameter 'a' must be one of 'png', 'jpeg', 'webp', 'heic', 'heif'")
    return(NULL)
  }

  if (!(model %in% c("1.5-flash", "1.5-pro"))) {
    cli_alert_danger("Error: Parameter 'a' must be one of '1.5-flash', '1.5-pro'")
    return(NULL)
  }

  if (temperature < 0 | temperature > 2) {
    cli_alert_danger("Error: Parameter 'temperature' must be between 0 and 2")
    return(NULL)
  }

  model_query <- paste0("gemini-", model, "-latest:generateContent")

  url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_query)

  api_key <- Sys.getenv("GEMINI_API_KEY")

  mime_type <- paste0("image/", type)

  sb <- cli_status("Gemini is answering...")
  req <- request(url) |>
    req_url_query(key = api_key) |>
    req_headers("Content-Type" = "application/json") |>
    req_body_json(list(
      contents = list(
        parts = list(
          list(
            text = prompt
          ),
          list(
            inline_data = list(
              mime_type = mime_type,
              data = base64encode(image)
            )
          )
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
