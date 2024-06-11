#' @title Generate text from text and image with Gemini
#' @description Generate text from text and image with Gemini
#' @param prompt The prompt to generate text, Default is "Explain this image"
#' @param image The image to generate text
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

gemini_image <- function(image = NULL, prompt = "Explain this image") {
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

  model_query <- "gemini-pro-vision:generateContent"

  url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_query)

  api_key <- Sys.getenv("GEMINI_API_KEY")

  sb <- cli_status("Gemini is answering...")
  req <- request(url) %>%
    req_url_query(key = api_key) %>%
    req_headers("Content-Type" = "application/json") %>%
    req_body_json(list(
      contents = list(
        parts = list(
          list(
            text = prompt
          ),
          list(
            inline_data = list(
              mime_type = "image/png",
              data = base64encode(image)
            )
          )
        )
      ),
      generationConfig = list(
        temperature = 0.5,
        maxOutputTokens = 1024
      )
    ))

  resp <- req_perform(req)
  cli_status_clear(id = sb)

  candidates <- resp_body_json(resp)$candidates

  outputs <- unlist(lapply(candidates, function(candidate) candidate$content$parts))
  return(outputs)
}
