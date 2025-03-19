#' @title Generate text from text and image with Gemini
#' @description Generate text from text and image with Gemini
#' @param image The image to generate text
#' @param prompt The prompt to generate text, Default is "Explain this image"
#' @param model The model to use. Options are "2.0-flash", "2.0-flash-lite", "2.0-pro-exp-02-05". Default is '2.0-flash'
#'             see https://ai.google.dev/gemini-api/docs/models/gemini
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

gemini_image <- function(image = NULL, prompt = "Explain this image", model = "2.0-flash",
                         temperature = 1, maxOutputTokens = 8192, topK = 40, topP = 0.95,
                         seed = 1234, type = "png") {
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
    cli_alert_danger("Error: Parameter 'type' must be one of 'png', 'jpeg', 'webp', 'heic', 'heif'")
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
  return(outputs)
}


#' @title Generate text from text and image with Gemini Vertex API
#' @description Generate text from text and image with Gemini Vertex API
#'
#' @param image The image to generate text
#' @param prompt A character string specifying the prompt to use with the image. Defaults to "Explain this image".
#' @param type A character string specifying the image type ("png", "jpeg", "webp", "heic", "heif"). Defaults to "png".
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
#' @return A character string containing Gemini's description of the image.
#'
#' @importFrom cli cli_alert_danger cli_status cli_status_clear
#' @importFrom httr2 request req_headers req_body_json req_perform resp_body_json
#' @importFrom base64enc base64encode
#'
#' @export
gemini_image.vertex <- function(image = NULL, prompt = "Explain this image", type = "png", tokens = NULL,
                                temperature = 1, maxOutputTokens = 8192, topK = 40, topP = 0.95, seed = 1234) {
  if (is.null(image)) {
    cli_alert_danger("{.arg image} must not be NULL")
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

  if (!(type %in% c("png", "jpeg", "webp", "heic", "heif"))) {
    cli_alert_danger("Error: Parameter 'type' must be one of 'png', 'jpeg', 'webp', 'heic', 'heif'")
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

  mime_type <- paste0("image/", type)

  request_body <- list(
    contents = list(
      list(
        role = "user",
        parts = list(
          list(
            inline_data = list(
              mime_type = mime_type,
              data = base64encode(image)
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
  )

  sb <- cli_status("Gemini is answering...")

  response <- request(tokens$url) |>
    req_headers(
      "Authorization" = paste0("Bearer ", tokens$key),
      "Content-Type" = "application/json"
    ) |>
    req_body_json(request_body) |>
    req_perform() |>
    resp_body_json()

  cli_status_clear(id = sb)

  return(response$candidates[[1]]$content$parts[[1]]$text)
}
