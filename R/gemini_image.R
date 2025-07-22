#' @title Generate text from text and image with Gemini
#' @description Generate text from text and image with Gemini
#' @param image The image to generate text
#' @param prompt The prompt to generate text, Default is "Explain this image"
#' @param model The model to use. Options are "2.0-flash", "2.0-flash-lite", "2.5-pro-exp-03-25". Default is '2.0-flash'
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
#' @importFrom httr2 request req_headers req_body_json req_perform resp_body_json
#' @importFrom base64enc base64encode
#' @importFrom cli cli_alert_danger cli_status_clear cli_status
#'
#' @details
#' The API key is now sent via the \code{x-goog-api-key} HTTP header instead of as a URL query parameter.
#'
#' @seealso https://ai.google.dev/docs/gemini_api_overview#text_image_input
#'

gemini_image <- function(image = NULL, prompt = "Explain this image", model = "2.0-flash",
                         temperature = 1, maxOutputTokens = 8192, topK = 40, topP = 0.95,
                         seed = 1234, type = "png") {
  # 1. validate_params 함수 사용
  if (!validate_params(prompt, model, temperature, topP, topK, seed, api_key = TRUE)) {
    return(NULL)
  }

  # 이미지 파일 검증
  if (missing(image)) {
    image <- system.file("docs/reference/figures/image.png", package = "gemini.R")
  }

  # 6. 이미지 파일 존재 여부 확인
  if (!file.exists(image)) {
    cli_alert_danger("Image file does not exist: ", image)
    return(NULL)
  }

  # 7. type 파라미터 검증 위치 이동
  if (!(type %in% c("png", "jpeg", "webp", "heic", "heif"))) {
    cli_alert_danger("Error: Parameter 'type' must be one of 'png', 'jpeg', 'webp', 'heic', 'heif'")
    return(NULL)
  }

  model_query <- paste0("gemini-", model, ":generateContent")
  url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_query)
  api_key <- Sys.getenv("GEMINI_API_KEY")
  mime_type <- paste0("image/", type)

  sb <- cli_status("Gemini is analyzing image...")

  # 8. 이미지 인코딩 오류 처리
  image_data <- NULL
  tryCatch(
    {
      image_data <- base64encode(image)
    },
    error = function(e) {
      cli_status_clear(id = sb)
      cli_alert_danger(paste0("Error encoding image: ", e$message))
      return(NULL)
    }
  )

  if (is.null(image_data)) {
    return(NULL)
  }

  # 2. generation_config 별도 리스트 사용
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
        list(
          text = prompt
        ),
        list(
          inline_data = list(
            mime_type = mime_type,
            data = image_data
          )
        )
      )
    ),
    generationConfig = generation_config
  )

  req <- request(url) |>
    req_headers(
      "Content-Type" = "application/json",
      "x-goog-api-key" = api_key
    ) |>
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
#' @return A character vector containing Gemini's description of the image.
#'
#' @importFrom cli cli_alert_danger cli_status cli_status_clear
#' @importFrom httr2 request req_headers req_body_json req_perform resp_body_json
#' @importFrom base64enc base64encode
#'
#' @export
gemini_image.vertex <- function(image = NULL, prompt = "Explain this image", type = "png", tokens = NULL,
                                temperature = 1, maxOutputTokens = 8192, topK = 40, topP = 0.95, seed = 1234) {
  # 1. Use validate_params function
  if (!validate_params(prompt, NULL, temperature, topP, topK, seed, api_key = FALSE, tokens = tokens)) {
    return(NULL)
  }

  # Validate image file
  if (is.null(image)) {
    cli_alert_danger("{.arg image} must not be NULL")
    return(NULL)
  }

  # 6. Check if image file exists
  if (!file.exists(image)) {
    cli_alert_danger("Image file does not exist: ", image)
    return(NULL)
  }

  # 7. Move type parameter validation
  if (!(type %in% c("png", "jpeg", "webp", "heic", "heif"))) {
    cli_alert_danger("Error: Parameter 'type' must be one of 'png', 'jpeg', 'webp', 'heic', 'heif'")
    return(NULL)
  }

  mime_type <- paste0("image/", type)

  # 8. Handle image encoding error
  image_data <- NULL
  tryCatch(
    {
      image_data <- base64encode(image)
    },
    error = function(e) {
      cli_alert_danger(paste0("Error encoding image: ", e$message))
      return(NULL)
    }
  )

  if (is.null(image_data)) {
    return(NULL)
  }

  # 2. Use separate list for generation_config
  generation_config <- list(
    temperature = temperature,
    maxOutputTokens = maxOutputTokens,
    topP = topP,
    topK = topK,
    seed = seed
  )

  request_body <- list(
    contents = list(
      list(
        role = "user",
        parts = list(
          list(
            inline_data = list(
              mime_type = mime_type,
              data = image_data
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

  # 4. Improve status message
  sb <- cli_status("Gemini Vertex is analyzing image...")

  # Separate API request for status code validation
  req <- request(tokens$url) |>
    req_headers(
      "Authorization" = paste0("Bearer ", tokens$key),
      "Content-Type" = "application/json"
    ) |>
    req_body_json(request_body)

  resp <- req_perform(req)

  # 3. Add status code validation
  if (resp$status_code != 200) {
    cli_status_clear(id = sb)
    cli_alert_danger(paste0("Error in generate request: Status code ", resp$status_code))
    return(NULL)
  }

  cli_status_clear(id = sb)

  # 5. Handle response same as gemini_image function
  response <- resp_body_json(resp)
  candidates <- response$candidates
  outputs <- unlist(lapply(candidates, function(candidate) candidate$content$parts))

  return(outputs)
}
