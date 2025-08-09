#' @title Generate text from text with Gemini
#' @description Generate text from text with Gemini
#' @param prompt The prompt to generate text from
#' @param model The model to use. Default is '2.0-flash'.
#'              see https://ai.google.dev/gemini-api/docs/models/gemini
#' @param temperature The temperature to use. Default is 1 value should be between 0 and 2
#'              see https://ai.google.dev/gemini-api/docs/models/generative-models#model-parameters
#' @param topP The top-p value to use. Default is 0.95 value should be between 0 and 1
#'              see https://ai.google.dev/gemini-api/docs/models/generative-models#model-parameters
#' @param topK The top-k value to use. Default is 40 value should be between 0 and 100
#'              see https://ai.google.dev/gemini-api/docs/models/generative-models#model-parameters
#' @param seed The seed to use. Default is 1234 value should be integer
#'              see https://ai.google.dev/gemini-api/docs/models/generative-models#model-parameters
#' @param maxOutputTokens The maximum number of tokens to generate.
#'              Default is 8192 and 100 tokens correspond to roughly 60-80 words.
#' @param timeout Request timeout in seconds. Default is 60.
#' @return Generated text or image
#' @export
#' @examples
#' \dontrun{
#' library(gemini.R)
#' setAPI("YOUR_API_KEY")
#' gemini("Explain dplyr's mutate function")
#' }
#' @importFrom httr2 request req_headers req_body_json req_perform resp_body_json req_timeout
#' @importFrom cli cli_status_clear cli_status
#'
#' @seealso https://ai.google.dev/docs/gemini_api_overview#text_input
#'

gemini <- function(prompt, model = "2.0-flash", temperature = 1, maxOutputTokens = 8192, topK = 40, topP = 0.95, seed = 1234, timeout = 60) {
  # Validate all parameters at once
  if (!validate_params(prompt, model, temperature, topP, topK, seed, api_key = TRUE)) {
    return(NULL)
  }

  model_query <- paste0("gemini-", model, ":generateContent")
  url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_query)
  api_key <- Sys.getenv("GEMINI_API_KEY")

  sb <- cli_status("Gemini is answering...")

  # Create generation config
  generation_config <- list(
    temperature = temperature,
    maxOutputTokens = maxOutputTokens,
    topP = topP,
    topK = topK,
    seed = seed
  )

  # Add responseModalities only for image generation model
  if (model == "2.0-flash-exp-image-generation") {
    generation_config$responseModalities <- list("Text", "Image")
  }

  # Create request body as a separate list
  request_body <- list(
    contents = list(
      parts = list(
        list(text = prompt)
      )
    ),
    generationConfig = generation_config
  )

  # Set timeout using req_timeout
  req <- request(url) |>
    req_headers(
      "x-goog-api-key" = api_key,
      "Content-Type" = "application/json"
    ) |>
    req_body_json(request_body) |>
    req_timeout(as.integer(timeout))
  resp <- req_perform(req)

  # Add logic to check status code
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

#' @title Generate text from text with Gemini Vertex API
#' @description Generate text from text with Gemini Vertex API
#'
#' @param prompt A character string containing the prompt for the Gemini model.
#' @param tokens A list containing the API URL and key from token.vertex() function.
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
#' @param timeout Request timeout in seconds. Default is 60.
#' @param labels (Optional) A named list for custom metadata labels. 
#'               Example: \code{list(team = "research", env = "test")}.
#'
#' @examples
#' \dontrun{
#' # token should be created before this. using the token.vertex() function
#' prompt <- "What is sachins Jersey number?"
#' gemini.vertex(prompt, tokens)
#' gemini.vertex(prompt, tokens, labels = list(team = "research", env = "test"))
#' }
#'
#' @seealso https://ai.google.dev/docs/gemini_api_overview#text_input
#' @return A character string containing the generated text.
#' @importFrom httr2 request req_headers req_body_json req_perform resp_body_json req_timeout
#' @importFrom cli cli_status_clear cli_status cli_alert_info cli_alert_danger
#'
#' @export

gemini.vertex <- function(prompt = NULL, tokens = NULL, temperature = 1, maxOutputTokens = 8192,
                          topK = 40, topP = 0.95, seed = 1234, timeout = 60, labels = NULL) {
  # Validate all parameters at once
  if (!validate_params(prompt, NULL, temperature, topP, topK, seed, api_key = FALSE, tokens = tokens)) {
    return(NULL)
  }

  sb <- cli_status("Gemini Vertex is answering...")

  # Create generation config
  generation_config <- list(
    temperature = temperature,
    maxOutputTokens = maxOutputTokens,
    topP = topP,
    topK = topK,
    seed = seed
  )

  # Create request body as a separate list
  request_body <- list(
    contents = list(
      list(
        role = "user",
        parts = list(
          list(text = prompt)
        )
      )
    ),
    generationConfig = generation_config
  )

  # Check labels format: must be a named list (key-value pairs)
  if (!is.null(labels)) {
    # Check if labels is a named list with all names non-empty
    if (!(is.list(labels) && !is.null(names(labels)) && all(nzchar(names(labels))))) {
      cli_alert_info("labels must be a named list with key-value pairs. Example: list(team = 'research', env = 'test')")
      cli_status_clear(id = sb)
      return(NULL)
    } else {
      # Add labels directly to request body
      request_body$labels <- labels
    }
  }

  # Add req_timeout to set timeout
  req <- request(tokens$url) |>
    req_headers(
      "Authorization" = paste0("Bearer ", tokens$key),
      "Content-Type" = "application/json"
    ) |>
    req_body_json(request_body) |>
    req_timeout(as.integer(timeout))

  resp <- req_perform(req)

  # Add logic to check status code
  if (resp$status_code != 200) {
    cli_status_clear(id = sb)
    cli_alert_danger(paste0("Error in generate request: Status code ", resp$status_code))
    return(NULL)
  }

  response <- resp_body_json(resp)

  cli_status_clear(id = sb)

  candidates <- response$candidates
  outputs <- unlist(lapply(candidates, function(candidate) candidate$content$parts))

  return(outputs)
}
