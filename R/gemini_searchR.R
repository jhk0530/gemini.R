#' @title Generate text with real-time information using Gemini (Retrieval)
#' @description Generate text responses with simplified access to Gemini models
#' @param prompt The prompt or question to ask
#' @param model The model to use. Options are "1.5-flash", "1.5-pro". Default is '1.5-flash'.
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
#' @return Generated text response from the Gemini model
#' @export
#' @examples
#' \dontrun{
#' library(gemini.R)
#' setAPI("YOUR_API_KEY")
#' gemini_searchR("Who won the latest F1 grand prix?")
#' }
#' @importFrom httr2 request req_url_query req_headers req_body_json req_perform resp_body_string
#' @importFrom cli cli_alert_danger cli_status_clear cli_status
#'
#' @seealso https://ai.google.dev/docs/gemini_api_overview#text_input

gemini_searchR <- function(prompt, model = "1.5-flash", temperature = 1, maxOutputTokens = 8192,
                           topK = 40, topP = 0.95, seed = 1234) {
  # Input validation
  if (is.null(prompt)) {
    cli_alert_danger("{.arg prompt} must not be NULL")
    return(NULL)
  }

  if (!is.character(prompt)) {
    cli_alert_danger("{.arg prompt} must be given as a STRING")
    return(NULL)
  }

  # API Key validation
  if (Sys.getenv("GEMINI_API_KEY") == "") {
    cli_alert_danger("Please set the {.envvar GEMINI_API_KEY} with {.fn setAPI} function.")
    return(NULL)
  }

  # Model validation
  supported_models <- c("1.5-flash", "1.5-pro")
  if (!(model %in% supported_models)) {
    cli_alert_danger("Error: Parameter 'model' must be one of '1.5-flash', '1.5-pro'")
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

  # Create API URL and model ID
  model_query <- paste0("gemini-", model, ":generateContent")

  url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/",model_query)

  api_key <- Sys.getenv("GEMINI_API_KEY")

  # Show status while processing
  sb <- cli_status("Gemini is thinking...")

  # Create request body
  request_body <- list(
    contents = list(
      list(
        parts = list(
          list(text = prompt)
        )
      )
    ),
    tools = list(
      list(
        google_search_retrieval = list(
          dynamic_retrieval_config = list(
            mode = "MODE_DYNAMIC",
            dynamic_threshold = 1
          )
        )
      )
    ),
    generationConfig = list(
      temperature = temperature,
      maxOutputTokens = maxOutputTokens,
      topK = topK,
      topP = topP,
      seed = seed
    )
  )

  # Make the request
  resp <- request(url) |>
    req_headers("Content-Type" = "application/json") |>
    req_url_query(key = api_key) |>
    req_body_json(request_body, auto_unbox = TRUE) |>
    req_perform()

  # Clear status indicator
  cli_status_clear(id = sb)

  # Process the response as raw string
  result <- resp_body_string(resp)

  # Parse and extract text content from JSON response
  parsed_result <- try(jsonlite::fromJSON(result, simplifyVector = FALSE), silent = TRUE)

  if (!inherits(parsed_result, "try-error")) {
    # If JSON parsing successful, try to extract the text
    if (!is.null(parsed_result$candidates) &&
      length(parsed_result$candidates) > 0 &&
      !is.null(parsed_result$candidates[[1]]$content$parts) &&
      length(parsed_result$candidates[[1]]$content$parts)) {
      content_text <- parsed_result$candidates[[1]]$content$parts[[1]]$text
      return(content_text)
    }
  }

  # If we can't parse or extract from JSON, just return the raw response
  return(result)
}
