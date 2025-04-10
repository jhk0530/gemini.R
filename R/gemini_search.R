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

  # Fix Model as 2.0 flash
  # see this: https://ai.google.dev/gemini-api/docs/grounding?lang=rest#configure-search
  model_query <- paste0("gemini-2.0-flash:generateContent")

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

  # Create API URL
  url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_query)
  api_key <- Sys.getenv("GEMINI_API_KEY")

  # Show status while processing
  sb <- cli_status("Gemini is searching...")

  # Make the request
  ## IMPORT: req_body_json not working with tools option.
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

  # Clear status indicator
  cli_status_clear(id = sb)

  # Process the response
  result <- resp_body_json(resp)

  # Extract the response text
  if (!is.null(result$candidates) &&
    length(result$candidates) > 0 &&
    !is.null(result$candidates[[1]]$content$parts) &&
    length(result$candidates[[1]]$content$parts) > 0) {
    # Extract text from first candidate's first part
    content_text <- result$candidates[[1]]$content$parts[[1]]$text
    # Print the response and return it
    return(content_text)
  } else {
    cli_alert_danger("No valid response received or empty response.")
    return(NULL)
  }
}
