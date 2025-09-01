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
#'
#' @details
#' The API key is now sent via the HTTP header \code{x-goog-api-key} instead of as a URL query parameter.
#'
#' @return Generated text with real-time information from Google Search
#' @export
#' @examples
#' \dontrun{
#' library(gemini.R)
#' setAPI("YOUR_API_KEY")
#' gemini_search("What is the current Google stock price?")
#' }
#' @importFrom httr2 request req_headers req_body_raw req_perform resp_body_json resp_body_string
#' @importFrom cli cli_alert_danger cli_status_clear cli_status
#'
#' @seealso https://ai.google.dev/gemini-api/docs/google-search

gemini_search <- function(prompt, temperature = 1, maxOutputTokens = 8192, topK = 40, topP = 0.95, seed = 1234) {
  # 1. Replace parameter validation with validate_params function
  # The model is always fixed as "2.0-flash", so use the special value "fixed_model"
  if (!validate_params(prompt, "fixed_model", temperature, topP, topK, seed, api_key = TRUE)) {
    return(NULL)
  }

  # Fix Model as 2.0 flash
  # see this: https://ai.google.dev/gemini-api/docs/grounding?lang=rest#configure-search
  model_query <- paste0("gemini-2.0-flash:generateContent")

  # Create API URL
  url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_query)
  api_key <- Sys.getenv("GEMINI_API_KEY")

  # Show status while processing
  sb <- cli_status("Gemini is searching...")

  # 2. Compose generation_config as a separate variable
  generation_config <- list(
    temperature = temperature,
    maxOutputTokens = maxOutputTokens,
    topP = topP,
    topK = topK,
    seed = seed
  )

  # 3. Compose JSON request body
  # req_body_json does not work, so req_body_raw should be used,
  # but first create the structure and then convert to JSON
  request_structure <- list(
    contents = list(
      list(
        parts = list(
          list(text = prompt)
        )
      )
    ),
    generationConfig = generation_config,
    tools = list(
      list(google_search = list())
    )
  )

  # Convert to JSON string
  request_json <- jsonlite::toJSON(request_structure, auto_unbox = TRUE)

  # Make the request
  ## IMPORTANT: req_body_json not working with tools option.
  resp <- request(url) |>
    req_headers(
      "Content-Type" = "application/json",
      "x-goog-api-key" = api_key
    ) |>
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

  # 4. Add status code validation
  if (resp$status_code != 200) {
    cli_status_clear(id = sb)
    cli_alert_danger(paste0("Error in search request: Status code ", resp$status_code))
    return(NULL)
  }

  # Clear status indicator
  cli_status_clear(id = sb)

  # Process the response
  result <- resp_body_json(resp)

  # 5. Unify response processing method
  candidates <- result$candidates

  # Extract the response text - keep the existing method to handle special cases
  if (!is.null(candidates) && length(candidates) > 0 &&
    !is.null(candidates[[1]]$content$parts) && length(candidates[[1]]$content$parts) > 0) {
    # Process in a way consistent with other functions
    outputs <- unlist(lapply(candidates, function(candidate) candidate$content$parts))
    return(outputs)
  } else {
    cli_alert_danger("No valid response received or empty response.")
    return(NULL)
  }
}
