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
#' @importFrom httr2 request req_headers req_body_json req_perform resp_body_json
#' @importFrom jsonlite fromJSON
#' @importFrom cli cli_alert_danger cli_status_clear cli_status
#'
#' @seealso https://ai.google.dev/docs/gemini_api_overview#text_input

gemini_searchR <- function(prompt, model = "1.5-flash", temperature = 1, maxOutputTokens = 8192,
                           topK = 40, topP = 0.95, seed = 1234) {
  # 1. Use validate_params function for parameter validation
  if (!validate_params(prompt, model, temperature, topP, topK, seed, api_key = TRUE)) {
    return(NULL)
  }

  # Model series validation is still required (not performed in validate_params)
  supported_models <- c("1.5-flash", "1.5-pro")
  if (!(model %in% supported_models)) {
    cli_alert_danger("Error: Parameter 'model' must be one of '1.5-flash', '1.5-pro'")
    return(NULL)
  }

  # Create API URL and model ID
  model_query <- paste0("gemini-", model, ":generateContent")
  url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_query)
  api_key <- Sys.getenv("GEMINI_API_KEY")

  # Show status while processing
  sb <- cli_status("Gemini is retrieving information...")

  # 2. Build generation_config as a separate variable
  generation_config <- list(
    temperature = temperature,
    maxOutputTokens = maxOutputTokens,
    topK = topK,
    topP = topP,
    seed = seed
  )

  # 3. Build a consistent request body
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
    generationConfig = generation_config
  )

  # Make the request
  req <- request(url) |>
    req_headers(
      "Content-Type" = "application/json",
      "x-goog-api-key" = api_key
    ) |>
    req_body_json(request_body, auto_unbox = TRUE)

  resp <- req_perform(req)

  # 4. Add status code validation
  if (resp$status_code != 200) {
    cli_status_clear(id = sb)
    cli_alert_danger(paste0("Error in retrieval request: Status code ", resp$status_code))
    return(NULL)
  }

  # Clear status indicator
  cli_status_clear(id = sb)

  # 5. Unified response handling
  result <- resp_body_json(resp)

  # Kept for compatibility with previous approach
  candidates <- result$candidates

  # 6. Improved response structure handling
  if (!is.null(candidates) && length(candidates) > 0) {
    outputs <- unlist(lapply(candidates, function(candidate) candidate$content$parts))
    return(outputs)
  } else {
    cli_alert_danger("No valid response received or empty response.")
    return(NULL)
  }
}
