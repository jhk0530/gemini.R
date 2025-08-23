#' @title Generate text from text with Gemma
#' @description Generate text from text with Gemma with Gemini API
#' @param prompt The prompt to generate text from
#' @param model The model to use. Default is 'gemma-3-1b-it'.
#'              see https://ai.google.dev/gemma/docs/get_started#models-list
#' @param api_key Your API key. If NULL, uses GEMINI_API_KEY environment variable.
#' @param timeout Request timeout in seconds. Default is 60.
#' @return Generated text
#' @export
#' @examples
#' \dontrun{
#' gemma("Roses are red...")
#' }
#' @importFrom httr2 request req_headers req_body_json req_perform resp_body_json req_timeout
#' @importFrom cli cli_status_clear cli_status cli_alert_danger

gemma <- function(prompt, model = "gemma-3-1b-it", api_key = NULL, timeout = 60) {
  # Check prompt
  if (missing(prompt) || !is.character(prompt) || nchar(prompt) == 0) {
    cli::cli_alert_danger("Prompt must be a non-empty character string.")
    return(NULL)
  }
  # Get API key
  if (is.null(api_key)) {
    api_key <- Sys.getenv("GEMINI_API_KEY")
  }
  if (is.null(api_key) || api_key == "") {
    cli::cli_alert_danger("API key is missing. Set GEMINI_API_KEY env variable or provide api_key argument.")
    return(NULL)
  }
  # Build URL 
  model_query <- paste0(model, ":generateContent")
  url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_query)
  sb <- cli::cli_status("Gemma is answering...")
  # Build request body
  request_body <- list(
    contents = list(
      list(
        parts = list(
          list(text = prompt)
        )
      )
    )
  )
  # Send request with API key in header for better security
  req <- httr2::request(url) |>
    httr2::req_headers(
      "Content-Type" = "application/json",
      "x-goog-api-key" = api_key 
    ) |>
    httr2::req_body_json(request_body) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_timeout(as.integer(timeout))
  resp <- httr2::req_perform(req)
  # Check status code
if (resp$status_code != 200) {
  cli::cli_status_clear(id = sb)
  # Get error details from response body
  error_details <- httr2::resp_body_string(resp)
  cli::cli_alert_danger(paste0("Error in generate request: Status code ", resp$status_code, "\nDetails: ", error_details))
  return(NULL)
}
cli::cli_status_clear(id = sb)
  candidates <- httr2::resp_body_json(resp)$candidates
  # Handle case when there are no candidates
  if (is.null(candidates) || length(candidates) == 0) {
    cli::cli_alert_danger("No candidates returned in response.")
    return(NULL)
  }
  # Extract and concatenate text fields from each candidate's parts
  outputs <- sapply(
    candidates,
    function(candidate) paste0(sapply(candidate$content$parts, `[[`, "text"), collapse = "")
  )
  return(outputs)
}