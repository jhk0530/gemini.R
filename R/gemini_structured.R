#' @title Generate structured response from Gemini
#' @description Returns a structured (JSON) response from the Gemini API.
#' @param prompt The prompt (question) to send to the model.
#' @param schema JSON schema (as a list) for the expected response.
#' @param model Model to use. Default is '2.5-flash'.
#' @param temperature Sampling temperature. Default is 1.
#' @param maxOutputTokens Maximum number of output tokens. Default is 8192.
#' @param topK Top-k value. Default is 40.
#' @param topP Top-p value. Default is 0.95.
#' @param seed Random seed. Default is 1234.
#' @param timeout Request timeout in seconds. Default is 60.
#' @return A structured list (parsed JSON).
#' @export
#' @examples
#' \dontrun{
#' schema <- list(
#'   type = "ARRAY",
#'   items = list(
#'     type = "OBJECT",
#'     properties = list(
#'       recipeName = list(type = "STRING"),
#'       ingredients = list(
#'         type = "ARRAY",
#'         items = list(type = "STRING")
#'       )
#'     ),
#'     propertyOrdering = c("recipeName", "ingredients")
#'   )
#' )
#' gemini_structured("List a few popular cookie recipes, and include the amounts of ingredients.", schema)
#' }
#' @importFrom httr2 request req_url_query req_headers req_body_json req_perform resp_body_json req_timeout
#' @importFrom cli cli_status_clear cli_status

gemini_structured <- function(
  prompt,
  schema,
  model = "2.5-flash",
  temperature = 1,
  maxOutputTokens = 8192,
  topK = 40,
  topP = 0.95,
  seed = 1234,
  timeout = 60
) {
  # Validate API key
  api_key <- Sys.getenv("GEMINI_API_KEY")
  if (is.null(api_key) || api_key == "") {
    cli_alert_danger("GEMINI_API_KEY environment variable is not set.")
    return(NULL)
  }

  # Set API endpoint
  model_query <- paste0("gemini-", model, ":generateContent")
  url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_query)

  sb <- cli_status("Gemini is generating a structured response...")

  # Create generation config
  generation_config <- list(
    temperature = temperature,
    maxOutputTokens = maxOutputTokens,
    topP = topP,
    topK = topK,
    seed = seed,
    responseMimeType = "application/json",
    responseSchema = schema
  )

  # Create request body
  request_body <- list(
    contents = list(
      list(
        parts = list(
          list(text = prompt)
        )
      )
    ),
    generationConfig = generation_config
  )

  # Send request
  req <- request(url) |>
    req_url_query(key = api_key) |>
    req_headers("Content-Type" = "application/json") |>
    req_body_json(request_body) |>
    req_timeout(as.integer(timeout))
  resp <- req_perform(req)

  # Check status code
  if (resp$status_code != 200) {
    cli_status_clear(id = sb)
    cli_alert_danger(paste0("Request error: Status code ", resp$status_code))
    return(NULL)
  }

  cli_status_clear(id = sb)

  # Parse response
  response <- resp_body_json(resp)
  # Gemini structured response is in candidates[[1]]$content$parts[[1]]$text as a JSON string
  json_str <- response$candidates[[1]]$content$parts[[1]]$text
  # Use simplifyVector = FALSE to get a pure list (JSON-like structure)
  # result <- jsonlite::fromJSON(json_str, simplifyVector = FALSE)
  result <- json_str

  return(result)
}
