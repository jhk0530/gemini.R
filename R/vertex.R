#' @title Generate Gemini Access Token and Endpoint URL
#' @description Generates an access token for the Gemini model and constructs the corresponding endpoint URL.
#'
#' @param jsonkey A path to JSON file containing the service account key from Vertex AI.
#' @param model_id The ID of the Gemini model. This will be prepended with "gemini-".
#' @param expTime The expiration time of the access token in seconds (default is 3600 seconds, or 1 hour).
#' @param region The Google Cloud region where your Vertex AI resources are located (default is "us-central1").
#'        See https://cloud.google.com/vertex-ai/docs/general/locations for available regions.
#'
#' @examples
#' \dontrun{
#' library(gemini.R)
#' tokens <- token.vertex(jsonkey = "YOURAPIKEY.json", model_id = "1.5-flash")
#'
#' # Specify a different region
#' tokens <- token.vertex(jsonkey = "YOURAPIKEY.json", model_id = "1.5-flash", region = "europe-west4")
#' }
#'
#' @return A list containing:
#' \item{key}{The generated access token.}
#' \item{url}{The endpoint URL for the Gemini model.}
#'
#' @importFrom jsonlite fromJSON
#' @importFrom httr2 jwt_claim jwt_encode_sig request req_body_form req_perform resp_body_json
#' @importFrom cli cli_alert_danger cli_alert_info cli_status cli_status_clear
#'
#' @export
token.vertex <- function(jsonkey = NULL, model_id = NULL, expTime = 3600, region = "us-central1") {
  # 1. Add parameter validation
  if (is.null(jsonkey)) {
    cli_alert_danger("JSON key file path must be provided.")
    return(NULL)
  }

  if (!file.exists(jsonkey)) {
    cli_alert_danger(paste0("JSON key file not found: ", jsonkey))
    return(NULL)
  }

  if (is.null(model_id)) {
    cli_alert_danger("Model ID must be provided.")
    return(NULL)
  }

  # 2. Add status message
  sb <- cli_status("Authenticating with Vertex AI...")

  # 3. Strengthen error handling
  account <- tryCatch(
    {
      fromJSON(jsonkey)
    },
    error = function(e) {
      cli_status_clear(id = sb)
      cli_alert_danger(paste0("Error reading JSON key file: ", e$message))
      return(NULL)
    }
  )

  if (is.null(account)) {
    return(NULL)
  }

  # Check required fields
  required_fields <- c("client_email", "private_key", "project_id")
  missing_fields <- required_fields[!required_fields %in% names(account)]

  if (length(missing_fields) > 0) {
    cli_status_clear(id = sb)
    cli_alert_danger(paste0(
      "JSON key file missing required fields: ",
      paste(missing_fields, collapse = ", ")
    ))
    return(NULL)
  }

  project_id <- account$project_id

  model_id <- paste0("gemini-", model_id, ":generateContent")

  endpoint_url <- paste0(
    "https://",
    region,
    "-aiplatform.googleapis.com/v1/projects/",
    project_id,
    "/locations/", region, "/publishers/google/models/",
    model_id
  )

  token_url <- "https://oauth2.googleapis.com/token"

  iat <- as.numeric(Sys.time())

  jwt_claims <- jwt_claim(
    iss = account$client_email, # Service account email
    scope = "https://www.googleapis.com/auth/cloud-platform", # Scope
    aud = token_url,
    iat = iat,
    exp = iat + expTime
  )

  # 4. Add error handling for JWT token creation
  jwt_token <- tryCatch(
    {
      jwt_encode_sig(jwt_claims, account$private_key)
    },
    error = function(e) {
      cli_status_clear(id = sb)
      cli_alert_danger(paste0("Error creating JWT token: ", e$message))
      return(NULL)
    }
  )

  if (is.null(jwt_token)) {
    return(NULL)
  }

  # 5. Add error handling for API call
  resp <- tryCatch(
    {
      request(token_url) |>
        req_body_form(
          grant_type = "urn:ietf:params:oauth:grant-type:jwt-bearer",
          assertion = jwt_token
        ) |>
        req_perform()
    },
    error = function(e) {
      cli_status_clear(id = sb)
      cli_alert_danger(paste0("Error requesting access token: ", e$message))
      return(NULL)
    }
  )

  if (is.null(resp)) {
    return(NULL)
  }

  # 6. Check response status code
  if (resp$status_code != 200) {
    cli_status_clear(id = sb)
    cli_alert_danger(paste0("Error response from token server: Status code ", resp$status_code))
    return(NULL)
  }

  # 7. Add error handling for JSON response processing
  token_data <- tryCatch(
    {
      resp_body_json(resp)
    },
    error = function(e) {
      cli_status_clear(id = sb)
      cli_alert_danger(paste0("Error processing JSON response: ", e$message))
      return(NULL)
    }
  )

  if (is.null(token_data)) {
    return(NULL)
  }

  access_token <- token_data$access_token

  cli_status_clear(id = sb)
  cli_alert_info("Authentication successful.")

  return(list(
    key = access_token,
    url = endpoint_url
  ))
}
