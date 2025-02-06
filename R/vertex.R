#' @title Generate Gemini Access Token and Endpoint URL
#' @description Generates an access token for the Gemini model and constructs the corresponding endpoint URL.
#'
#' @param jsonkey A path to JSON file containing the service account key from Vertex AI.
#' @param model_id The ID of the Gemini model. This will be prepended with "gemini-".
#' @param expTime The expiration time of the access token in seconds (default is 3600 seconds, or 1 hour).
#'
#' @examples
#' \dontrun{
#' library(gemini.R)
#' tokens <- token.vertex(jsonkey = "YOURAPIKEY.json", model_id = "1.5-flash")
#'
#' }
#'
#'
#' @return A list containing:
#' \item{key}{The generated access token.}
#' \item{url}{The endpoint URL for the Gemini model.}
#'
#' @importFrom jsonlite fromJSON
#' @importFrom httr2 jwt_claim jwt_encode_sig request req_body_form req_perform resp_body_json
#'
#' @export
token.vertex <- function(jsonkey = NULL, model_id = NULL, expTime = 3600){
  account <- fromJSON(jsonkey)
  project_id <- account$project_id

  model_id <- paste0('gemini-', model_id, ':generateContent')

  endpoint_url <- paste0(
    "https://us-central1-aiplatform.googleapis.com/v1/projects/",
    project_id,
    "/locations/us-central1/publishers/google/models/",
    model_id
  )

  token_url <- "https://oauth2.googleapis.com/token"

  iat <- as.numeric(Sys.time())

  jwt_claims <- jwt_claim(
    iss = account$client_email,  # 서비스 계정 이메일
    scope = "https://www.googleapis.com/auth/cloud-platform",  # 권한 범위
    aud = token_url,
    iat = iat,
    exp = iat + expTime
  )

  jwt_token <- jwt_encode_sig(jwt_claims, account$private_key)

  resp <- request(token_url) |>
    req_body_form(
      grant_type = "urn:ietf:params:oauth:grant-type:jwt-bearer",
      assertion = jwt_token
    ) |>
    req_perform() |>
    resp_body_json()

  access_token <- resp$access_token

  return(list(
    key = access_token,
    url = endpoint_url
  ))
}
