#' @title Count Tokens for Gemini Content (Including Images)
#' @description Calculates the token count for a given content, including text and image data, using the Vertex AI Gemini API.
#'
#' @param jsonkey A path to JSON file containing the service account key from Vertex AI.
#' @param model_id The ID of the Gemini model.
#' @param content The content (text, image, or list of text/image parts) for which to count tokens.
#'   - For text, provide a string.
#'   - For images, provide a list with `data` (base64 encoded image) and `mimeType` (e.g., "image/png", "image/jpeg").
#'   - For multiple content parts, provide a list where each element is either a text string or an image list.
#' @param region The Google Cloud region where your Vertex AI resources are located (default is "us-central1").
#'        See https://cloud.google.com/vertex-ai/docs/regions for available regions.
#'
#' @examples
#' \dontrun{
#' library(gemini.R)
#'
#' # For text content
#' key_file <- "YOURAPIKEY.json"
#' model <- "2.0-flash"
#' token_count_text <- countTokens(
#'   jsonkey = key_file, 
#'   model_id = model, 
#'   content = "Hello, world!"
#' )
#' print(token_count_text)
#'
#' # For image content (assuming 'image.jpg' is in your working directory)
#' image_data <- base64enc::base64encode("image.jpg")
#' image_content <- list(data = image_data, mimeType = "image/jpeg")
#' token_count_image <- countTokens(
#'   jsonkey = key_file,
#'   model_id = model,
#'   content = image_content
#' )
#' print(token_count_image)
#'
#' # For multiple content parts (text and image)
#' content_parts <- list(
#'   list(text = "This is the first part."),
#'   list(data = image_data, mimeType = "image/jpeg"),
#'   list(text = "This is the last part")
#' )
#' token_count_parts <- countTokens(
#'   jsonkey = key_file,
#'   model_id = model,
#'   content = content_parts
#' )
#' print(token_count_parts)
#' }
#'
#' @return A numeric value representing the token count of the content.
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom httr2 request req_headers req_body_json req_perform resp_body_json
#' @importFrom base64enc base64encode
#' @export
countTokens <- function(jsonkey = NULL, model_id = NULL, content = NULL, region = "us-central1") {
  stopifnot(!is.null(jsonkey), !is.null(model_id), !is.null(content))

  tokens <- token.vertex(jsonkey = jsonkey, model_id = model_id, region = region)
  access_token <- tokens$key

  project_id <- fromJSON(jsonkey)$project_id
  model_name <- paste0("gemini-", model_id)
  endpoint_url <- paste0(
    "https://", region, "-aiplatform.googleapis.com/v1/projects/",
    project_id, "/locations/", region, "/publishers/google/models/", model_name, ":countTokens"
  )

  # Helper function to process content parts
  process_content_part <- function(part) {
    if (is.null(part) || length(part) == 0) {
      stop("Invalid content part: found an empty part (NULL or empty list).")
    }
    if (!is.list(part) && !is.character(part)) {
      stop("Invalid content part: each part must be a character or a list.")
    }
    if (is.character(part)) {
      return(list(text = part))
    } else if (is.list(part) && all(c("data", "mimeType") %in% names(part))) {
      if (!is.character(part$data) || !is.character(part$mimeType)) {
        stop("Invalid image part : data and mimeType must be character.")
      }
      return(list(inlineData = list(data = part$data, mimeType = part$mimeType)))
    } else {
      stop("Invalid content part format. Must be a string (text) or a list with 'data' and 'mimeType' (image).")
    }
  }

  # Process content parts
  if (is.list(content) && !("data" %in% names(content) && "mimeType" %in% names(content))) {
    # check if the list is a nested list
    if (!all(sapply(content, is.list) | sapply(content, is.character))) {
      stop("Invalid content format : list must contain only list or character")
    }
    processed_parts <- lapply(content, process_content_part)
    body_data <- list(contents = list(parts = processed_parts))
  } else {
    processed_part <- process_content_part(content)
    body_data <- list(contents = list(parts = list(processed_part)))
  }

  resp <- request(endpoint_url) |>
    req_headers(
      "Content-Type" = "application/json",
      "Authorization" = paste("Bearer", access_token)
    ) |>
    req_body_json(body_data) |>
    req_perform() |>
    resp_body_json()

  return(resp$totalTokens)
}
