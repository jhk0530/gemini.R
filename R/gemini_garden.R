#' @title Interact with Vertex AI Model Garden
#' @description
#' This function sends a PDF file to the Vertex AI Model Garden (Mistral model) for processing, such as OCR. The PDF is encoded as base64 and sent to the rawPredict endpoint. The function is designed for future extension to support other document types and tasks.
#'
#' @param token Token object (e.g., from \code{token.vertex()}) containing the access token, region, and model_id.
#' @param project_id Google Cloud project ID.
#' @param pdf_path Path to the PDF file to be processed.
#'
#' @return A parsed list containing the results from the Vertex AI API (e.g., OCR results).
#'
#' @details
#' The PDF file is read and encoded as base64, then sent to the Vertex AI rawPredict endpoint for processing using a Mistral model. This function is structured for future extension to support other document types and model tasks available in Vertex AI Model Garden.
#'
#' For more information about available models, endpoints, and supported tasks, see \href{https://cloud.google.com/vertex-ai/generative-ai/docs/model-garden/explore-models}{Vertex AI Model Garden documentation}.
#'
#' @examples
#' \dontrun{
#' # Issue a token using token.vertex() first
#' my_token <- token.vertex(
#'   jsonkey = "your-service-account.json",
#'   region = "us-central1",
#'   model_id = "mistral-ocr-2505"
#' )
#' result <- gemini_garden(
#'   token = my_token,
#'   project_id = "your-project-id",
#'   pdf_path = "sample.pdf"
#' )
#' print(result)
#' }
#'
#' @importFrom jsonlite base64_enc
#' @seealso \url{https://cloud.google.com/vertex-ai/generative-ai/docs/model-garden/explore-models}
#' @importFrom httr2 request req_headers req_body_json req_perform resp_body_string
#' @importFrom jsonlite toJSON fromJSON
#'
#' @export
gemini_garden <- function(token, project_id, pdf_path) {
  # Extract access token from token object
  access_token <- token$key

  # Extract region from /locations/{region}/ in the URL
  region <- sub(".*/locations/([^/]+).*", "\\1", token$url)
  # Extract model_id using regular expression
  model_id <- sub(".*/models/([^:/]+).*", "\\1", token$url)
  # Remove "gemini-" prefix from model_id if present
  model_id <- sub("^gemini-", "", model_id)

  # Construct endpoint URL for rawPredict (for mistral models)
  url <- sprintf(
    "https://%s-aiplatform.googleapis.com/v1/projects/%s/locations/%s/publishers/mistralai/models/%s:rawPredict",
    region, project_id, region, model_id
  )

  # Read and encode PDF file as base64
  pdf_bin <- readBin(pdf_path, "raw", file.info(pdf_path)$size)
  base64_pdf <- base64_enc(pdf_bin)

  # Build request payload
  payload <- list(
    model = model_id,
    document = list(
      type = "document_url",
      document_url = paste0("data:application/pdf;base64,", base64_pdf)
    ),
    include_image_base64 = TRUE
  )

  # Make the API request using httr2
  req <- httr2::request(url) |>
    httr2::req_headers(
      Authorization = paste("Bearer", access_token),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_json(payload)

  resp <- httr2::req_perform(req)

  # Output the response as a parsed list
  response_text <- httr2::resp_body_string(resp)
  return(jsonlite::fromJSON(response_text, flatten = TRUE))
}
