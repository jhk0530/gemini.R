#' @title Summarize or analyze one or more local documents using Gemini API
#' @description
#' Summarize, compare, or analyze the content of one or more local documents (PDF, TXT, HTML, etc.) using the Gemini API.
#'
#' @param pdf_path Path(s) to the local file(s). Can be a character vector.
#' @param prompt The prompt to send to Gemini (e.g., "Summarize these documents").
#' @param type File type. One of "PDF", "JavaScript", "Python", "TXT", "HTML", "CSS", "Markdown", "CSV", "XML", "RTF". Default is "PDF".
#' @param model The model to use. Default is '2.5-flash'.
#'              see https://ai.google.dev/gemini-api/docs/models/gemini
#' @param api_key Gemini API key. Defaults to \code{Sys.getenv("GEMINI_API_KEY")}. The API key is sent via the HTTP header \code{x-goog-api-key}. 
#' @param large Logical. If \code{TRUE}, use the file upload API for large files (only one file supported). Default is \code{FALSE}.
#' @param local Logical. If \code{TRUE}, treat \code{pdf_path} as a local file path. If \code{FALSE}, download from URL. Default is \code{FALSE}.
#'
#' @return The summary or response text from Gemini.
#'
#' @details
#' This function encodes one or more local files, sends them along with a prompt to the Gemini API, and returns the generated summary or response.
#'
#' @examples
#' \dontrun{
#' gemini_docs(
#'   pdf_path = c("doc1.pdf", "doc2.pdf"),
#'   prompt = "Compare these documents",
#'   type = "PDF",
#'   model = "2.5-flash"
#' )
#' }
#'
#' @importFrom httr2 request req_headers req_body_json req_perform resp_body_json
#' @importFrom base64enc base64encode
#' @importFrom utils download.file
#'
#' @export
#' @seealso https://ai.google.dev/gemini-api/docs/document-processing?lang=rest
gemini_docs <- function(pdf_path, prompt, type = "PDF", model = "2.5-flash", api_key = Sys.getenv("GEMINI_API_KEY"), large = FALSE, local = FALSE) {
  # If local = FALSE and input is a URL, download to a temp file
  temp_files <- character(0)
  if (!local) {
    # Download the file to a temp file
    temp_file <- tempfile(fileext = paste0(".", tools::file_ext(pdf_path)))
    download.file(pdf_path, temp_file, mode = "wb", quiet = TRUE)
    temp_files <- c(temp_files, temp_file)
    pdf_path <- temp_file

    # Remove temp files on exit
    if (length(temp_files) > 0) on.exit(unlink(temp_files), add = TRUE)
  }

  # Check file existence
  if (length(pdf_path) < 1) stop("At least one file path must be provided.")
  if (any(!file.exists(pdf_path))) stop("Some files do not exist: ", paste(pdf_path[!file.exists(pdf_path)], collapse = ", "))

  # Define supported mime types
  mime_types <- list(
    PDF = "application/pdf",
    JavaScript = c("application/x-javascript", "text/javascript"),
    Python = c("application/x-python", "text/x-python"),
    TXT = "text/plain",
    HTML = "text/html",
    CSS = "text/css",
    Markdown = "text/md",
    CSV = "text/csv",
    XML = "text/xml",
    RTF = "text/rtf"
  )

  # Check type validity
  if (!(type %in% names(mime_types))) {
    stop("Unsupported type. Supported types are: ", paste(names(mime_types), collapse = ", "))
  }

  # Use the first mime type if multiple are available
  mime_type <- if (is.character(mime_types[[type]])) mime_types[[type]][1] else as.character(mime_types[[type]][1])

  # Build model query URL
  model_query <- paste0("gemini-", model, ":generateContent")

  if (!large && !local) {
    # Base64 encode all files and send directly (for small files)
    file_parts <- lapply(pdf_path, function(path) {
      list(
        inline_data = list(
          mime_type = mime_type,
          data = base64enc::base64encode(path)
        )
      )
    })

    parts <- c(file_parts, list(list(text = prompt)))

    body <- list(
      contents = list(
        list(parts = parts)
      )
    )

    url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_query)

    req <- httr2::request(url) |>
      httr2::req_headers(
        "x-goog-api-key" = api_key,
        "Content-Type" = "application/json"
      ) |>
      httr2::req_body_json(body, auto_unbox = TRUE)

    resp <- httr2::req_perform(req)

    if (resp$status_code != 200) {
      stop(paste0("Error in Gemini API request: Status code ", resp$status_code))
    }

    result <- httr2::resp_body_json(resp)

    # Extract summary text (robust extraction of all $text fields)
    out <- tryCatch(
      {
        texts <- lapply(result$candidates[[1]]$content$parts, function(part) part$text)
        texts <- texts[!sapply(texts, is.null)]
        paste(texts, collapse = "\n")
      },
      error = function(e) {
        paste(unlist(result$candidates[[1]]$content$parts), collapse = "\n")
      }
    )
    return(out)
  } else {
    # Use file upload API for large or local files (only one file supported)
    if (length(pdf_path) > 1) stop("Large/Local mode supports only one file at a time.")
    file_uri <- upload_api(pdf_path[1], api_key, mime_type)

    # Build request body for Gemini API using file_uri
    body <- list(
      contents = list(list(
        parts = list(
          list(text = prompt),
          list(file_data = list(mime_type = mime_type, file_uri = file_uri))
        )
      ))
    )

    url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_query)

    req <- httr2::request(url) |>
      httr2::req_headers(
        "x-goog-api-key" = api_key,
        "Content-Type" = "application/json"
      ) |>
      httr2::req_body_json(body, auto_unbox = TRUE)

    resp <- httr2::req_perform(req)
    if (resp$status_code != 200) {
      stop(paste0("Error in Gemini API request: Status code ", resp$status_code))
    }

    result <- httr2::resp_body_json(resp)
    out <- tryCatch(
      {
        texts <- lapply(result$candidates[[1]]$content$parts, function(part) part$text)
        texts <- texts[!sapply(texts, is.null)]
        paste(texts, collapse = "\n")
      },
      error = function(e) {
        paste(unlist(result$candidates[[1]]$content$parts), collapse = "\n")
      }
    )
    return(out)
  }
}

#' @title Summarize or analyze documents using Vertex AI Gemini
#' @description
#' Summarize, compare, or analyze the content of one or more documents (PDF, TXT, HTML, etc.) using Vertex AI Gemini.
#'
#' @param file_uri The URI(s) or URL(s) of the file(s) to include in the prompt. Accepts Cloud Storage URI (gs://...), HTTP(S) URL, or YouTube video URL.
#' @param prompt The text instructions to include in the prompt.
#' @param mime_type The media type of the file (e.g., "application/pdf", "text/plain").
#' @param tokens A list containing the API URL and key from token.vertex() function.
#' @param temperature The temperature to use. Default is 1.
#' @param maxOutputTokens The maximum number of tokens to generate. Default is 8192.
#' @param topK The top-k value to use. Default is 40.
#' @param topP The top-p value to use. Default is 0.95.
#' @param seed The seed to use. Default is 1234.
#'
#' @return The summary or response text from Gemini Vertex.
#' @export
#' @examples
#' \dontrun{
#' tokens <- token.vertex()
#' gemini_docs.vertex(
#'   file_uri = "gs://cloud-samples-data/generative-ai/pdf/2403.05530.pdf",
#'   prompt = "Summarize this document.",
#'   mime_type = "application/pdf",
#'   tokens = tokens
#' )
#' }
#' @seealso https://cloud.google.com/vertex-ai/docs/generative-ai/multimodal/send-request-document

gemini_docs.vertex <- function(file_uri, prompt, mime_type = "application/pdf", tokens = NULL,
                               temperature = 1, maxOutputTokens = 8192, topK = 40, topP = 0.95, seed = 1234) {
  # Validate input parameters
  if (missing(file_uri) || length(file_uri) < 1) {
    stop("At least one file_uri must be provided.")
  }
  if (missing(prompt) || !nzchar(prompt)) {
    stop("A non-empty prompt must be provided.")
  }
  if (is.null(tokens) || is.null(tokens$url) || is.null(tokens$key)) {
    stop("tokens must be provided with 'url' and 'key'.")
  }

  # Build parts for each file_uri
  file_parts <- lapply(file_uri, function(uri) {
    list(
      fileData = list(
        fileUri = uri,
        mimeType = mime_type
      )
    )
  })

  # Add the prompt as the last part
  parts <- c(file_parts, list(list(text = prompt)))

  # Build request body
  request_body <- list(
    contents = list(
      list(
        role = "user",
        parts = parts
      )
    ),
    generationConfig = list(
      temperature = temperature,
      maxOutputTokens = maxOutputTokens,
      topP = topP,
      topK = topK,
      seed = seed
    )
  )

  # Send request to Vertex AI Gemini
  req <- httr2::request(tokens$url) |>
    httr2::req_headers(
      "Authorization" = paste0("Bearer ", tokens$key),
      "Content-Type" = "application/json"
    ) |>
    httr2::req_body_json(request_body)

  resp <- httr2::req_perform(req)

  # Check response status
  if (resp$status_code != 200) {
    stop(paste0("Error in Vertex Gemini API request: Status code ", resp$status_code))
  }

  # Parse and return the response
  result <- httr2::resp_body_json(resp)
  candidates <- result$candidates
  outputs <- unlist(lapply(candidates, function(candidate) candidate$content$parts))
  return(outputs)
}

# Helper function for file upload API (Gemini/Vertex)
upload_api <- function(local_file, api_key, mime_type) {
  # Start resumable upload session
  file_size <- file.info(local_file)$size
  meta_body <- list(file = list(display_name = basename(local_file)))
  meta_req <- httr2::request("https://generativelanguage.googleapis.com/upload/v1beta/files") |>
    httr2::req_headers(
      "x-goog-api-key" = api_key,
      "X-Goog-Upload-Protocol" = "resumable",
      "X-Goog-Upload-Command" = "start",
      "X-Goog-Upload-Header-Content-Length" = as.character(file_size),
      "X-Goog-Upload-Header-Content-Type" = mime_type,
      "Content-Type" = "application/json"
    ) |>
    httr2::req_body_json(meta_body, auto_unbox = TRUE)

  meta_resp <- httr2::req_perform(meta_req)
  if (meta_resp$status_code != 200) {
    stop(paste0("Error starting upload session: Status code ", meta_resp$status_code))
  }
  upload_url <- httr2::resp_headers(meta_resp)[["x-goog-upload-url"]]
  if (is.null(upload_url)) stop("Failed to get upload URL from Gemini API.")

  # Upload the file bytes
  upload_req <- httr2::request(upload_url) |>
    httr2::req_headers(
      "Content-Length" = as.character(file_size),
      "X-Goog-Upload-Offset" = "0",
      "X-Goog-Upload-Command" = "upload, finalize"
    ) |>
    httr2::req_body_file(local_file)

  upload_resp <- httr2::req_perform(upload_req)
  if (upload_resp$status_code != 200) {
    stop(paste0("Error uploading file: Status code ", upload_resp$status_code))
  }
  upload_result <- httr2::resp_body_json(upload_resp)
  file_uri <- upload_result$file$uri
  if (is.null(file_uri)) stop("Failed to get file_uri after upload.")
  return(file_uri)
}
