#' @title Summarize or analyze one or more local documents using Gemini API
#' @description
#' Summarize, compare, or analyze the content of one or more local documents (PDF, TXT, HTML, etc.) using the Gemini API.
#'
#' @param pdf_path Path(s) to the local file(s). Can be a character vector.
#' @param prompt The prompt to send to Gemini (e.g., "Summarize these documents").
#' @param type File type. One of "PDF", "JavaScript", "Python", "TXT", "HTML", "CSS", "Markdown", "CSV", "XML", "RTF". Default is "PDF".
#' @param api_key Gemini API key. Defaults to \code{Sys.getenv("GEMINI_API_KEY")}.
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
#'   type = "PDF"
#' )
#' }
#'
#' @importFrom httr2 request req_url_query req_headers req_body_json req_perform resp_body_json
#' @importFrom base64enc base64encode
#'
#' @export
#' @seealso https://ai.google.dev/gemini-api/docs/document-processing?lang=rest
gemini_docs <- function(pdf_path, prompt, type = "PDF", api_key = Sys.getenv("GEMINI_API_KEY")) {
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

  # Base64 encode all files
  file_parts <- lapply(pdf_path, function(path) {
    list(
      inline_data = list(
        mime_type = mime_type,
        data = base64enc::base64encode(path)
      )
    )
  })

  # Add the prompt as the last part
  parts <- c(file_parts, list(list(text = prompt)))

  # Prepare request body
  body <- list(
    contents = list(
      list(parts = parts)
    )
  )

  url <- "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.0-flash:generateContent"

  req <- httr2::request(url) |>
    httr2::req_url_query(key = api_key) |>
    httr2::req_headers("Content-Type" = "application/json") |>
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
}
