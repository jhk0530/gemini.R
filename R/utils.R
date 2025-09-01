#' @title Validate Gemini API parameters
#' @description Helper function to validate parameters for Gemini API calls
#' @param prompt The prompt text to validate
#' @param model The model name to validate
#' @param temperature The temperature value to validate
#' @param topP The topP value to validate
#' @param topK The topK value to validate
#' @param seed The seed value to validate
#' @param api_key Whether to check for API key (TRUE/FALSE)
#' @param tokens The tokens object for vertex API (optional, provide NULL if not applicable)
#' @return TRUE if all validations pass, otherwise the function stops execution with an error message
#' @keywords internal
#' @importFrom cli cli_alert_danger

validate_params <- function(prompt, model, temperature = 1, topP = 0.95,
                            topK = 40, seed = 1234, api_key = TRUE, tokens = NULL) {
  # Validate prompt
  if (is.null(prompt)) {
    cli_alert_danger("{.arg prompt} must not be NULL")
    return(FALSE)
  }

  if (!is.character(prompt)) {
    cli_alert_danger("{.arg prompt} must be given as a STRING")
    return(FALSE)
  }

  # if api_key, check whether model is NULL
  if (api_key && is.null(model)) {
    cli_alert_danger("{.arg model} must not be NULL")
    return(FALSE)
  }

  # API Key validation
  if (api_key && Sys.getenv("GEMINI_API_KEY") == "") {
    cli_alert_danger("Please set the {.envvar GEMINI_API_KEY} with {.fn setAPI} function.")
    return(FALSE)
  }

  # Tokens validation for vertex API - when API key is not used
  if (!api_key && is.null(tokens)) {
    cli_alert_danger("{.arg tokens} must not be NULL. Use token.vertex() function to generate tokens.")
    return(FALSE)
  }

  # Parameter validations
  if (temperature < 0 | temperature > 2) {
    cli_alert_danger("Error: Parameter 'temperature' must be between 0 and 2")
    return(FALSE)
  }

  if (topP < 0 | topP > 1) {
    cli_alert_danger("Error: Parameter 'topP' must be between 0 and 1")
    return(FALSE)
  }

  if (topK < 0 | topK > 100) {
    cli_alert_danger("Error: Parameter 'topK' must be between 0 and 100")
    return(FALSE)
  }

  if (!is.numeric(seed) || seed %% 1 != 0) {
    cli_alert_danger("Error: Parameter 'seed' must be an integer")
    return(FALSE)
  }

  # All validations passed
  return(TRUE)
}

#' @title Trim whitespace from string
#' @description Removes leading and trailing whitespace from a string
#' @param x Character string to trim
#' @return Character string with leading and trailing whitespace removed
#' @keywords internal
trim <- function(x) {
  gsub("^\\s+|\\s+$", "", x)
}

#' @title Read and encode image file as base64
#' @description
#' Reads an image file from the given path and encodes it as a base64 string.
#' Used internally for Gemini API image upload.
#' @param img_path Path to the image file.
#' @return Base64-encoded string of the image, or NULL if the file does not exist.
#' @importFrom cli cli_alert_danger
#' @importFrom base64enc base64encode
#' @keywords internal
#' @examples
#' \dontrun{
#' base64_img <- read_image("cat.png")
#' }
read_image <- function(img_path) {
  if (is.null(img_path) || !file.exists(img_path)) {
    cli::cli_alert_danger("Image file does not exist: {img_path}")
    return(NULL)
  }

  img_path <- normalizePath(img_path)
  img_bin <- readBin(img_path, what = "raw", n = file.info(img_path)$size)
  img_base64 <- base64enc::base64encode(img_bin)
  return(img_base64)
}

#' @title Send POST request to Gemini API
#' @description
#' Sends a POST request to the Gemini API with the specified URL and body, and returns the response as a character string.
#' @param url Character. The Gemini API endpoint URL.
#' @param body List. The request body to send as JSON.
#' @return Character string containing the API response.
#' @keywords internal
#' @importFrom httr2 request req_url_query req_headers req_body_json req_perform resp_body_string
#' @examples
#' \dontrun{
#' body <- list(contents = list(list(parts = list(list(text = "Hello!")))))
#' gemini_request("https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash-image-preview:generateContent", body)
#' }
gemini_request <- function(url, body) {
  api_key <- Sys.getenv("GEMINI_API_KEY")
  # tryCatch for error handling
  res_txt <- tryCatch({
    resp <- httr2::request(url) |>
      httr2::req_url_query(key = api_key) |>
      httr2::req_headers("Content-Type" = "application/json") |>
      httr2::req_body_json(body) |>
      httr2::req_perform()
    httr2::resp_body_string(resp)
  }, error = function(e) {
    cli::cli_alert_danger(paste("API request failed:", e$message))
    return(NULL)
  })
  return(res_txt)
}

#' @title Extract and save image from Gemini API response
#' @description
#' Extracts base64-encoded image data from the Gemini API response text and saves it as a binary image file.
#' @param res_txt Character string. The raw response text from the Gemini API.
#' @param output_path Character string. The file path to save the decoded image.
#' @return The path to the saved image file, or NULL if no image data was found.
#' @keywords internal
#' @examples
#' \dontrun{
#' save_image(res_txt, "output.png")
#' }
save_image <- function(res_txt, output_path) {    
  if (is.null(output_path)) {
    cli::cli_alert_danger("`output_path` must be provided to save the image.")
    return(NULL)
  }
  
  parsed_resp <- tryCatch(jsonlite::fromJSON(res_txt, simplifyVector = FALSE), error = function(e) NULL)
  
  if (is.null(parsed_resp) ||
      is.null(parsed_resp$candidates) ||
      is.null(parsed_resp$candidates[[1]]$content$parts[[1]]$inlineData$data)) {
    cli::cli_alert_danger("No image data found in response.")
    return(NULL)
  }
  base64_data <- parsed_resp$candidates[[1]]$content$parts[[1]]$inlineData$data
  
  img_out <- base64enc::base64decode(base64_data)
  writeBin(img_out, output_path)
  cli::cli_alert_success("Image saved to {output_path}")
  return(output_path)
}


#' @title Initialize Gemini API request body
#' @description
#' Creates an empty body structure for Gemini API requests.
#' @return A list representing an empty Gemini API request body.
#' @keywords internal
init_body <- function() {
  list(
    contents = list(
      list(
        parts = list()
      )
    )
  )
}

#' @title Add text prompt to Gemini API body
#' @description
#' Appends a text prompt part to the Gemini API request body.
#' @param body A list representing the current Gemini API request body.
#' @param prompt Character. The text prompt to add.
#' @return The updated body list with the text prompt added.
#' @keywords internal
add_text <- function(body, prompt) {
  body$contents[[1]]$parts <- append(body$contents[[1]]$parts, list(list(text = prompt)))
  return(body)
}

#' @title Add inline image data to Gemini API body
#' @description
#' Appends an inline_data part (image) to the Gemini API request body.
#' @param body A list representing the current Gemini API request body.
#' @param mime_type Character. The MIME type of the image (e.g., "image/png").
#' @param data Character. The base64-encoded image data.
#' @return The updated body list with the inline image data added.
#' @keywords internal
add_inline_data <- function(body, mime_type, data) {
  body$contents[[1]]$parts <- append(
    body$contents[[1]]$parts,
    list(list(inline_data = list(mime_type = mime_type, data = data)))
  )
  return(body)
}
