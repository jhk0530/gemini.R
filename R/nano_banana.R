#' @title Generate, edit, or transfer images using Gemini API
#' @description
#' Generate a new image, edit an existing image, or transfer styles/content between two images using the Gemini API (aka Nano Banana).
#' This function supports image generation from text, image editing with a prompt and a base image, and image transfer between two images.
#' @param prompt Character. The prompt describing the image to generate or edit.
#' @param type Character. The type of operation: "generate" (text-to-image), "edit" (image editing), "transfer" (image-to-image).
#' @param img_path Character. Path to the input image PNG file.
#' @param img_path2 Character. Path to the second input image PNG file.
#' @param output_path Character. The filename to save the result image.
#' @return The path to the saved image file, or NULL if an error occurred.
#' @examples
#' \dontrun{
#' # Generate an image from text
#' prompt <- "Create a picture of a nano banana dish in a fancy restaurant with a Gemini theme"
#' nano_banana(prompt, output_path = "gemini-native-image.png")
#'
#' # Edit an image with a prompt (continued from generate)
#' prompt <- paste(
#'   "Create a picture of my cat eating a nano-banana",
#'   "in a restaurant under the Gemini constellation"
#' )
#' nano_banana(
#'   prompt,
#'   type = "edit",
#'   img_path = "gemini-native-image.png",
#'   output_path = "edited_image.png"
#' )
#'
#' # Transfer style/content between two images
#' prompt <- paste(
#'   "Take the blue floral dress from the first image",
#'   "and let the woman from the second image wear it."
#' )
#' nano_banana(
#'   prompt,
#'   type = "transfer",
#'   img_path = "dress.png",
#'   img_path2 = "model.png",
#'   output_path = "transferred_image.png"
#' )
#' }
#' @export
nano_banana <- function(prompt, type = "generate", img_path = NULL, img_path2 = NULL, output_path) {
  if (missing(output_path) || is.null(output_path) || !nzchar(output_path)) {
    cli::cli_alert_danger("output_path is required.")
    return(NULL)
  }

  if (!type %in% c("generate", "edit", "transfer")) {
    cli::cli_alert_danger("Invalid type: '{type}'. Must be one of 'generate', 'edit', or 'transfer'.")
    return(NULL)
  }

  url <- "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash-image-preview:generateContent"

  # Default (type == "generate")
  body <- init_body() |>
    add_text(prompt)

  if (type == "edit") {
    img <- read_image(img_path)
    if (is.null(img)) {
      cli::cli_alert_danger("Failed to read image from img_path: file does not exist or is invalid.")
      return(NULL)
    }
    body <- body |>
      add_inline_data("image/png", img)
  }

  if (type == "transfer") {
    img1 <- read_image(img_path)
    if (is.null(img1)) {
      cli::cli_alert_danger("Failed to read image from img_path: file does not exist or is invalid.")
      return(NULL)
    }
    img2 <- read_image(img_path2)
    if (is.null(img2)) {
      cli::cli_alert_danger("Failed to read image from img_path2: file does not exist or is invalid.")
      return(NULL)
    }

    body <- body |>
      add_inline_data("image/png", img1) |>
      add_inline_data("image/png", img2)
  }

  res_txt <- gemini_request(url, body)
    
  output <- save_image(res_txt, output_path)

  if (is.null(output)) {
    cli::cli_alert_danger("Failed to save image to output_path.")
    return(NULL)  # Check for failure and return NULL explicitly
  }

  return(normalizePath(output))
}
