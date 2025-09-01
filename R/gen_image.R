#' @title Generate and save image using Gemini
#' @description Generate an image using Gemini's image generation capabilities and save it to a file
#' @param prompt The prompt to generate an image from
#' @param filename The filename to save the image to. Default is "gemini_image.png"
#' @param overwrite Logical, whether to overwrite existing file. Default is TRUE
#' @param model The model to use. Default is "2.0-flash-exp-image-generation"
#' @param temperature The temperature to use. Default is 1 value should be between 0 and 2
#'              see https://ai.google.dev/gemini-api/docs/models/generative-models#model-parameters
#' @param maxOutputTokens The maximum number of tokens to generate.
#'              Default is 8192 and 100 tokens correspond to roughly 60-80 words.
#' @param topK The top-k value to use. Default is 40 value should be between 0 and 100
#'              see https://ai.google.dev/gemini-api/docs/models/generative-models#model-parameters
#' @param topP The top-p value to use. Default is 0.95 value should be between 0 and 1
#'              see https://ai.google.dev/gemini-api/docs/models/generative-models#model-parameters
#' @param seed The seed to use. Default is 1234 value should be integer
#'              see https://ai.google.dev/gemini-api/docs/models/generative-models#model-parameters
#'
#' @return The path to the saved file or NULL if an error occurred
#' @export
#' @examples
#' \dontrun{
#' library(gemini.R)
#' setAPI("YOUR_API_KEY")
#' gen_image("Create an image of a cat wearing sunglasses")
#' }
#' @importFrom base64enc base64decode
#' @importFrom cli cli_alert_success cli_alert_danger cli_alert_warning cli_status cli_status_clear
gen_image <- function(prompt, filename = "gemini_image.png", overwrite = TRUE,
                      model = "2.0-flash-exp-image-generation",
                      temperature = 1, maxOutputTokens = 8192,
                      topK = 40, topP = 0.95, seed = 1234) {
  # 1. Validate parameters using validate_params function
  if (!validate_params(prompt, model, temperature, topP, topK, seed, api_key = TRUE)) {
    return(NULL)
  }

  # Check if the model is for image generation
  if (model != "2.0-flash-exp-image-generation") {
    cli_alert_danger("Error: For image generation, model must be '2.0-flash-exp-image-generation'")
    return(NULL)
  }

  # Check if file exists and overwrite is FALSE
  if (file.exists(filename) && !overwrite) {
    cli_alert_warning("File already exists. Set overwrite = TRUE to replace it.")
    return(NULL)
  }

  # Generate the image
  sb <- cli_status("Generating image with Gemini...")

  # 2. Add error handling for response
  response <- tryCatch(
    {
      gemini(
        prompt = prompt,
        model = model,
        temperature = temperature,
        maxOutputTokens = maxOutputTokens,
        topK = topK,
        topP = topP,
        seed = seed
      )
    },
    error = function(e) {
      cli_status_clear(id = sb)
      cli_alert_danger(paste("Error generating image:", e$message))
      return(NULL)
    }
  )

  # 3. Handle case when response is NULL
  if (is.null(response)) {
    cli_status_clear(id = sb)
    cli_alert_danger("Failed to generate image - no response received")
    return(NULL)
  }

  cli_status_clear(id = sb)

  # 4. Add validation for response structure
  if (length(response) < 2) {
    cli_alert_danger("Invalid response format - missing image data")
    return(NULL)
  }

  # 5. Improve error handling for file saving
  tryCatch(
    {
      # Extract image data part (second element)
      base64_data <- response[2]

      # Add check for empty data
      if (is.null(base64_data) || nchar(base64_data) == 0) {
        cli_alert_danger("Empty image data received")
        return(NULL)
      }

      # Base64 decode
      image_data <- base64enc::base64decode(base64_data)

      # Write file
      writeBin(image_data, filename)
      cli_alert_success(paste("Image saved to", filename))
      return(filename)
    },
    error = function(e) {
      cli_alert_danger(paste("Error saving image:", e$message))
      return(NULL)
    }
  )
}

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
#' @details
#' - For type = "generate", only prompt and output_path are required.
#' - For type = "edit", prompt, img_path, and output_path are required.
#' - For type = "transfer", prompt, img_path, img_path2, and output_path are required.
#' @examples
#' \dontrun{
#' # Generate an image from text
#' prompt <- "Create a picture of a nano banana dish in a fancy restaurant with a Gemini theme"
#' nano_banana(prompt, output_path = "gemini-native-image.png")
#'
#' # Edit an image with a prompt (continued from generate)
#' prompt <- "Create a picture of my cat eating a nano-banana in a fancy restaurant under the Gemini constellation"
#' nano_banana(prompt, type = "edit", img_path = "gemini-native-image.png", output_path = "edited_image.png")
#'
#' # Transfer style/content between two images
#' prompt <- "Take the blue floral dress from the first image and let the woman from the second image wear it."
#' nano_banana(prompt, type = "transfer", img_path = "dress.png", img_path2 = "model.png", output_path = "transferred_image.png")
#' }
#' @export
nano_banana <- function(prompt, type = 'generate', img_path = NULL, img_path2 = NULL, output_path) {
  if (missing(output_path) || is.null(output_path) || !nzchar(output_path)) {
    cli::cli_alert_danger("output_path is required.")
    return(NULL)
  }

  url <- "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash-image-preview:generateContent"

  # Default (type == "generate")
  body <- init_body() |>
    add_text(prompt)

  if (type == "edit") {
    img <- read_image(img_path)
    body <- body |>
      add_inline_data("image/png", img)
  }

  if (type == "transfer") {
    img1 <- read_image(img_path)
    img2 <- read_image(img_path2)

    body <- body |>
      add_inline_data("image/png", img1) |>
      add_inline_data("image/png", img2)
  }

  res_txt <- gemini_request(url, body)
  output <- save_image(res_txt, output_path)
  return(normalizePath(output))
}
