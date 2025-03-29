#' @title Generate and save image using Gemini
#' @description Generate an image using Gemini's image generation capabilities and save it to a file
#' @param prompt The prompt to generate an image from
#' @param filename The filename to save the image to. Default is "gemini_image.png"
#' @param overwrite Logical, whether to overwrite existing file. Default is TRUE
#' @param model The model to use. Default is "2.0-flash-exp-image-generation"
#' @param temperature The temperature to use. Default is 1
#' @param seed The seed to use. Default is 1234
#'
#' @return The path to the saved file
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
                      temperature = 1, seed = 1234) {

  if (is.null(prompt)) {
    cli_alert_danger("{.arg prompt} must not be NULL")
    return(NULL)
  }

  if (!is.character(prompt)) {
    cli_alert_danger("{.arg prompt} must be given as a STRING")
    return(NULL)
  }

  # Check if file exists and overwrite is FALSE
  if (file.exists(filename) && !overwrite) {
    cli_alert_warning("File already exists. Set overwrite = TRUE to replace it.")
    return(NULL)
  }

  # API Key check
  if (Sys.getenv("GEMINI_API_KEY") == "") {
    cli_alert_danger("Please set the {.envvar GEMINI_API_KEY} with {.fn setAPI} function.")
    return(NULL)
  }

  # Generate the image
  sb <- cli_status("Generating image with Gemini...")
  response <- gemini(prompt, model = model, temperature = temperature, seed = seed)
  cli_status_clear(id = sb)

  tryCatch({
    base64_data <- response[2]
    image_data <- base64enc::base64decode(base64_data)
    writeBin(image_data, filename)
    cli_alert_success(paste("Image saved to", filename))
    return(filename)
  }, error = function(e) {
    cli_alert_danger(paste("Error saving image:", e$message))
    return(NULL)
  })
}
