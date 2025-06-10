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
  response <- tryCatch({
    gemini(
      prompt = prompt, 
      model = model, 
      temperature = temperature, 
      maxOutputTokens = maxOutputTokens,
      topK = topK,
      topP = topP,
      seed = seed
    )
  }, error = function(e) {
    cli_status_clear(id = sb)
    cli_alert_danger(paste("Error generating image:", e$message))
    return(NULL)
  })
  
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
  tryCatch({
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
  }, error = function(e) {
    cli_alert_danger(paste("Error saving image:", e$message))
    return(NULL)
  })
}
