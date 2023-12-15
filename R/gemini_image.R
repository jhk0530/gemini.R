#' @title Generate text from text and image with Gemini
#' @description Generate text from text and image with Gemini
#' @param prompt The prompt to generate text
#' @param image The image to generate text
#'
#' @return Generated text
#' @export
#' @examples
#' gemini_image("Explain this image", "https://upload.wikimedia.org/wikipedia/commons/thumb/4/44/Sidney_Hall_-_Urania%27s_Mirror_-_Gemini.jpg/560px-Sidney_Hall_-_Urania%27s_Mirror_-_Gemini.jpg")
#'
#' @importFrom httr POST content_type_json
#' @importFrom base64enc base64encode
#'
#' @seealso https://ai.google.dev/docs/gemini_api_overview
#'

gemini_image <- function(prompt, image) {
  model_query <- "gemini-pro-vision::generateContent"

  response <- POST(
    url = paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_query),
    query = list(key = Sys.getenv("GEMINI_API_KEY")),
    content_type_json(),
    encode = "json",
    body = list(
      contents = list(
        parts = list(
          list(
            text = prompt
          ),
          list(
            inline_data = list(
              mime_type = "image/png",
              data = base64encode(image)
            )
          )
        )
      ),
      generationConfig = list(
        temperature = 0.5,
        maxOutputTokens = 1024
      )
    )
  )
  candidates <- content(response)$candidates
  outputs <- unlist(lapply(candidates, function(candidate) candidate$content$parts))
  return(outputs)
}
