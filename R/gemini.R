#' @title Generate text from text with Gemini
#' @description Generate text from text with Gemini
#' @param prompt The prompt to generate text from
#'
#' @return Generated text
#' @export
#' @examples
#' gemini("Explain dplyr's mutate function")
#'
#' @importFrom httr POST content content_type_json
#'
#' @seealso https://ai.google.dev/docs/gemini_api_overview
#'
gemini <- function(prompt) {
  model_query <- "gemini-pro:generateContent"

  response <- POST(
    url = paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_query),
    query = list(key = Sys.getenv("GEMINI_API_KEY")),
    content_type_json(),
    encode = "json",
    body = list(
      contents = list(
        parts = list(
          list(text = prompt)
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
