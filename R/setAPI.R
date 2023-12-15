#' @title Set API key
#' @description Set API key as an environment variable
#' @param api_key The API key to set
#' @return NULL
#' @export
#' @examples
#' setAPI("my_api_key")
#'
#' @seealso https://makersuite.google.com/app/apikey
#'

setAPI <- function(api_key){
  api_key <- Sys.setenv(GEMINI_API_KEY = api_key)
}
