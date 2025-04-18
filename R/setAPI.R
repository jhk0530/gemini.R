#' @title Set Gemini API Key
#' @description Sets the Gemini API key as an environment variable for use in API calls.
#' @param api_key A character string containing your Gemini API key.
#' @return No return value, called for side effects.
#' @export
#' @examples
#' \dontrun{
#' setAPI("YOUR_API_KEY")
#' }
#' @seealso https://makersuite.google.com/app/apikey
#' @note Please be aware you have to agree to the terms of service of the API provider.
#'   Any app that uses the API key is subject to the terms of service.
#'   Also, please be aware that the API key is a sensitive information.
#' @importFrom cli cli_alert_info cli_div cli_end cli_alert cli_alert_danger

setAPI <- function(api_key) {
  # API 키 유효성 검사
  if (is.null(api_key) || !is.character(api_key) || nchar(api_key) == 0) {
    cli_alert_danger("API key must be a non-empty string.")
    return(invisible())
  }
  
  # 일반적인 API 키 길이 확인 (Google API 키는 보통 39자)
  if (nchar(api_key) < 10) {
    cli_alert_danger("API key seems too short. Please verify your key.")
    return(invisible())
  }
  
  # 안전하게 API 키 마지막 부분만 표시
  last_chars <- 4
  if (nchar(api_key) > last_chars) {
    last <- substr(api_key, nchar(api_key) - (last_chars - 1), nchar(api_key))
  } else {
    # API 키가 너무 짧은 경우, 마지막 문자만
    last <- substr(api_key, nchar(api_key), nchar(api_key))
  }
  
  # 환경 변수 설정
  Sys.setenv(GEMINI_API_KEY = api_key)

  # 사용자 피드백 제공
  cli_div(theme = list(span.str = list("background-color" = "blue")))
  cli_alert_info("API key {.str ...{last}} is set.")
  cli_end()

  cli_alert("You may try {.run gemini_chat('What is CRAN?')}")
  
  return(invisible())
}
