#' @title Generate Unit Tests for R Functions
#'
#' @description Generates unit test code for an R function using the Gemini AI model.
#'
#' @param prompt A character string specifying additional instructions for the Gemini model.
#'        If NULL, a default prompt requesting unit tests is used.
#'
#' @return Invisibly returns the generated test code, but primarily inserts it into the RStudio console.
#'
#' @examples
#' \dontrun{
#' # Select your function code in the editor, then run:
#' gen_tests()
#' 
#' # For custom instructions:
#' gen_tests("Generate comprehensive testthat tests with edge cases")
#' }
#'
#' @importFrom rstudioapi getActiveDocumentContext executeCommand insertText
#' @importFrom cli cli_alert_danger cli_status cli_status_clear
#'
#' @export
gen_tests <- function(prompt = NULL) {
  if (is.null(prompt)) {
    prompt <-
      paste0(
        "Generate unit test code for an R function.",
        "\nReturn only R code without markdown code blocks.",
        "\nDo not include ```r and ``` markers in the response.",
        "\nPreferably use testthat framework for testing."
      )
  }

  # 코드 선택 가져오기
  context <- tryCatch({
    getActiveDocumentContext()
  }, error = function(e) {
    cli_alert_danger("Failed to get active document context: ", e$message)
    return(NULL)
  })
  
  if (is.null(context)) {
    return(invisible(NULL))
  }
  
  selectedCode <- context$selection[[1]]$text
  
  # 코드 선택 여부 확인
  if (is.null(selectedCode) || nchar(trim(selectedCode)) == 0) {
    cli_alert_danger("No code selected. Please select an R function to generate tests for.")
    return(invisible(NULL))
  }

  # 상태 메시지 표시
  sb <- cli_status("Generating unit tests...")
  
  # API 호출 및 오류 처리
  test_code <- tryCatch({
    result <- gemini(
      prompt = paste0(
        prompt,
        "\n---\n",
        selectedCode
      )
    )
    
    if (is.null(result) || length(result) == 0 || nchar(trim(result)) == 0) {
      cli_status_clear(id = sb)
      cli_alert_danger("Failed to generate test code or received empty response.")
      return(invisible(NULL))
    }
    
    result
  }, 
  error = function(e) {
    cli_status_clear(id = sb)
    cli_alert_danger(paste0("Error generating test code: ", e$message))
    return(invisible(NULL))
  })
  
  # 오류 발생 시 조기 종료
  if (is.null(test_code)) {
    return(invisible(NULL))
  }
  
  cli_status_clear(id = sb)
  
  # 콘솔에 테스트 코드 삽입
  tryCatch({
    executeCommand("activateConsole")
    insertText(text = test_code)
  }, error = function(e) {
    cli_alert_danger(paste0("Failed to insert text into console: ", e$message))
  })
  
  # 생성된 테스트 코드를 보이지 않게 반환
  return(invisible(test_code))
}
