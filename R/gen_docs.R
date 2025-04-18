#' @title Generate Roxygen Documentation
#'
#' @description Generates Roxygen2 documentation for an R function based on the currently selected code.
#'
#' @param prompt  A character string specifying additional instructions for the LLM.  Defaults to a prompt requesting Roxygen2 documentation without the original code.
#' 
#' @return Invisibly returns the generated documentation string, but primarily inserts the text into the RStudio console.
#' @examples
#' \dontrun{
#' # Select your function code in the editor, then run:
#' gen_docs()
#' 
#' # For custom instructions:
#' gen_docs("Generate minimal Roxygen docs for this function")
#' }
#' 
#' @importFrom rstudioapi getActiveDocumentContext executeCommand insertText
#' @importFrom cli cli_alert_danger cli_status cli_status_clear
#' @export
gen_docs <- function(prompt = NULL) {
  if (is.null(prompt)) {
    prompt <-
      paste0(
        "Generate Roxygen documentation for an R function.",
        "\nReturn value should start with #' only",
        "\nAlso not include original input code in return value.",
        "\nDo not include ```r and ```"
      )
  }

  context <- getActiveDocumentContext()
  selectedCode <- context$selection[[1]]$text
  
  # 코드 선택 여부 확인
  if (nchar(trim(selectedCode)) == 0) {
    cli_alert_danger("No code selected. Please select an R function to document.")
    return(invisible(NULL))
  }

  # 상태 메시지 표시
  sb <- cli_status("Generating Roxygen documentation...")
  
  # API 호출 및 오류 처리
  description <- tryCatch({
    result <- gemini(
      prompt = paste0(
        prompt,
        "\n---",
        selectedCode
      )
    )
    
    if (is.null(result) || length(result) == 0 || nchar(trim(result)) == 0) {
      cli_status_clear(id = sb)
      cli_alert_danger("Failed to generate documentation or received empty response.")
      return(invisible(NULL))
    }
    
    result
  }, 
  error = function(e) {
    cli_status_clear(id = sb)
    cli_alert_danger(paste0("Error generating documentation: ", e$message))
    return(invisible(NULL))
  })
  
  # 오류 발생 시 조기 종료
  if (is.null(description)) {
    return(invisible(NULL))
  }
  
  cli_status_clear(id = sb)
  
  # 결과가 있으면 콘솔에 삽입
  executeCommand("activateConsole")
  insertText(text = description)
  
  # 생성된 문서를 보이지 않게 반환
  return(invisible(description))
}
