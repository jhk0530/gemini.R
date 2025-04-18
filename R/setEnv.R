#' @title Store API key in local environment file
#' @description Saves the API key to a local .Renviron file for persistent access across R sessions
#'
#' @param api_key The API key to store
#' @param overwrite Whether to overwrite the existing API key if already present in .Renviron (default: TRUE)
#' @param install_message Whether to display a message about how to use the API (default: TRUE)
#'
#' @return No return value, called for side effects.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' setEnv("your_api_key")
#' }
#'
#' @seealso \code{\link{setAPI}} which sets the API key for the current session only
#'
#' @importFrom cli cli_alert_success cli_alert_info cli_alert_warning cli_div cli_end cli_alert cli_alert_danger
#'
setEnv <- function(api_key, overwrite = TRUE, install_message = TRUE) {
  # 1. 개선된 API 키 유효성 검사
  if (is.null(api_key) || !is.character(api_key) || nchar(api_key) == 0) {
    cli_alert_danger("API key must be a non-empty string.")
    return(invisible(NULL))
  }
  
  # 일반적인 API 키 길이 확인 (Google API 키는 보통 39자)
  if (nchar(api_key) < 10) {
    cli_alert_warning("API key seems too short. Please verify your key.")
  }

  # 3. 플랫폼 호환성 개선
  home <- normalizePath("~", winslash = "/", mustWork = FALSE)
  renviron_path <- file.path(home, ".Renviron")

  # 4. 오류 처리 개선
  # .Renviron 파일 생성 시 tryCatch 추가
  if (!file.exists(renviron_path)) {
    result <- tryCatch({
      file.create(renviron_path)
    }, error = function(e) {
      cli_alert_danger(paste("Failed to create .Renviron file:", e$message))
      return(FALSE)
    })
    
    if (result == FALSE) {
      return(invisible(NULL))
    }
    
    cli_alert_info("Created .Renviron file at {.path {renviron_path}}")
  }

  # .Renviron 파일 읽기 시 tryCatch 추가
  existing_content <- tryCatch({
    readLines(renviron_path, warn = FALSE)
  }, error = function(e) {
    cli_alert_danger(paste("Failed to read .Renviron file:", e$message))
    return(NULL)
  })
  
  if (is.null(existing_content)) {
    return(invisible(NULL))
  }

  # 기존 코드 그대로 유지
  gemini_line_index <- grep("^GEMINI_API_KEY=", existing_content)

  if (length(gemini_line_index) > 0) {
    if (!overwrite) {
      cli_alert_warning("GEMINI_API_KEY already exists in .Renviron. Use overwrite = TRUE to replace it.")
      return(invisible(NULL))
    } else {
      # Replace existing entry
      existing_content[gemini_line_index] <- paste0("GEMINI_API_KEY=", api_key)
      cli_alert_info("Replacing existing GEMINI_API_KEY in .Renviron")
    }
  } else {
    # Append new entry
    existing_content <- c(existing_content, paste0("GEMINI_API_KEY=", api_key))
  }

  # 파일 쓰기 시 tryCatch 추가
  result <- tryCatch({
    writeLines(existing_content, renviron_path)
    TRUE
  }, error = function(e) {
    cli_alert_danger(paste("Failed to write to .Renviron file:", e$message))
    return(FALSE)
  })
  
  if (result == FALSE) {
    return(invisible(NULL))
  }

  # 2. API 키 마스킹 개선
  last_chars <- 4
  if (nchar(api_key) > last_chars) {
    last <- substr(api_key, nchar(api_key) - (last_chars - 1), nchar(api_key))
  } else {
    # API 키가 너무 짧은 경우, 마지막 문자만
    last <- substr(api_key, nchar(api_key), nchar(api_key))
  }

  cli_div(theme = list(span.str = list("background-color" = "blue")))
  cli_alert_success("API key {.str ...{last}} has been saved to {.path {renviron_path}}")
  cli_end()

  # Remind to restart R session
  cli_alert_info("Please restart your R session for the changes to take effect.")
  cli_alert_info("After restarting, the API key will be automatically available in all R sessions.")

  # Show usage example if requested
  if (install_message) {
    cli_alert("You may restart R and try {.run gemini_chat('What is CRAN?')}")
  }

  return(invisible(NULL))
}
