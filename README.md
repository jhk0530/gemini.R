
# gemini.R

<!-- badges: start -->
<!-- badges: end -->

R package to use Google's gemini via API on R

## Installation

You can install the development version of gemini.R from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jhk0530/gemini.R")
```

## Example

``` r
library(gemini.R)

setAPI("your API key")
gemini("Explain about the gemini in astrology in one line")

# text 
# "Gemini, the third astrological sign, is associated with 
# communication, adaptability, and a thirst for knowledge." 

```

## gemini with image via Shiny

To use `gemini_image` function, image file required. 
and I recommend to use shiny app with `fileInput` function.

``` r
library(shiny)
library(gemini.R)

ui <- fluidPage(
  sidebarLayout(
    NULL,
    mainPanel(
      fileInput(
        inputId = "file",
        label = "Choose file to upload",
      ),
      imageOutput(outputId = "image1"),
      textInput(
        inputId = "prompt", 
        label = "Prompt", 
        placeholder = "Enter Prompts Here"
      ),
      actionButton("goButton", "Ask to gemini"),
      textOutput("text1")
    )
  )
)

server <- function(input, output) {
  
  observeEvent(input$file, {
    path <- input$file$datapath
    output$image1 <- renderImage({
      list(
        src = path
      )
    }, deleteFile = FALSE)
  })
  
  observeEvent(input$goButton, {
    output$text1 <- renderText({
      gemini_image(input$prompt, input$file$datapath)
    })
  })
}

shinyApp(ui = ui, server = server)

```
<p style = 'text-align:center;'>
  <img src='shiny.png' width = '70%'>
</p>

> Note, 
>
> image is from [Google AI for Developers](https://ai.google.dev/tutorials/rest_quickstart)
