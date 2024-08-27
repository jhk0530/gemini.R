# gemini.R <img src = 'https://github.com/user-attachments/assets/331ba265-9852-4ca5-9c9a-ff19b9e494b3' width = 120 align = 'right'></img>

<!-- badges: start -->
[![CRAN status badge](https://www.r-pkg.org/badges/version/gemini.R)](https://cran.r-project.org/web/packages/gemini.R/index.html)
![r-hub](https://github.com/jhk0530/gemini.R/actions/workflows/rhub.yaml/badge.svg)
[![gemini.R status badge](https://jhk0530.r-universe.dev/badges/gemini.R)](https://jhk0530.r-universe.dev/gemini.R)
<!-- badges: end -->

![Alt](https://repobeats.axiom.co/api/embed/bc0595c11e22d5380cf10a646bc6049db9e5fc44.svg "Repobeats analytics image")

R package to use Google's gemini via API on R

## Installation

You can install the development version of gemini.R from GitHub with:

``` r
# install.packages("pak")
pak::pak("jhk0530/gemini.R")
```

Or install from [R-universe](https://r-universe.dev/search/)
```r
install.packages("gemini.R",
  repos = c("https://jhk0530.r-universe.dev", "https://cloud.r-project.org")
)
```

## Example

``` r
library(gemini.R)

setAPI("your API key") # check https://makersuite.google.com/app/apikey
gemini("Explain about the gemini in astrology in one line")

# text 
# "Gemini, the third astrological sign, is associated with 
# communication, adaptability, and a thirst for knowledge." 

```

## gemini with image

``` r

# uses default prompt as "Explain this image"
# uses included image of gemini.R package
gemini_image(image = system.file("docs/reference/figures/image.png", package = "gemini.R"))

# text 
# " The image shows a table with a white tablecloth. On the table are two cups of coffee, 
# a bowl of blueberries, and five scones. The scones are covered in blueberries and have 
# a crumbly texture. There are also some pink flowers on the table. The background is a 
# dark blue color. The image is taken from a top-down perspective." 

```

## gemini with image via Shiny

To use `gemini_image` function, image file required. 
and I recommend to use shiny app with `fileInput` function.

``` r
library(shiny)
library(gemini.R)

setAPI("YOUR KEY")
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
  <img src='./man/figures/shiny.png' width = '70%'>
</p>

> [!note] 
>
> image is from [Google AI for Developers](https://ai.google.dev/tutorials/rest_quickstart)
>
> I've got many inspiration from [Deepanshu Bhalla](https://www.linkedin.com/in/deepanshubhalla/)'s [article](https://www.listendata.com/2023/12/google-gemini-r.html)
