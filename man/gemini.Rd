% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gemini.R
\name{gemini}
\alias{gemini}
\title{Generate text from text with Gemini}
\usage{
gemini(prompt, model = "1.5-flash", temperature = 0.5, maxOutputTokens = 1024)
}
\arguments{
\item{prompt}{The prompt to generate text from}

\item{model}{The model to use. Options are '1.5-flash', '1.5-pro', '1.0-pro'. Default is '1.5-flash'
see https://ai.google.dev/gemini-api/docs/models/gemini}

\item{temperature}{The temperature to use. Default is 0.5 value should be between 0 and 2
see https://ai.google.dev/gemini-api/docs/models/generative-models#model-parameters}

\item{maxOutputTokens}{The maximum number of tokens to generate.
Default is 1024 and 100 tokens correspond to roughly 60-80 words.}
}
\value{
Generated text
}
\description{
Generate text from text with Gemini
}
\examples{
\dontrun{
library(gemini.R)
setAPI("YOUR_API_KEY")
gemini("Explain dplyr's mutate function")
}
}
\seealso{
https://ai.google.dev/docs/gemini_api_overview#text_input
}
