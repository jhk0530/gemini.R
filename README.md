# gemini.R

<!-- badges: start -->

[![CRAN status badge](https://www.r-pkg.org/badges/version/gemini.R)](https://CRAN.R-project.org/package=gemini.R)
[![CRAN downloads badge](https://cranlogs.r-pkg.org/badges/gemini.R)](https://cran.r-project.org/package=gemini.R)
![r-hub](https://github.com/jhk0530/gemini.R/actions/workflows/rhub.yaml/badge.svg)
[![gemini.R status badge](https://jhk0530.r-universe.dev/badges/gemini.R)](https://jhk0530.r-universe.dev/gemini.R)

<!-- badges: end -->

R package to use Google's gemini via API on R

## Installation

```r
install.packages("gemini.R") # CRAN
# install.packages("pak")
pak::pak("jhk0530/gemini.R") # GitHub
```

## Quick Start

```r
library(gemini.R)
setAPI("YOUR_API_KEY")
gemini("Explain about the gemini in astrology in one line")

# text
# "Gemini, the third astrological sign, is associated with
# communication, adaptability, and a thirst for knowledge."
```

## Terms

Before using the API, please review the following terms:

- [Google Cloud Platform Terms of Service](https://cloud.google.com/terms)
- [Gemini API Additional Terms of Service](https://ai.google.dev/gemini-api/terms)

Additional terms and conditions may apply.  
All applications using this API must comply with Google's Terms of Service.

<!-- After build_site, update favicon files of /docs as favicon of root with pkgdown::build_favicons(overwrite = TRUE)-->
