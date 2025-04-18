# gemini.R

# gemini.R 0.13.0

## 0.13.0

- Relaxed the `model` parameter validation to support newly released models like `gemini-2.5-flash-preview-04-17`
- Enhanced error handling across all functions for improved stability
- Standardized response processing across all API functions
- Updated documentation to reflect expanded model compatibility

## 0.12.0

- Added Gemini searching `gemini_search` for ground using **gemini-2.0 models**
- Added Gemini searching `gemini_searchR` for retrive using **gemini-1.5 models**

## 0.11.0

- Added Gemini 2.5-pro-exp model (`2.5-pro-exp-03-25`)
- Deprecated Gemini 2.0-pro-exp model (`2.0-pro-exp-02-05`)
- Now image generation possible with `gen_image()` using `2.0-flash-exp-image-generation` model.

## 0.10.0

- `countTokens` function for countTokens API
- `setEnv` function added.
- Deprecated Gemini 1.5 models (`1.5-pro`, `1.5-flash`)
- Add CRANlogs badge in readme

## 0.9.2

- Modified `.Rbuildignore` to ignore README.md

## 0.9.1

- Add roxygen documents for `region`

## 0.9.0

- Updated parameters across all functions (`temperature`, `maxOutputTokens`, `topK`, `topP`, `seed`)
- Adjusted default values: temperature to **1** and maxOutputTokens to **8192**, model as **2.0-flash**

## 0.8.1

- requires `region` from user in Vertex AI API

## 0.8.0

- Add Vertex AI with new functions: `token.vertex()`, `gemini.vertex()`, `gemini_image.vertex()`, `gemini_audio.vertex()`
- These functions are in experimental stage and may change in future versions.

## 0.7.0

- Generate unit test using `gen_test()` added.
- `gemini_audio` supports more than `mp3`.
- gemini.R supports `2.0-flash-exp` as model.

## 0.6.1

- Generate roxygen with `gen_docs()` added.
- Some terms are improved.

## 0.5.0

- gemini.R supports `1.5=flash` model.
- gemini functions require `temperature`, `maxTokenOutput`

## 0.4.0

- Updated to use [httr2](https://httr2.r-lib.org/)
- `setAPI` now shows last 4 words of API key for confirmation.
- `cat` updated using [cli](https://github.com/r-lib/cli) for better readability in console.
- `gemini_image()` takes default prompt as "Explain this image" and example image of flower. See `?gemini_image`

## 0.3.0

- R CMD CHECK no longer shows any NOTEs or WARNINGs.
- use R-hub to check multiple platforms with github action

## 0.2.0

### What's changed

- Added Multi-turn conversation or **Chat** 💬. See `gemini_chat`
- Slight change with function documentation.
- Now generates message to inform to `setAPI` when API key is not exist yet.
