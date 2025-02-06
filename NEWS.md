# gemini.R

# gemini.R 0.8.0

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
