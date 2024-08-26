# gemini.R (development version)

# gemini.R 0.5.1

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
