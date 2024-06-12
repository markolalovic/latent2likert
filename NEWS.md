# latent2likert 0.1.0 (Release date: 2020-10-17)

- Initial release.
- Tested on platforms: x86_64-pc-linux-gnu (64-bit) and x86_64-w64-mingw32 (64-bit).

# latent2likert 1.0.0 (Release date: 2024-03-28)

- The option to generate correlated Likert scale items was added to the function `rLikert()`.
- New function `estimate_parameters()` was added that allows for the estimation of parameters from existing survey data to replicate it more accurately.
- Ensured compatibility with 
- Issues related to the dependencies have been resolved. 
- This version now only imports the standard R packages mvtnorm, stats, and graphics packages stats and graphics, which are typically included in R releases.
- The package sn is necessary only when generating correlated responses based on skew normal distribution.
- Added user prompts for installing the sn package when necessary.

# latent2likert 1.1.0 (Release date: 2024-06-06)

- A minor update of functions and vignettes.

# latent2likert 1.1.1 (Release date: 2024-06-06)

- The package was renamed from responsesR to latent2likert.
- To capture the essence of converting latent variables into Likert scale responses.
- Improvements of package website and documentation.

# latent2likert 1.2.1 (Release date: 2024-06-12)

- Refactored code for improved modularity and maintainability.
- Modularized core functions for better readability.
- Improved the structure and organization of the codebase.
- Improved error handling of different cases for correlations input.
- Updated the estimate_parameters function for estimating latent parameters from survey data.
- The codebase is currently in development, finer details may change.

