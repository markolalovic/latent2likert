## Version 0.1.0
- Release date: 2020-10-17
- Initial release (development version).
- Tested on platforms: x86_64-pc-linux-gnu (64-bit) and x86_64-w64-mingw32 (64-bit).

## Version 1.0.0
- Release date: 2024-03-28
- Added the option to generate correlated Likert scale items in the `rlikert()` function.
- Added the `estimate_params()` function for estimating parameters from existing survey data.
- Resolved dependency issues.
- Reduced dependency to only standard R packages: mvtnorm, stats, and graphics.
- The sn package is now required only for generating correlated responses using a skew normal distribution.
- Added user prompts to install the sn package when needed.

## Version 1.1.0
- Release date: 2024-06-06
- Minor updates to functions and vignettes.

## Version 1.1.1
- Release date: 2024-06-06
- Renamed the package from `responsesR` to `latent2likert` to better reflect its purpose of converting latent variables into Likert scale responses.
- Improved the package website and documentation.
- **Note:** The codebase is under development, and finer details may change.

## Version 1.2.1
- Release date: 2024-06-12
- Refactored code for enhanced modularity and maintainability.
- Modularized core functions for improved readability.
- Improved the structure and organization of the codebase.
- Enhanced error handling for various correlation input scenarios.

## Version 1.2.2
- Release date: 2024-07-26
- Exported previously internal function `estimate_mean_and_sd` based on user feedback, now available for direct use.
- Moved the package `sn` to Suggests.
- Fixed vignette building errors.
