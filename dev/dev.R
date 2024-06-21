# Development
devtools::load_all("/Users/markolalovic/dev/latent2likert/")

covr::codecov(
  quiet = FALSE,
  clean = FALSE,
  token = Sys.getenv("CODECOV_TOKEN")
)
