# testing clean install
remove.packages("mvtnorm")
remove.packages("responsesR")
rm(list = ls())
.rs.restartR()

devtools::install_github("markolalovic/responsesR")
library("responsesR")
