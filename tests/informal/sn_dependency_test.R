remove.packages("sn")
rm(list = ls())
.rs.restartR()

check_sn_dependency <- function() {
  if (!requireNamespace("sn", quietly = TRUE)) {
    stop(
      "Package \"sn\" must be installed to get correlated skew responses.
        Please run:\n\n \tinstall.packages(\"sn\")\n\n",
      call. = FALSE)
  }
  print("Package `sn` is installed.")
}

check_sn_dependency()
# Error: Package "sn" must be installed to get correlated skew responses.
# Please run:
#
#   install.packages("sn")

install.packages("sn")
check_sn_dependency()
# [1] "Package `sn` is installed."
