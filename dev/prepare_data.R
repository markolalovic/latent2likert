# Prepare survey data
library(psych)

# Take only the first 5 items and gender
vars <- c("A1", "A2", "A3", "A4", "A5")
part_bfi <- bfi[, c(vars, "gender")]

# Zero-one coding male = 0, female = 1
part_bfi$gender <- as.integer(part_bfi$gender - 1)

# Impute the missing values with modes
for (var in vars) {
  mode_value <- names(sort(-table(part_bfi[, var])))[1]
  part_bfi[, var][is.na(part_bfi[, var])] <- as.integer(mode_value)

  stopifnot(
    class(part_bfi[, var]) == "integer",
    sum(is.na(part_bfi[, var])) == 0
  )
}
str(part_bfi)

# Save it to `./data` directory
#save(part_bfi, file="./data/part_bfi.rda")

# Save and add `LazyData: true` in DESCRIPTION
usethis::use_data(part_bfi)
