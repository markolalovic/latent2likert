context("Testing helper functions")

test_that("prop_table gives the correct result, univariate case", {
  data <- rep(c(1,2,3,4), each=2)
  tab <- get_prop_table(data, K=4)
  correct_tab <- rep(0.25, 4)
  names(correct_tab) <- 1:4
  expect_that( identical(tab, correct_tab), equals(TRUE) )
})

test_that("prop_table gives the correct result, multivariate case", {
  y <- rep(c(1,2,3,4), each=2)
  data <- cbind(y, y)
  tab <- get_prop_table(data, K=4)

  correct_tab <- rbind(rep(0.25, 4), rep(0.25, 4))
  dimnames(correct_tab) <- dimnames(tab)

  expect_that( identical(tab, correct_tab), equals(TRUE) )
})

test_that("pad_levels gives the correct result", {
  pk <- rep(0.25, 4)
  names(pk) <- 1:4
  padded_pk <- pad_levels(pk = pk, K = 5)

  correct_pk <- c(rep(0.25, 4), 0)
  names(correct_pk) <- 1:5
  expect_that( identical(padded_pk, correct_pk), equals(TRUE) )
})

test_that("percentify gives the correct result", {
  xbreaks <- seq(from = 0, to = 1, length.out = 6)
  xlabs <- sapply(xbreaks, percentify)
  correct_xlabs <- c("0%", "20%", "40%", "60%", "80%", "100%")
  expect_that( identical(xlabs, correct_xlabs), equals(TRUE) )
})
