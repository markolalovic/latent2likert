# should get identity, 1-1 map
case_1 <- function(R) { sum(!is.character(R) & !is.matrix(R) & (R == 0)) }
case_2 <- function(R) { sum(R == "random") }
case_3 <- function(R) { sum(!is.character(R) & !is.matrix(R) & (R != 0)) }
case_4 <- function(R) { sum(is.matrix(R)) }

R <- get_rand_corr_matrix(3)
inputs <- list("1"=0, "2"="random", "3"=0.5, "4"=R)
rbind(sapply(inputs, case_1), sapply(inputs, case_2), 
      sapply(inputs, case_3), sapply(inputs, case_4))