## Installation
install.packages("devtools") #TODO: submit on CRAN
library(devtools)

install_github("markolalovic/responsesR")
library(responsesR)


## Simulate unbiased responses
responses <- rLikert(size = 100, items = 10)
head(responses)
par(mfrow=c(2, 5))
for(i in 1:10) {
  barplot(table(responses[, i]))
}

responses <- rLikert(size = 10^6, levels = 100)
par(mfrow=c(1, 1))
barplot(table(responses)/10^6)


## Simulate response bias
responses <- rLikert(size=100, items=10,
                     location=0.3, scale=0.8, shape=-5)
par(mfrow=c(2, 5))
for(i in 1:10) {
  barplot(table(responses[, i]))
}

responses <- rLikert(size = 10^6, levels = 100,
                     location=0.3, scale=0.8, shape=-5)
par(mfrow=c(1, 1))
barplot(table(responses)/10^6)


## Simulate correlated Likert scale items
#TODO



