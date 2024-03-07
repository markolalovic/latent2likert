
# responsesR <img src='man/figures/logo.png' align="right" height="160" />

<!-- badges: start -->

[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

The purpose of this package is to provide an easy way to simulate and
analyse Likert-type responses.

- It allows users to generate symmetrically or asymmetrically
  distributed responses to Likert-scale items using optimal
  discretization of a normal or skew-normal distribution.

- The relationship between a continuous latent distribution and ordinal
  Likert responses is maintained during this process. Staying within the
  framework of the classical theory’s assumptions regarding the
  treatment of Likert-type responses.

- It implements the approach to convert the ordinal Likert responses to
  possible latent values assuming that the latent distribution is normal
  or skew-normal distribution.

- Given actual responses to the Likert-scale survey questionnaire, this
  can be utilized by the researcher not to estimate the hypothetical
  parameters of a particular population, but to compare how they differ
  among different populations.

- The use of classical statistical methods is enabled without arbitrary
  mapping responses to numbers.

## Installation

You can install the latest version using `devtools`:

``` r
install.packages("devtools")
library(devtools)
```

Then install `responsesR` using:

``` r
install_github("markolalovic/responsesR")
library(responsesR)
```

# Quick Start

To generate a sample of 100 observations for 10 Likert scale items, run

``` r
df <- genLikert(size = 100, items = 10)
```

The result is a data frame of simulated responses where rows correspond
to observations and columns correspond to Likert scale items:

``` r
head(df)
#>   X1 X2 X3 X4 X5 X6 X7 X8 X9 X10
#> 1  1  3  1  4  4  1  1  3  4   4
#> 2  3  3  3  2  5  4  4  4  3   3
#> 3  5  3  5  3  5  4  5  4  4   3
#> 4  3  1  3  5  3  4  3  2  5   2
#> 5  2  3  1  4  2  3  2  2  4   3
#> 6  2  3  2  4  4  3  3  3  4   5
```

``` r
par(mfrow=c(2, 5))
for(i in 1:10) {
  barplot(table(df[, i]))
}
```

<img src="./man/figures/README-unnamed-chunk-6-1.png" width="100%" />

## Correlation

By default, a correlation matrix is generated randomly, which means that
the correlations between pairs of responses to individual items are
random:

``` r
par(mfrow=c(1, 1))
corrplot(corr=cor(df))
```

<img src="./man/figures/README-unnamed-chunk-7-1.png" width="50%" />

In order to set a specific correlation between pairs of items, for
example 0.5, use:

``` r
df <- genLikert(size = 100, items = 10, correlation = 0.5)
corrplot(corr=cor(df))
```

<img src="./man/figures/README-unnamed-chunk-8-1.png" width="50%" />

You can also provide a correlation matrix. For example, a 3 by 3
correlation matrix:

``` r
R <- c(1.00, -0.63, -0.39, -0.63, 1.00, 0.41, -0.39, 0.41, 1.00)
R <- matrix(R, nrow=3)
R
#>       [,1]  [,2]  [,3]
#> [1,]  1.00 -0.63 -0.39
#> [2,] -0.63  1.00  0.41
#> [3,] -0.39  0.41  1.00
```

And use it to generate responses to 3 items with latent correlation
matrix R:

``` r
set.seed(12345)
df <- genLikert(size = 100, items = 3, correlation = R)
corrplot(corr=cor(df), method = "number")
```

<img src="./man/figures/README-unnamed-chunk-10-1.png" width="50%" />

## Levels

By default, the `genLikert` function uses a 5-point Likert scale. To set
the number of possible responses use the “levels” parameter. For
example, for a 10-point Likert scale use:

``` r
df <- genLikert(size = 1000, items = 1, levels = 10)
barplot(table(df))
```

<img src="./man/figures/README-unnamed-chunk-11-1.png" width="65%" />

You can use a levels vector to generate responses for different point
scales. For example, to generate responses to 3 items with point scales
2, 4 and 10:

``` r
df <- genLikert(size = 1000, items = 3, levels = c(2, 4, 10))
par(mfrow=c(1, 3))
for(i in 1:3) {
  barplot(table(df[, i]))
}
```

<img src="./man/figures/README-unnamed-chunk-12-1.png" width="65%" />

## Location, scale and shape parameters

By default, the function `genLikert` uses a standard normal latent
distribution and generates symmetrically distributed responses.

Introducing asymmetries and changing the properties of hypothetical
survey respondents can be achieved by setting parameters `location`,
`scale`, and `shape` of the corresponding latent distribution.

### Location

You can use a location vector to generate responses from latent
distributions with different means. For example, to set the means of
latent distributions to -1, 0, and 1, use:

``` r
df <- genLikert(size = 1000, items = 3, location = c(-1, 0, 1))
```

The generated responses and corresponding latent distributions drawn
below:

``` r
par(mfrow=c(2, 3))
for(i in 1:3) {
  barplot(table(df[, i]))
}
x <- seq(-5, 5, length = 1000)
y <- lapply(c(-1, 0, 1), function(mu) dnorm(x, mean = mu))
for(i in 1:3) {
  plot(x, y[[i]], type="l", lwd = 2, xlab = "", ylab = "")
}
```

<img src="./man/figures/README-unnamed-chunk-14-1.png" width="100%" />

### Scale

The scale vector is used to set the variances of latent distributions.
Using `scale= c(0.5, 1, 1.5)` generates the following responses with
corresponding latent distributions drawn below:

``` r
df <- genLikert(size = 1000, items = 3, scale = c(0.5, 1, 1.5))

par(mfrow=c(2, 3))
for(i in 1:3) {
  barplot(table(df[, i]))
}
y <- lapply(c(0.5, 1, 1.5), function(s) dnorm(x, sd = s))
for(i in 1:3) {
  plot(x, y[[i]], type="l", lwd = 2, xlab = "", ylab = "")
}
```

<img src="./man/figures/README-unnamed-chunk-15-1.png" width="100%" />

### Shape

Skewness is introduced to the latent distribution through the shape
parameter. The skewness increases as the absolute value of the shape
parameter increases. The latent distribution is right-skewed if
`shape > 0` and left-skewed if `shape < 0`. The value of the shape
parameter should always be between -9 and 9. The skew normal
distribution is used as the latent distribution by the `genLikert`
function if `shape != 0`. This can be observed by increasing the sample
size and the number of levels:

``` r
df <- genLikert(size = 10^6, levels = 100, shape = -5)
par(mfrow=c(1, 1))
barplot(table(df)/10^6)
```

<img src="./man/figures/README-unnamed-chunk-16-1.png" width="65%" />

You can use a shape vector to generate responses from latent
distributions with different skewness. For example,
`shape = c(-2.5, 0, 2.5)` sets the skewness of latent distributions to
-2.5, 0 and 2.5. The generated responses with corresponding latent
distributions are drawn below:

``` r
df <- genLikert(size = 1000, items = 3, shape = c(-2.5, 0, 2.5))

par(mfrow=c(2, 3))
for(i in 1:3) {
  barplot(table(df[, i]))
}
y <- lapply(c(-2.5, 0, 2.5), function(a) dSN(x, alpha = a))
for(i in 1:3) {
  plot(x, y[[i]], type="l", lwd = 2, xlab = "", ylab = "")
}
```

<img src="./man/figures/README-unnamed-chunk-17-1.png" width="100%" />

## Estimating parameters of the latent distribution

Given responses to the Likert-scale survey questions, it is possible to
estimates the parameters using the function `estimateParameters`. The
function assumes that the underlying latent distribution is normal.

As an example, let’s generate 1000 responses where the underlying
distribution is assumed to be normal with `mean = -0.5` and
`variance = 0.5`:

``` r
set.seed(12345)
df <- genLikert(size = 1000, location = -0.5, scale = 0.5)
par(mfrow=c(1, 1))
barplot(table(df))
```

<img src="./man/figures/README-unnamed-chunk-18-1.png" width="65%" />

Besides the responses, the function `estimateParameters` requires
specifying the number of possible levels in order to return the
estimates:

``` r
estimateParameters(df, levels = 5)
#>       mean   variance 
#> -0.4685787  0.5019484
```

It is also possible to provide responses to multiple items. For example,
if we generate responses to three items with different values of latent
parameters:

``` r
df <- genLikert(size = 1000, 
                items = 3, 
                levels = c(5, 6, 7), 
                location = c(1,   2,  3), 
                scale    = c(0.5, 1,  1.5))
par(mfrow=c(1, 3))
for(i in 1:3) {
  barplot(table(df[, i]))
}
```

<img src="./man/figures/README-unnamed-chunk-20-1.png" width="65%" />

The function `estimateParameters` returns a table with the estimates for
each item:

``` r
estimateParameters(df, levels = c(5, 6, 7))
#>           items
#> estimates         X1        X2        X3
#>   mean     0.9933573 2.0090236 3.0461336
#>   variance 0.4741466 0.9887716 1.4972725
```

To ensure that the estimates are good, it’s important to provide a large
number of responses. The estimated values vary and can be far from the
actual values if the number of responses is small. For example if we
generate only 100 responses from the same underlying latent
distribution:

``` r
df <- genLikert(size = 100, 
                items = 2, 
                levels = c(5, 5), 
                location = c(-0.5, -0.5), 
                scale    = c(0.5, 0.5))
par(mfrow=c(1, 2))
for(i in 1:2) {
  barplot(table(df[, i]))
}
```

<img src="./man/figures/README-unnamed-chunk-22-1.png" width="65%" />

Even though the latent distribution is the same, the estimated values
are different and are not in line with the actual latent parameters:

``` r
estimateParameters(df, levels = c(5, 6, 7))
#>           items
#> estimates          X1         X2
#>   mean     -0.4157670 -0.7906553
#>   variance  0.4905879  0.4647513
```
