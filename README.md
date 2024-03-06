
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
  can be utilized by the researcher to estimate not only the values of a
  particular population, but also how they differ among different
  populations.

- The use of classical statistical methods is enabled without arbitrary
  mapping of responses to numbers.

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
```

    ##   X1 X2 X3 X4 X5 X6 X7 X8 X9 X10
    ## 1  3  3  4  5  2  3  2  4  3   3
    ## 2  4  4  5  2  3  3  2  2  4   5
    ## 3  5  3  4  2  3  1  1  3  3   4
    ## 4  3  2  3  3  1  3  2  5  4   4
    ## 5  3  4  2  3  3  3  4  2  3   4
    ## 6  4  4  2  2  4  3  3  4  3   3

``` r
par(mfrow=c(2, 5))
for(i in 1:10) {
  barplot(table(df[, i]))
}
```

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

## Correlation

By default, a correlation matrix is generated randomly, which means that
the correlations between pairs of responses to individual items are
random:

``` r
par(mfrow=c(1, 1))
corrplot(corr=cor(df))
```

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

In order to set a specific correlation between pairs of items, for
example 0.5, use:

``` r
df <- genLikert(size = 100, items = 10, correlation = 0.5)
corrplot(corr=cor(df))
```

![](README_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

You can also provide a correlation matrix. For example, a 3 by 3
correlation matrix:

``` r
R <- c(1.00, -0.63, -0.39, -0.63, 1.00, 0.41, -0.39, 0.41, 1.00)
R <- matrix(R, nrow=3)
R
```

    ##       [,1]  [,2]  [,3]
    ## [1,]  1.00 -0.63 -0.39
    ## [2,] -0.63  1.00  0.41
    ## [3,] -0.39  0.41  1.00

And use it for 3 Likert scale items:

``` r
set.seed(12345)
df <- genLikert(size = 100, items = 3, correlation = R)
corrplot(corr=cor(df), method = "number")
```

![](README_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

## Levels

By default, the `genLikert` function uses a 5-point Likert scale. To
change the number of possible responses use the “levels” parameter. For
example, for a 10-point Likert scale use:

``` r
df <- genLikert(size = 1000, items = 1, levels = 10)
barplot(table(df))
```

![](README_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

You can use a levels vector to generate responses for different point
scales. For example, to generate responses to 3 Likert scale items with
2, 4, and 10-point Likert scales, use:

``` r
df <- genLikert(size = 1000, items = 3, levels=c(2, 4, 10))
par(mfrow=c(1, 3))
for(i in 1:3) {
  barplot(table(df[, i]))
}
```

![](README_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

## Location, scale and shape parameters

By default, the function `genLikert` uses a standard normal latent
distribution and generates symmetrically distributed responses. This can
be observed by increasing the sample size and number of levels:

``` r
df <- genLikert(size = 10^6, levels = 100)
par(mfrow=c(1, 1))
barplot(table(df)/10^6)
```

![](README_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

Introducing asymmetries and changing the properties of hypothetical
survey respondents can be achieved by utilizing parameters `location`,
`scale`, and `shape`.

You can use a location vector to generate responses from latent
distributions with different means. As an example, let’s use the means
-1, 0, and 1:

``` r
df <- genLikert(size = 1000, items = 3, location = c(-1, 0, 1))
par(mfrow=c(1, 3))
for(i in 1:3) {
  barplot(table(df[, i]))
}
```

![](README_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

The scale vector is used to change the variances of latent
distributions:

``` r
df <- genLikert(size = 1000, items = 3, scale = c(0.5, 1, 1.5))
par(mfrow=c(1, 3))
for(i in 1:3) {
  barplot(table(df[, i]))
}
```

![](README_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

Finally, the shape parameter can be used to introduce response bias or
skewness:

``` r
df <- genLikert(size = 1000, items = 3, shape = c(-5, 0, 5))
par(mfrow=c(1, 3))
for(i in 1:3) {
  barplot(table(df[, i]))
}
```

![](README_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

When `shape != 0`, the function `genLikert` uses a skew-normal
distribution. This can be observed by increasing the sample size and
number of levels:

``` r
df <- genLikert(size = 10^6, levels = 100, shape = -5)
par(mfrow=c(1, 1))
barplot(table(df)/10^6)
```

![](README_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

## Estimating the parameters of the latent population distribution

This will be added shortly.
