# responsesR

* The purpose of this package is to provide an easy way to simulate Likert-scale data.

* It allows users to generate symmetrically or asymmetrically distributed Likert scale item responses.

* While maintaining a nice mathematical relationship between a continuous latent distribution and simulated ordinal Likert responses.

* This is done by using optimal discretization of skew-normal distribution.

&nbsp;


# How-to


## Installation
To install the package from GitHub, we first need `devtools`:

```R
install.packages("devtools")
library(devtools)
```

Then install `responsesR` using:

```R
install_github("markolalovic/responsesR")
library(responsesR)
```


## Simulate unbiased responses
To generate a sample of size = 100 with the number of Likert scale items = 10:

```R
responses <- rLikert(size = 100, items = 10)
```

The result is a data frame of simulated responses where rows correspond to observations and columns to Likert scale items:

```R
head(responses)
```

<table>
<caption>A data.frame: 6 × 10</caption>
<thead>
	<tr><th></th><th scope=col>X1</th><th scope=col>X2</th><th scope=col>X3</th><th scope=col>X4</th><th scope=col>X5</th><th scope=col>X6</th><th scope=col>X7</th><th scope=col>X8</th><th scope=col>X9</th><th scope=col>X10</th></tr>
	<tr><th></th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;int&gt;</th></tr>
</thead>
<tbody>
	<tr><th scope=row>1</th><td>2</td><td>4</td><td>2</td><td>4</td><td>1</td><td>2</td><td>4</td><td>4</td><td>3</td><td>2</td></tr>
	<tr><th scope=row>2</th><td>4</td><td>3</td><td>3</td><td>2</td><td>4</td><td>1</td><td>2</td><td>3</td><td>2</td><td>3</td></tr>
	<tr><th scope=row>3</th><td>3</td><td>2</td><td>3</td><td>2</td><td>1</td><td>5</td><td>3</td><td>4</td><td>5</td><td>2</td></tr>
	<tr><th scope=row>4</th><td>2</td><td>4</td><td>3</td><td>2</td><td>3</td><td>4</td><td>2</td><td>2</td><td>5</td><td>1</td></tr>
	<tr><th scope=row>5</th><td>5</td><td>3</td><td>1</td><td>2</td><td>4</td><td>3</td><td>3</td><td>3</td><td>1</td><td>3</td></tr>
	<tr><th scope=row>6</th><td>2</td><td>4</td><td>2</td><td>2</td><td>2</td><td>4</td><td>2</td><td>2</td><td>3</td><td>4</td></tr>
</tbody>
</table>

```R
par(mfrow=c(2, 5))
for(i in 1:10) {
    barplot(table(responses[, i]))
}
```

<img src="figures/example-1.png" alt="Example 1." width="800">

By default, the function `rLikert` generates symmetrically distributed responses from a standard normal distribution. Also by default, it is using a 5-point Likert scale. You can change the number of possible responses by setting the "levels" parameter; e.g. for a 10-point Likert scale use `levels = 10`.

The sampling distribution converges to a normal distribution. This can be observed by increasing the sample size and number of levels levels (the number of items = 1 by default):

```R
responses <- rLikert(size = 10^6, levels = 100)
par(mfrow=c(1, 1))
barplot(table(responses)/10^6)
```

<img src="figures/convergence-1.png" alt="Convergence 1." width="500">


## Simulate response bias

To simulate asymmetrically distributed responses either:

* shift the distribution by using the `location` parameter;
* change the variability or spread by using the `scale` parameter;
* or introduce asymmetry or skewness using the `shape` parameter.



<img src="figures/anim-location.gif" alt="Animation of location parameter." width="800">

<img src="figures/anim-scale.gif" alt="Animation of location parameter." width="800">

<img src="figures/anim-shape.gif" alt="Animation of shape parameter." width="800">

&nbsp;


This way simulating some properties of hypothetical survey respondents:

* strong agreement with some statement by increasing the `location` parameter;
* common or typical answers by using the `scale` parameter;
* preferences or tendencies using the `shape` parameter.

For example:
```R
responses <- rLikert(size=100, items=10,
                     location=0.3, scale=0.8, shape=-5)
```

```R
par(mfrow=c(2, 5))
for(i in 1:10) {
    barplot(table(responses[, i]))
}
```

<img src="figures/example-2.png" alt="Example 1." width="800">


The sampling distribution converges to a skew-normal distribution. We can observe this by increasing the sample size and number of levels (the number of items = 1 by default):

```R
responses <- rLikert(size = 10^6, levels = 100,
                     location=0.3, scale=0.8, shape=-5)
par(mfrow=c(1, 1))
barplot(table(responses)/10^6)
```

<img src="figures/convergence-2.png" alt="Convergence 1." width="500">


## Simulate correlated Likert scale items

This will be added shortly.

