# prediction

**prediction** is a one-function package that provides type-safe methods for generating predictions from fitted regression models. It provides an S3 generic, `prediction()`, and associated methods, which always return a `"data.frame"` class object rather than the mix of vectors, lists, etc. that are returned by the `predict()` method for many model types. It provides a key piece of infrastructure for the **margins** package.

## Simple code examples



A major downside of the `predict()` methods for common modelling classes is that the result is not typesafe. Consider the following simple example:


```r
library("stats")
library("datasets")
x <- lm(mpg ~ cyl * hp + wt, data = mtcars)
class(predict(x))
```

```
## [1] "numeric"
```

```r
class(predict(x, se.fit = TRUE))
```

```
## [1] "list"
```

**prediction** solves this issue by providing a wrapper around `predict()`, called `prediction()`, that always returns a tidy data frame:


```r
library("prediction")
p <- prediction(x)
class(p)
```

```
## [1] "prediction" "data.frame"
```

```r
head(p)
```

```
##     fitted se.fitted
## 1 21.90488 0.6927034
## 2 21.10933 0.6266557
## 3 25.64753 0.6652076
## 4 20.04859 0.6041400
## 5 17.25445 0.7436172
## 6 19.53360 0.6436862
```

The output always contains the original data (i.e., either data found using the `find_data()` function or passed to the `data` argument to `prediction()`). This makes it much simpler to pass predictions to, e.g., further summary or plotting functions.

## Supported model classes

The currently supported model classes are:

 - "lm" from `stats::lm()`
 - "glm" from `stats::glm()`, `MASS::glm.nb()`, `glmx::glmx()`, `glmx::hetglm()`
 - "ar" from `stats::ar()`
 - "Arima" from `stats::arima()`
 - "arima0" from `stats::arima0()`
 - "betareg" from `betareg::betareg()`
 - "clm" from `ordinal::clm()`
 - "coxph" from `survival::coxph()`
 - "crch" from `crch::crch()`
 - "gam" from `gam::gam()`
 - "gls" from `nlme::gls()`
 - "hxlr" from `crch::hxlr()`
 - "ivreg" from `AER::ivreg()`
 - "loess" from `stats::loess()`
 - "nls" from `stats::nls()`
 - "nnet" from `nnet::nnet()`, `nnet::multinom()`
 - "polr" from `MASS::polr()`
 - "rq" from `quantreg::rq()`
 - "selection" from `sampleSelection::selection()`
 - "survreg" from `survival::survreg()`
 - "svm" from `e1071::svm()`
 - "svyglm" from `survey::svyglm()`

## Requirements and Installation

[![CRAN](http://www.r-pkg.org/badges/version/prediction)](https://cran.r-project.org/package=prediction)
[![Build Status](https://travis-ci.org/leeper/prediction.svg?branch=master)](https://travis-ci.org/leeper/prediction)
[![Build status](https://ci.appveyor.com/api/projects/status/a4tebeoa98cq07gy/branch/master?svg=true)](https://ci.appveyor.com/project/leeper/prediction/branch/master)
[![codecov.io](http://codecov.io/github/leeper/prediction/coverage.svg?branch=master)](http://codecov.io/github/leeper/prediction?branch=master)
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)

The development version of this package can be installed directly from GitHub using `ghit`:

```R
if (!require("ghit")) {
    install.packages("ghit")
    library("ghit")
}
install_github("leeper/prediction")
```
