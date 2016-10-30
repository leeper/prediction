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

```r
str(p)
```

```
## Classes 'prediction' and 'data.frame':	32 obs. of  2 variables:
##  $ fitted   :Classes 'fit', 'numeric'  Named num [1:32] 21.9 21.1 25.6 20 17.3 ...
##   .. ..- attr(*, "names")= chr [1:32] "Mazda RX4" "Mazda RX4 Wag" "Datsun 710" "Hornet 4 Drive" ...
##  $ se.fitted:Classes 'se.fit', 'numeric'  Named num [1:32] 0.693 0.627 0.665 0.604 0.744 ...
##   .. ..- attr(*, "names")= chr [1:32] "Mazda RX4" "Mazda RX4 Wag" "Datsun 710" "Hornet 4 Drive" ...
##  - attr(*, "type")= chr "response"
```


## Requirements and Installation

[![CRAN](http://www.r-pkg.org/badges/version/prediction)](https://cran.r-project.org/package=prediction)
[![Build Status](https://travis-ci.org/leeper/prediction.svg?branch=master)](https://travis-ci.org/leeper/prediction)
[![Build status](https://ci.appveyor.com/api/projects/status/t6nxndmvvcw3gw7f/branch/master?svg=true)](https://ci.appveyor.com/project/leeper/prediction/branch/master)
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

