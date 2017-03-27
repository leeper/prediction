# CHANGES TO prediction 0.1.12

* Added `prediction.princomp()` method for "princomp" objects from **stats**. (#1)
* Added `prediction.naiveBayes()` method for "naiveBayes" objects from **e1071**. (#1)
* Added `prediction.rlm()` method for "rlm" objects from **MASS**. (#1)
* Added `prediction.qda()` method for "qda" objects from **MASS**. (#1)
* Added `prediction.lda()` method for "lda" objects from **MASS**. (#1)
* `find_data()` now respects the `subset` argument in an original model call. (#15)
* `find_data()` now respects the `na.action` argument in an original model call. (#15)
* `find_data()` now gracefully fails when a model is specified without a formula. (#16)
* `prediction()` methods no longer add a "fit" or "se.fit" class to any columns. Fitted values are identifiable by the column name only.

# CHANGES TO prediction 0.1.11

* `build_datalist()` now returns `at` value combinations as a list.

# CHANGES TO prediction 0.1.10

* Added `prediction.nnet()` method for "nnet" and "multinom" objects from **nnet**. (#1)

# CHANGES TO prediction 0.1.9

* `prediction()` methods now return the value of `data` as part of the response data frame. (#8, h/t Ben Whalley)
* Slight change to `find_data()` methods for `"crch"` and `"hxlr"`. (#5)
* Added `prediction.glmx()` and `prediction.hetglm()` methods for "glmx" and "hetglm" objects from **glmx**. (#1)
* Added `prediction.betareg()` method for "betareg" objects from **betareg**. (#1)
* Added `prediction.rq()` method for "rq" objects from **quantreg**. (#1)
* Added `prediction.gam()` method for "gam" objects from **gam**. (#1)
* Expanded basic test suite.

# CHANGES TO prediction 0.1.8

* Added `prediction()` and `find_data()` methods for `"crch"` `"hxlr"` objects from **crch**. (#4, h/t Carl Ganz)

# CHANGES TO prediction 0.1.7

* Added `prediction()` and `find_data()` methods for `"merMod"` objects from **lme4**. (#1)

# CHANGES TO prediction 0.1.6

* Moved the `seq_range()` function from **margins** to **prediction**.
* Moved the `build_datalist()` function from **margins** to **prediction**. This will simplify the ability to calculate arbitrary predictions.

# CHANGES TO prediction 0.1.5

* Added `prediction.svm()` method for objects of class `"svm"` from **e1071**. (#1)
* Fixed a bug in `prediction.polr()` when attempting to pass a `type` argument, which is always ignored. A warning is now issued when attempting to override this.

# CHANGES TO prediction 0.1.4

* Added `mean_or_mode()` and `median_or_mode()` functions, which provide a simple way to aggregate a variable of factor or numeric type. (#3)
* Added `prediction()` methods for various time-series model classes: "ar", "arima0", and "Arima".

# CHANGES TO prediction 0.1.3

* `find_data()` is now a generic, methods for "lm", "glm", and "svyglm" classes. (#2, h/t Carl Ganz)

# CHANGES TO prediction 0.1.2

* Added support for "svyglm" class from the **survey** package. (#1)
* Added tentative support for "clm" class from the **ordinal** package. (#1)

# CHANGES TO prediction 0.1.0

* Initial package released.
