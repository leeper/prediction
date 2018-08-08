# prediction 0.3.10

* Added tests for `find_data()` and `prediction.lm()` to check for correct behavior in the presence of missing data (`na.action`) and `subset` arguments. (#28)

# prediction 0.3.8

* Provisional support for variances of average predictions for GLMs. (#17)
* Added an example dataset, `margex`, borrowed from Stata's identically named data.

# prediction 0.3.7

* `summary(prediction(...))` now reports variances of average predictions, along with test statistics, p-values, and confidence intervals, where supported. (#17)
* Added a function `prediction_summary()` which simply calls `summary(prediction(...))`.
* All methods now return additional attributes.

# prediction 0.3.6

* Small fixes for failing CRAN checks. (#25)
* Remove `prediction.bigglm()` method (from **biglm**) due to failing tests. (#25)

# prediction 0.3.5

* Fixed a bug that required specifying `stats::poly()` rather than just `poly()` in model formulae. (#22)

# prediction 0.3.4

* Added `prediction.glmnet()` method for "glmnet" objects from **glmnet**. (#1)

# prediction 0.3.3

* `prediction.merMod()` gains an `re.form` argument to pass forward to `predict.merMod()`.

# prediction 0.3.2

* Fix typo in "speedglm" that was overwriting "glm" method.

# prediction 0.3.0

* CRAN release.

# prediction 0.2.11

* Added `prediction.glmML()` method for "glimML" objects from **aod**. (#1)
* Added `prediction.glmQL()` method for "glimQL" objects from **aod**. (#1)
* Added `prediction.truncreg()` method for "truncreg" objects from **truncreg**. (#1)
* Noted implicit support for "tobit" objects from **AER**. (#1)

# prediction 0.2.10

* Added `prediction.bruto()` method for "bruto" objects from **mda**. (#1)
* Added `prediction.fda()` method for "fda" objects from **mda**. (#1)
* Added `prediction.mars()` method for "mars" objects from **mda**. (#1)
* Added `prediction.mda()` method for "mda" objects from **mda**. (#1)
* Added `prediction.polyreg()` method for "polyreg" objects from **mda**. (#1)

# prediction 0.2.9

* Added `prediction.speedglm()` and `prediction.speedlm()` methods for "speedglm" and "speedlm" objects from **speedglm**. (#1)
* Added `prediction.bigLm()` method for "bigLm" objects from **bigFastlm**. (#1)
* Added `prediction.biglm()` and `prediction.bigglm()` methods for "biglm" and "bigglm" objects from **biglm**, including those based by `"ffdf"` from **ff**. (#1)

# prediction 0.2.8

* Changed internal behavior of `build_datalist()`. The function now returns an an `at_specification` attribute, which is a data frame representation of the `at` argument.

# prediction 0.2.6

* Due to a change in gam_1.15, `prediction.gam()` is now `prediction.Gam()` for "Gam" objects from **gam**. (#1)

# prediction 0.2.6

* Added `prediction.train()` method for "train" objects from **caret**. (#1)

# prediction 0.2.5

* The `at` argument in `build_datalist()` now accepts a data frame of combinations for limiting the set of levels.

# prediction 0.2.3

* Most `prediction()` methods gain a (experimental) `calculate_se` argument, which regulates whether to calculate standard errors for predictions. Setting to `FALSE` can improve performance if they are not needed.

# prediction 0.2.3

* `build_datalist()` gains an `as.data.frame` argument, which - if `TRUE` - returns a stacked data frame rather than a list. This argument is now used internally in most `prediction()` functions in an effort to improve performance. (#18)

# prediction 0.2.2

* Expanded test suite scope and fixed a few small bugs.
* Added a `summary.prediction()` method to interact with the average predicted values that are printed when `at != NULL`.

# prediction 0.2.1

* Added `prediction.knnreg()` method for "knnreg" objects from **caret**. (#1)
* Added `prediction.gausspr()` method for "gausspr" objects from **kernlab**. (#1)
* Added `prediction.ksvm()` method for "ksvm" objects from **kernlab**. (#1)
* Added `prediction.kqr()` method for "kqr" objects from **kernlab**. (#1)
* Added `prediction.earth()` method for "earth" objects from **earth**. (#1)
* Added `prediction.rpart()` method for "rpart" objects from **rpart**. (#1)

# prediction 0.2.0

* CRAN Release.
* Added `mean_or_mode.data.frame()` and `median_or_mode.data.frame()` methods.

# prediction 0.1.17

* Added `prediction.zeroinfl()` method for "zeroinfl" objects from **pscl**. (#1)
* Added `prediction.hurdle()` method for "hurdle" objects from **pscl**. (#1)
* Added `prediction.lme()` method for "lme" and "nlme" objects from **nlme**. (#1)
* Documented `prediction.merMod()`.

# prediction 0.1.16

* Added `prediction.plm()` method for "plm" objects from **plm**. (#1)

# prediction 0.1.15

* Expanded test suite considerably and updated `CONTRIBUTING.md` to reflect expected test-driven development.
* A few small code tweaks and bug fixes resulting from the updated test suite.

# prediction 0.1.14

* Added `prediction.mnp()` method for "mnp" objects from **MNP**. (#1)
* Added `prediction.mnlogit()` method for "mnlogit" objects from **mnlogit**. (#1)
* Added `prediction.gee()` method for "gee" objects from **gee**. (#1)
* Added `prediction.lqs()` method for "lqs" objects from **MASS**. (#1)
* Added `prediction.mca()` method for "mca" objects from **MASS**. (#1)
* Noted (built-in) support for "brglm" objects from **brglm** via the `prediction.glm()` method. (#1)

# prediction 0.1.13

* Added a `category` argument to `prediction()` methods for models of multilevel outcomes (e.g., ordered probit, etc.) to be dictate which level is expressed as the `"fitted"` column. (#14)
* Added an `at` argument to `prediction()` methods. (#13)
* Made `mean_or_mode()` and `median_or_mode()` S3 generics.
* Fixed a bug in `mean_or_mode()` and `median_or_mode()` where incorrect factor levels were being returned.

# prediction 0.1.12

* Added `prediction.princomp()` method for "princomp" objects from **stats**. (#1)
* Added `prediction.ppr()` method for "ppr" objects from **stats**. (#1)
* Added `prediction.naiveBayes()` method for "naiveBayes" objects from **e1071**. (#1)
* Added `prediction.rlm()` method for "rlm" objects from **MASS**. (#1)
* Added `prediction.qda()` method for "qda" objects from **MASS**. (#1)
* Added `prediction.lda()` method for "lda" objects from **MASS**. (#1)
* `find_data()` now respects the `subset` argument in an original model call. (#15)
* `find_data()` now respects the `na.action` argument in an original model call. (#15)
* `find_data()` now gracefully fails when a model is specified without a formula. (#16)
* `prediction()` methods no longer add a "fit" or "se.fit" class to any columns. Fitted values are identifiable by the column name only.

# prediction 0.1.11

* `build_datalist()` now returns `at` value combinations as a list.

# prediction 0.1.10

* Added `prediction.nnet()` method for "nnet" and "multinom" objects from **nnet**. (#1)

# prediction 0.1.9

* `prediction()` methods now return the value of `data` as part of the response data frame. (#8, h/t Ben Whalley)
* Slight change to `find_data()` methods for `"crch"` and `"hxlr"`. (#5)
* Added `prediction.glmx()` and `prediction.hetglm()` methods for "glmx" and "hetglm" objects from **glmx**. (#1)
* Added `prediction.betareg()` method for "betareg" objects from **betareg**. (#1)
* Added `prediction.rq()` method for "rq" objects from **quantreg**. (#1)
* Added `prediction.gam()` method for "gam" objects from **gam**. (#1)
* Expanded basic test suite.

# prediction 0.1.8

* Added `prediction()` and `find_data()` methods for `"crch"` `"hxlr"` objects from **crch**. (#4, h/t Carl Ganz)

# prediction 0.1.7

* Added `prediction()` and `find_data()` methods for `"merMod"` objects from **lme4**. (#1)

# prediction 0.1.6

* Moved the `seq_range()` function from **margins** to **prediction**.
* Moved the `build_datalist()` function from **margins** to **prediction**. This will simplify the ability to calculate arbitrary predictions.

# prediction 0.1.5

* Added `prediction.svm()` method for objects of class `"svm"` from **e1071**. (#1)
* Fixed a bug in `prediction.polr()` when attempting to pass a `type` argument, which is always ignored. A warning is now issued when attempting to override this.

# prediction 0.1.4

* Added `mean_or_mode()` and `median_or_mode()` functions, which provide a simple way to aggregate a variable of factor or numeric type. (#3)
* Added `prediction()` methods for various time-series model classes: "ar", "arima0", and "Arima".

# prediction 0.1.3

* `find_data()` is now a generic, methods for "lm", "glm", and "svyglm" classes. (#2, h/t Carl Ganz)

# prediction 0.1.2

* Added support for "svyglm" class from the **survey** package. (#1)
* Added tentative support for "clm" class from the **ordinal** package. (#1)

# prediction 0.1.0

* Initial package released.
