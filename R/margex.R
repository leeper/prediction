#' @rdname margex
#' @docType data
#' @title Artificial data for margins, copied from Stata
#' @description The dataset is identical to the one provided by Stata and available from \code{webuse::webuse("margex")} with categorical variables explicitly encoded as factors.
#' @format A data frame with 3000 observations on the following 11 variables.
#'  \describe{
#'    \item{\samp{y}}{A numeric vector}
#'    \item{\samp{outcome}}{A binary numeric vector with values (0,1)}
#'    \item{\samp{sex}}{A factor with two levels}
#'    \item{\samp{group}}{A factor with three levels}
#'    \item{\samp{age}}{A numeric vector}
#'    \item{\samp{distance}}{A numeric vector}
#'    \item{\samp{ycn}}{A numeric vector}
#'    \item{\samp{yc}}{A numeric vector}
#'    \item{\samp{treatment}}{A factor with two levels}
#'    \item{\samp{agegroup}}{A factor with three levels}
#'    \item{\samp{arm}}{A factor with three levels}
#'  }
#' @source \url{http://www.stata-press.com/data/r14/margex.dta}
#' @examples
#' \donttest{
#' 
#' # Examples from Stata's help files
#' # Also available from: webuse::webuse("margex")
#' data("margex")
#' 
#' # A simple case after regress
#' # . regress y i.sex i.group
#' # . margins sex
#' m1 <- lm(y ~ factor(sex) + factor(group), data = margex)
#' prediction(m1, at = list(sex = 0:1))
#' 
#' # A simple case after logistic
#' # . logistic outcome i.sex i.group
#' # . margins sex
#' m2 <- glm(outcome ~ sex + group, binomial(), data = margex)
#' prediction(m2, at = list(sex = 0:1))
#' 
#' # Average response versus response at average
#' # . margins sex
#' prediction(m2, at = list(sex = c("male", "female")))
#' # . margins sex, atmeans
#' ## TODO
#' 
#' # Multiple margins from one margins command
#' # . margins sex group
#' prediction(m2, at = list(sex = c("male", "female")))
#' prediction(m2, at = list(group = c("1", "2", "3")))
#' 
#' # Margins with interaction terms
#' # . logistic outcome i.sex i.group sex#group
#' # . margins sex group
#' m3 <- glm(outcome ~ sex * group, binomial(), data = margex)
#' prediction(m3, at = list(sex = c("male", "female")))
#' prediction(m3, at = list(group = c("1", "2", "3")))
#' 
#' # Margins with continuous variables
#' # . logistic outcome i.sex i.group sex#group age
#' # . margins sex group
#' m4 <- glm(outcome ~ sex * group + age, binomial(), data = margex)
#' prediction(m4, at = list(sex = c("male", "female")))
#' prediction(m4, at = list(group = c("1", "2", "3")))
#' 
#' # Margins of continuous variables
#' # . margins, at(age=40)
#' prediction(m4, at = list(age = 40))
#' # . margins, at(age=(30 35 40 45 50))
#' prediction(m4, at = list(age = c(30, 35, 40, 45, 50)))
#' 
#' # Margins of interactions
#' # . margins sex#group
#' prediction(m4, at = list(sex = c("male", "female"), group = c("1", "2", "3")))
#' 
#' }
#' @seealso \code{\link{prediction}}
"margex"
