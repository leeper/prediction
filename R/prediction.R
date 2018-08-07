#' @rdname prediction
#' @name prediction-package
#' @title Extract Predictions from a Model Object
#' @description Extract predicted values via \code{\link[stats]{predict}} from a model object, conditional on data, and return a data frame.
#' @param model A model object, perhaps returned by \code{\link[stats]{lm}} or \code{\link[stats]{glm}}.
#' @param data A data.frame over which to calculate marginal effects. If missing, \code{\link{find_data}} is used to specify the data frame.
#' @param at A list of one or more named vectors, specifically values at which to calculate the predictions. These are used to modify the value of \code{data} (see \code{\link{build_datalist}} for details on use).
#' @param type A character string indicating the type of marginal effects to estimate. Mostly relevant for non-linear models, where the reasonable options are \dQuote{response} (the default) or \dQuote{link} (i.e., on the scale of the linear predictor in a GLM). For models of class \dQuote{polr} (from \code{\link[MASS]{polr}}), possible values are \dQuote{class} or \dQuote{probs}; both are returned.
#' @param vcov A matrix containing the variance-covariance matrix for estimated model coefficients, or a function to perform the estimation with \code{model} as its only argument.
#' @param calculate_se A logical indicating whether to calculate standard errors for observation-specific predictions and average predictions (if possible). The output will always contain a \dQuote{calculate_se} column regardless of this value; this only controls the calculation of standard errors. Setting it to \code{FALSE} may improve speed.
#' @param category For multi-level or multi-category outcome models (e.g., ordered probit, multinomial logit, etc.), a value specifying which of the outcome levels should be used for the \code{"fitted"} column. If missing, some default is chosen automatically.
#' @param \dots Additional arguments passed to \code{\link[stats]{predict}} methods.
#' @details This function is simply a wrapper around \code{\link[stats]{predict}} that returns a data frame containing the value of \code{data} and the predicted values with respect to all variables specified in \code{data}.
#' 
#' Methods are currently implemented for the following object classes:
#' \itemize{
#'   \item \dQuote{lm}, see \code{\link[stats]{lm}}
#'   \item \dQuote{glm}, see \code{\link[stats]{glm}}, \code{\link[MASS]{glm.nb}}, \code{\link[glmx]{glmx}}, \code{\link[glmx]{hetglm}}, \code{\link[brglm]{brglm}}
#'   \item \dQuote{ar}, see \code{\link[stats]{ar}}
#'   \item \dQuote{Arima}, see \code{\link[stats]{arima}}
#'   \item \dQuote{arima0}, see \code{\link[stats]{arima0}}
#'   \item \dQuote{bigglm}, see \code{\link[biglm]{bigglm}} (including \dQuote{ffdf}-backed models provided by \code{\link[ffbase]{bigglm.ffdf}})
#'   \item \dQuote{bigLm}, see \code{\link[bigFastlm]{bigLm}}
#'   \item \dQuote{betareg}, see \code{\link[betareg]{betareg}}
#'   \item \dQuote{bruto}, see \code{\link[mda]{bruto}}
#'   \item \dQuote{clm}, see \code{\link[ordinal]{clm}}
#'   \item \dQuote{coxph}, see \code{\link[survival]{coxph}}
#'   \item \dQuote{crch}, see \code{\link[crch]{crch}}
#'   \item \dQuote{earth}, see \code{\link[earth]{earth}}
#'   \item \dQuote{fda}, see \code{\link[mda]{fda}}
#'   \item \dQuote{Gam}, see \code{\link[gam]{gam}}
#'   \item \dQuote{gausspr}, see \code{\link[kernlab]{gausspr}}
#'   \item \dQuote{gee}, see \code{\link[gee]{gee}}
#'   \item \dQuote{glmnet}, see \code{\link[glmnet]{glmnet}}
#'   \item \dQuote{gls}, see \code{\link[nlme]{gls}}
#'   \item \dQuote{glimML}, see \code{\link[aod]{betabin}}, \code{\link[aod]{negbin}}
#'   \item \dQuote{glimQL}, see \code{\link[aod]{quasibin}}, \code{\link[aod]{quasipois}}
#'   \item \dQuote{hurdle}, see \code{\link[pscl]{hurdle}}
#'   \item \dQuote{hxlr}, see \code{\link[crch]{hxlr}}
#'   \item \dQuote{ivreg}, see \code{\link[AER]{ivreg}}
#'   \item \dQuote{knnreg}, see \code{\link[caret]{knnreg}}
#'   \item \dQuote{kqr}, see \code{\link[kernlab]{kqr}}
#'   \item \dQuote{ksvm}, see \code{\link[kernlab]{ksvm}}
#'   \item \dQuote{lda}, see \code{\link[MASS]{lda}}
#'   \item \dQuote{lme}, see \code{\link[nlme]{lme}}
#'   \item \dQuote{loess}, see \code{\link[stats]{loess}}
#'   \item \dQuote{lqs}, see \code{\link[MASS]{lqs}}
#'   \item \dQuote{mars}, see \code{\link[mda]{mars}}
#'   \item \dQuote{mca}, see \code{\link[MASS]{mca}}
#'   \item \dQuote{mclogit}, see \code{\link[mclogit]{mclogit}}
#'   \item \dQuote{mda}, see \code{\link[mda]{mda}}
#'   \item \dQuote{merMod}, see \code{\link[lme4]{lmer}}, \code{\link[lme4]{glmer}}
#'   \item \dQuote{mnlogit}, see \code{\link[mnlogit]{mnlogit}}
#'   \item \dQuote{mnp}, see \code{\link[MNP]{mnp}}
#'   \item \dQuote{naiveBayes}, see \code{\link[e1071]{naiveBayes}}
#'   \item \dQuote{nlme}, see \code{\link[nlme]{nlme}}
#'   \item \dQuote{nls}, see \code{\link[stats]{nls}}
#'   \item \dQuote{nnet}, see \code{\link[nnet]{nnet}}
#'   \item \dQuote{plm}, see \code{\link[plm]{plm}}
#'   \item \dQuote{polr}, see \code{\link[MASS]{polr}}
#'   \item \dQuote{polyreg}, see \code{\link[mda]{polyreg}}
#'   \item \dQuote{ppr}, see \code{\link[stats]{ppr}}
#'   \item \dQuote{princomp}, see \code{\link[stats]{princomp}}
#'   \item \dQuote{qda}, see \code{\link[MASS]{qda}}
#'   \item \dQuote{rlm}, see \code{\link[MASS]{rlm}}
#'   \item \dQuote{rpart}, see \code{\link[rpart]{rpart}}
#'   \item \dQuote{rq}, see \code{\link[quantreg]{rq}}
#'   \item \dQuote{selection}, see \code{\link[sampleSelection]{selection}}
#'   \item \dQuote{speedglm}, see \code{\link[speedglm]{speedglm}}
#'   \item \dQuote{speedlm}, see \code{\link[speedglm]{speedlm}}
#'   \item \dQuote{survreg}, see \code{\link[survival]{survreg}}
#'   \item \dQuote{svm}, see \code{\link[e1071]{svm}}
#'   \item \dQuote{svyglm}, see \code{\link[survey]{svyglm}}
#'   \item \dQuote{tobit}, see \code{\link[AER]{tobit}}
#'   \item \dQuote{train}, see \code{\link[caret]{train}}
#'   \item \dQuote{truncreg}, see \code{\link[truncreg]{truncreg}}
#'   \item \dQuote{zeroinfl}, see \code{\link[pscl]{zeroinfl}}
#' }
#' 
#' Where implemented, \code{prediction} also returns average predictions (and the variances thereof). Variances are implemented using the delta method, as described in \url{http://indiana.edu/~jslsoc/stata/ci_computations/spost_deltaci.pdf}.
#' 
#' @return A data frame with class \dQuote{prediction} that has a number of rows equal to number of rows in \code{data}, or a multiple thereof, if \code{!is.null(at)}. The return value contains \code{data} (possibly modified by \code{at} using \code{\link{build_datalist}}), plus a column containing fitted/predicted values (\code{"fitted"}) and a column containing the standard errors thereof (\code{"calculate_se"}). Additional columns may be reported depending on the object class. The data frame also carries attributes used by \code{print} and \code{summary}, which will be lost during subsetting.
#' @examples
#' require("datasets")
#' x <- lm(Petal.Width ~ Sepal.Length * Sepal.Width * Species, data = iris)
#' # prediction for every case
#' prediction(x)
#' 
#' # prediction for first case
#' prediction(x, iris[1,])
#' 
#' # basic use of 'at' argument
#' summary(prediction(x, at = list(Species = c("setosa", "virginica"))))
#' 
#' # basic use of 'at' argument
#' prediction(x, at = list(Sepal.Length = seq_range(iris$Sepal.Length, 5)))
#' 
#' # prediction at means/modes of input variables
#' prediction(x, at = lapply(iris, mean_or_mode))
#' 
#' # prediction with multi-category outcome
#' \dontrun{
#'   library("mlogit")
#'   data("Fishing", package = "mlogit")
#'   Fish <- mlogit.data(Fishing, varying = c(2:9), shape = "wide", choice = "mode")
#'   mod <- mlogit(mode ~ price + catch, data = Fish)
#'   prediction(mod)
#'   prediction(mod, category = 3)
#' }
#' 
#' @keywords models
#' @seealso \code{\link{find_data}}, \code{\link{build_datalist}}, \code{\link{mean_or_mode}}, \code{\link{seq_range}}
#' @import stats
#' @export
prediction <- function(model, ...) {
    UseMethod("prediction")
}

#' @rdname prediction
#' @export
prediction.default <- 
function(model, 
         data = find_data(model, parent.frame()), 
         at = NULL, 
         type = "response", 
         vcov = stats::vcov(model),
         calculate_se = TRUE,
         ...) {
    
    # extract predicted values
    data <- data
    if (missing(data) || is.null(data)) {
        if (isTRUE(calculate_se)) {
            pred <- predict(model, type = type, se.fit = TRUE, ...)
            pred <- make_data_frame(fitted = pred[["fit"]], se.fitted = pred[["se.fit"]])
        } else {
            pred <- predict(model, type = type, se.fit = FALSE, ...)
            pred <- make_data_frame(fitted = pred, se.fitted = rep(NA_real_, length(pred)))
        }
    } else {
        # setup data
        if (!is.null(at)) {
            data <- build_datalist(data, at = at, as.data.frame = TRUE)
            at_specification <- attr(data, "at_specification")
        }
        # calculate predictions
        if (isTRUE(calculate_se)) {
            tmp <- predict(model, newdata = data, type = type, se.fit = TRUE, ...)
            # cbind back together
            pred <- make_data_frame(data, fitted = tmp[["fit"]], se.fitted = tmp[["se.fit"]])
        } else {
            tmp <- predict(model, newdata = data, type = type, se.fit = FALSE, ...)
            # cbind back together
            pred <- make_data_frame(data, fitted = tmp, se.fitted = rep(NA_real_, nrow(data)))
        }
    }
    
    # variance(s) of average predictions
    J <- NULL
    vc <- NA_real_
    
    # output
    structure(pred, 
              class = c("prediction", "data.frame"),
              at = if (is.null(at)) at else at_specification,
              type = type,
              call = if ("call" %in% names(model)) model[["call"]] else NULL,
              model_class = class(model),
              row.names = seq_len(nrow(pred)),
              vcov = vc,
              jacobian = J,
              weighted = FALSE)
}
