#' @rdname prediction
#' @name prediction-package
#' @title Extract Predictions from a Model Object
#' @description Extract predicted values via \code{\link[stats]{predict}} from a model object, conditional on data, and return a data frame.
#' @param model A model object, perhaps returned by \code{\link[stats]{lm}} or \code{\link[stats]{glm}}.
#' @param data A data.frame over which to calculate marginal effects. If missing, \code{\link{find_data}} is used to specify the data frame.
#' @param at A list of one or more named vectors, specifically values at which to calculate the predictions. These are used to modify the value of \code{data} (see \code{\link{build_datalist}} for details on use).
#' @param type A character string indicating the type of marginal effects to estimate. Mostly relevant for non-linear models, where the reasonable options are \dQuote{response} (the default) or \dQuote{link} (i.e., on the scale of the linear predictor in a GLM). For models of class \dQuote{polr} (from \code{\link[MASS]{polr}}), possible values are \dQuote{class} or \dQuote{probs}; both are returned.
#' @param category For multi-level or multi-category outcome models (e.g., ordered probit, multinomial logit, etc.), a value specifying which of the outcome levels should be used for the \code{"fitted"} column. If missing, some default is chosen automatically.
#' @param \dots Additional arguments passed to \code{\link[stats]{predict}} methods.
#' @details This function is simply a wrapper around \code{\link[stats]{predict}} that returns a data frame containing the value of \code{data} and the predicted values with respect to all variables specified in \code{data}.
#' 
#' Methods are currently implemented for the following object classes:
#' \itemize{
#'   \item \dQuote{lm}, see \code{\link[stats]{lm}}
#'   \item \dQuote{glm}, see \code{\link[stats]{glm}}, \code{\link[MASS]{glm.nb}}, \code{\link[glmx]{glmx}}, \code{\link[glmx]{hetglm}}
#'   \item \dQuote{ar}, see \code{\link[stats]{ar}}
#'   \item \dQuote{Arima}, see \code{\link[stats]{arima}}
#'   \item \dQuote{arima0}, see \code{\link[stats]{arima0}}
#'   \item \dQuote{betareg}, see \code{\link[betareg]{betareg}}
#'   \item \dQuote{clm}, see \code{\link[ordinal]{clm}}
#'   \item \dQuote{coxph}, see \code{\link[survival]{coxph}}
#'   \item \dQuote{crch}, see \code{\link[crch]{crch}}
#'   \item \dQuote{gam}, see \code{\link[gam]{gam}}
#'   \item \dQuote{gls}, see \code{\link[nlme]{gls}}
#'   \item \dQuote{hxlr}, see \code{\link[crch]{hxlr}}
#'   \item \dQuote{ivreg}, see \code{\link[AER]{ivreg}}
#'   \item \dQuote{lda}, see \code{\link[MASS]{lda}}
#'   \item \dQuote{loess}, see \code{\link[stats]{loess}}
#'   \item \dQuote{lqs}, see \code{\link[MASS]{lqs}}
#'   \item \dQuote{mca}, see \code{\link[MASS]{mca}}
#'   \item \dQuote{mclogit}, see \code{\link[mclogit]{mclogit}}
#'   \item \dQuote{mlogit}, see \code{\link[mlogit]{mlogit}}
#'   \item \dQuote{naiveBayes}, see \code{\link[e1071]{naiveBayes}}
#'   \item \dQuote{nls}, see \code{\link[stats]{nls}}
#'   \item \dQuote{nnet}, see \code{\link[nnet]{nnet}}
#'   \item \dQuote{polr}, see \code{\link[MASS]{polr}}
#'   \item \dQuote{ppr}, see \code{\link[stats]{ppr}}
#'   \item \dQuote{princomp}, see \code{\link[stats]{princomp}}
#'   \item \dQuote{qda}, see \code{\link[MASS]{qda}}
#'   \item \dQuote{rlm}, see \code{\link[MASS]{rlm}}
#'   \item \dQuote{rq}, see \code{\link[quantreg]{rq}}
#'   \item \dQuote{selection}, see \code{\link[sampleSelection]{selection}}
#'   \item \dQuote{survreg}, see \code{\link[survival]{survreg}}
#'   \item \dQuote{svm}, see \code{\link[e1071]{svm}}
#'   \item \dQuote{svyglm}, see \code{\link[survey]{svyglm}}
#' }
#' 
#' @return A data frame with class \dQuote{prediction} that has a number of rows equal to number of rows in \code{data}, or a multiple thereof, if \code{!is.null(at)}. The return value contains \code{data} (possibly modified by \code{at} using \code{\link{build_datalist}}), plus a column containing fitted/predicted values (\code{"fitted"}) and a column containing the standard errors thereof (\code{"se.fitted"}). Additional columns may be reported depending on the object class.
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
#' prediction(x, at = list(Species = c("setosa", "virginica")))
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
#' @importFrom stats predict get_all_vars model.frame
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
         ...) {
    
    # extract predicted values
    data <- data
    if (missing(data) || is.null(data)) {
        pred <- predict(model, type = type, se.fit = TRUE, ...)
        pred <- data.frame(fitted = pred[["fit"]], se.fitted = pred[["se.fit"]])
    } else {
        # setup data
        out <- build_datalist(data, at = at)
        for (i in seq_along(out)) {
            tmp <- predict(model, 
                           newdata = out[[i]], 
                           type = type, 
                           se.fit = TRUE,
                           ...)
            out[[i]] <- cbind(out[[i]], fit = tmp[["fit"]], se.fit = tmp[["se.fit"]])
            rm(tmp)
        }
        pred <- do.call("rbind", out)
        names(pred)[names(pred) == "fit"] <- "fitted"
        names(pred)[names(pred) == "se.fit"] <- "se.fitted"
    }
    
    # obs-x-(ncol(data)+2) data.frame of predictions
    structure(pred, 
              class = c("prediction", "data.frame"),
              row.names = seq_len(nrow(pred)),
              at = if (is.null(at)) at else names(at), 
              model.class = class(model),
              type = type)
}
