#' @rdname prediction
#' @title Extract Predictions from a Model Object
#' @description Extract predicted values via \code{\link[stats]{predict}} from a model object, conditional on data
#' @param model A model object, perhaps returned by \code{\link[stats]{lm}} or \code{\link[stats]{glm}}.
#' @param data A data.frame over which to calculate marginal effects. If missing, \code{\link{find_data}} is used to specify the data frame.
#' @param at A list of one or more named vectors, specifically values at which to calculate the predictions. These are used to modify the value of \code{data} (see \code{\link{build_datalist}} for details on use).
#' @param type A character string indicating the type of marginal effects to estimate. Mostly relevant for non-linear models, where the reasonable options are \dQuote{response} (the default) or \dQuote{link} (i.e., on the scale of the linear predictor in a GLM). For models of class \dQuote{polr} (from \code{\link[MASS]{polr}}), possible values are \dQuote{class} or \dQuote{probs}; both are returned.
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
#'   \item \dQuote{loess}, see \code{\link[stats]{loess}}
#'   \item \dQuote{nls}, see \code{\link[stats]{nls}}
#'   \item \dQuote{nnet}, see \code{\link[nnet]{nnet}}
#'   \item \dQuote{polr}, see \code{\link[MASS]{polr}}
#'   \item \dQuote{rq}, see \code{\link[quantreg]{rq}}
#'   \item \dQuote{selection}, see \code{\link[sampleSelection]{selection}}
#'   \item \dQuote{survreg}, see \code{\link[survival]{survreg}}
#'   \item \dQuote{svm}, see \code{\link[e1071]{svm}}
#'   \item \dQuote{svyglm}, see \code{\link[survey]{svyglm}}
#' }
#' 
#' @return A data.frame with class \dQuote{prediction} that has a number of rows equal to number of rows in \code{data}, where each row is an observation and the last two columns represent fitted/predicted values (\code{fitted}) and the standard errors thereof (\code{se.fitted}). Additional columns may be reported depending on the object class.
#' @examples
#' require("datasets")
#' x <- lm(Petal.Width ~ Sepal.Length * Sepal.Width * Species, data = iris)
#' # prediction for every case
#' prediction(x)
#' 
#' # prediction for first case
#' prediction(x, iris[1,])
#' 
#' # prediction at means/modes of input variables
#' prediction(x, lapply(iris, mean_or_mode))
#' 
#' @keywords models
#' @seealso \code{\link{find_data}}, \code{\link{build_datalist}}, \code{\link{mean_or_mode}}, \code{\link{seq_range}}
#' @importFrom stats predict get_all_vars model.frame
#' @export
prediction <- function(model, data, ...) {
    UseMethod("prediction")
}

#' @rdname prediction
#' @export
prediction.default <- function(model, data = find_data(model, parent.frame()), at = NULL, type = "response", ...) {
    
    # extract predicted values
    if (missing(data)) {
        pred <- predict(model, type = type, se.fit = TRUE, ...)
    } else {
        # reduce memory profile
        model[["model"]] <- NULL
        attr(model[["terms"]], ".Environment") <- NULL
    
        # setup data
        data_list <- build_datalist(data, at = at)
        if (is.null(names(data_list))) {
            names(data_list) <- NA_character_
        }
        out <- list()
        for (i in seq_along(data_list)) {
            out[[i]] <- cbind(data_list[[i]],
                              predict(model, 
                                      data = data_list[[i]], 
                                      type = type, 
                                      se.fit = TRUE,
                                      ...)
                              )
        }
        pred <- do.call("rbind", out)
    }
    names(pred)[names(pred) == "fit"] <- "fitted"
    names(pred)[names(pred) == "se.fit"] <- "se.fitted"
    class(pred[["fitted"]]) <- c("fit", "numeric")
    class(pred[["se.fitted"]]) <- c("se.fit", "numeric")
    
    # obs-x-(ncol(data)+2) data.frame of predictions
    structure(if (!length(data)) {
                  data.frame(pred[c("fitted", "se.fitted")])
              } else { 
                  pred
              }, 
              class = c("prediction", "data.frame"),
              row.names = seq_len(length(pred[["fitted"]])),
              at = at, 
              type = type)
}

#' @importFrom utils head
#' @export
head.prediction <- function(x, ...) {
    head(`class<-`(x, "data.frame"), ...)
}

#' @importFrom utils tail
#' @export
tail.prediction <- function(x, ...) {
    tail(`class<-`(x, "data.frame"), ...)
}
