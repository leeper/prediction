#' @rdname prediction
#' @title Extract Predictions from a Model Object
#' @description Extract predicted values via \code{\link[stats]{predict}} from a model object, conditional on data
#' @param model A model object, perhaps returned by \code{\link[stats]{lm}} or \code{\link[stats]{glm}}.
#' @param data A data.frame over which to calculate marginal effects. If missing, \code{\link{find_data}} is used to specify the data frame.
#' @param type A character string indicating the type of marginal effects to estimate. Mostly relevant for non-linear models, where the reasonable options are \dQuote{response} (the default) or \dQuote{link} (i.e., on the scale of the linear predictor in a GLM). For models of class \dQuote{polr} (from \code{\link[MASS]{polr}}), possible values are \dQuote{class} or \dQuote{probs}; both are returned.
#' @param bind A boolean indicating whether the dataset used should be bound the predicted values.
#' @param \dots Additional arguments passed to \code{\link[stats]{predict}} methods.
#' @details This function is simply a wrapper around \code{\link[stats]{predict}} that returns a data frame containing predicted values with respect to all variables specified in \code{data}.
#' 
#' Methods are currently implemented for the following object classes:
#' \itemize{
#'   \item \dQuote{lm}, see \code{\link[stats]{lm}}
#'   \item \dQuote{glm}, see \code{\link[stats]{glm}}, \code{\link[MASS]{glm.nb}}
#'   \item \dQuote{svyglm}, see \code{\link[survey]{svyglm}}
#'   \item \dQuote{ar}, see \code{\link[stats]{ar}}
#'   \item \dQuote{Arima}, see \code{\link[stats]{arima}}
#'   \item \dQuote{arima0}, see \code{\link[stats]{arima0}}
#'   \item \dQuote{loess}, see \code{\link[stats]{loess}}
#'   \item \dQuote{polr}, see \code{\link[MASS]{polr}}
#'   \item \dQuote{gls}, see \code{\link[nlme]{gls}}
#'   \item \dQuote{ivreg}, see \code{\link[AER]{ivreg}}
#'   \item \dQuote{nls}, see \code{\link[stats]{nls}}
#'   \item \dQuote{coxph}, see \code{\link[survival]{coxph}}
#'   \item \dQuote{survreg}, see \code{\link[survival]{survreg}}
#'   \item \dQuote{svm}, see \code{\link[e1071]{svm}}
#'   \item \dQuote{crch}, see \code{\link[crch]{crch}}
#'   \item \dQuote{hxlr}, see \code{\link[crch]{hxlr}}
#' }
#' 
#' @return A data.frame with class \dQuote{prediction} that has a number of rows equal to number of rows in \code{data}, where each row is an observation and the first two columns represent fitted/predicted values (\code{fitted}) and the standard errors thereof (\code{se.fitted}). Additional columns may be reported depending on the object class.
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
#' # bind predictions to data used for predictions
#' prediction(x, bind=TRUE)
#' 
#' @keywords models
#' @seealso \code{\link{find_data}}, \code{\link{build_datalist}}, \code{\link{mean_or_mode}}, \code{\link{seq_range}}
#' @importFrom stats predict get_all_vars model.frame
#' @export
prediction <- function(model, data, bind = TRUE, ...) {
   # wrap prediction_() with additional logic
   predictions <- prediction_(model, data, ...)
   
   # bind the results to the original data 
   if (bind == T){
      predictions <- cbind(data, predictions)
    } 
   predictions
}

prediction_ <- function(model, data, ...){
  UseMethod("prediction")
}

#' @rdname prediction
#' @export
prediction.default <- function(model, data = find_data(model, parent.frame()), type = "response", ...) {
    # setup data
    data <- data
    
    # extract predicted value at input value (value can only be 1 number)
    pred <- predict(model, newdata = data, type = type, se.fit = TRUE, ...)
    class(pred[["fit"]]) <- c("fit", "numeric")
    class(pred[["se.fit"]]) <- c("se.fit", "numeric")
    
    # obs-x-2 data.frame of predictions
    structure(list(fitted = pred[["fit"]], 
                   se.fitted = pred[["se.fit"]]), 
              class = c("prediction", "data.frame"), 
              row.names = seq_len(length(pred[["fit"]])),
              type = type)
}

#' @importFrom utils head
#' @export
print.prediction <- function(x, digits = 4, ...) {
    f <- x[["fitted"]]
    if (is.numeric(f)) {
        m <- mean(x[["fitted"]], na.rm = TRUE)
        m <- sprintf(paste0("%0.", digits, "f"), m)
        message(paste0("Average prediction: ", m, ", for ", length(f), " ", ngettext(length(f), "observation", "observations")))
    } else if (is.factor(f)) {
        m <- sort(table(x[["fitted"]]), decreasing = TRUE)[1]
        message(paste0("Modal prediction: ", shQuote(names(m)), " for ", m, " of ", length(f), " ", 
                ngettext(length(f), "observation", "observations"),
                " with total ", nlevels(f), " ", ngettext(nlevels(f), "level", "levels") ))
    } else {
        print(head(x), ...)
    }
    invisible(x)
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
