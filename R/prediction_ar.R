#' @rdname prediction
#' @export
prediction.ar <- function(model, data, ...) {
    
    # extract predicted values
    if (missing(data)) {
        pred <- predict(object = model, se.fit = TRUE, ...)
    } else {
        pred <- predict(object = model, newdata = data, se.fit = TRUE, ...)
    }
    names(pred) <- c("fitted", "se.fitted")
    class(pred[["fitted"]]) <- c("fit", "numeric")
    class(pred[["se.fitted"]]) <- c("se.fit", "numeric")
    
    # obs-x-(ncol(data)+2) data.frame of predictions
    data <- data
    structure(if (!length(data)) data.frame(pred) else cbind(data, pred), 
              class = c("prediction", "data.frame"), 
              row.names = seq_len(length(pred[["fitted"]])),
              model.class = class(model),
              type = NULL)
}

#' @rdname prediction
#' @export
prediction.arima0 <- prediction.ar
