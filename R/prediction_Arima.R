#' @rdname prediction
#' @export
prediction.Arima <- function(model, data, ...) {
    
    # extract predicted values
    if (missing(data)) {
        pred <- predict(object = model, se.fit = TRUE, ...)
        names(pred) <- c("fitted", "se.fitted")
        class(pred[["fitted"]]) <- c("fit", "numeric")
        class(pred[["se.fitted"]]) <- c("se.fit", "numeric")
    } else {
        stop("There is no 'data' argument for objects of class 'Arima'")
    }
    
    # obs-x-(ncol(data)+2) data.frame of predictions
    structure(if (missing(data)) data.frame(pred) else cbind(data, pred), 
              class = c("prediction", "data.frame"), 
              row.names = seq_len(length(pred[["fitted"]])),
              model.class = class(model),
              type = NULL)
}

