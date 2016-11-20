#' @rdname prediction
#' @export
prediction.Arima <- function(model, data, ...) {
    # setup data
    if (!missing(data)) {
        stop("There is no 'data' argument for objects of class 'Arima'")
    }
    
    # extract predicted value at input value
    pred <- predict(object = model, se.fit = TRUE, ...)
    names(pred) <- c("fit", "se.fit")
    class(pred[["fit"]]) <- c("fit", "numeric")
    class(pred[["se.fit"]]) <- c("se.fit", "numeric")
    
    # obs-x-2 data.frame of predictions
    structure(list(fitted = pred[["fit"]], 
                   se.fitted = pred[["se.fit"]]), 
              class = c("prediction", "data.frame"), 
              row.names = seq_len(length(pred[["fit"]])),
              model.class = class(model),
              type = NULL)
}

