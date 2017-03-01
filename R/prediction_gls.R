#' @rdname prediction
#' @export
prediction.gls <- function(model, data, ...) {
    
    # extract predicted values
    if (missing(data)) {
        pred <- data.frame(fitted = predict(model, type = "class", ...),
                           se.fitted = NA_real_)
    } else {
        pred <- data.frame(fitted = predict(model, newdata = data, type = "class", ...),
                           se.fitted = NA_real_)
    }
    class(pred[["fitted"]]) <- c("fit", class(pred[["fit"]]))
    class(pred[["se.fitted"]]) <- c("se.fit", "numeric")
    
    # obs-x-(ncol(data)+2) data.frame of predictions
    data <- data
    structure(if (!length(data)) pred else cbind(data, pred),
              class = c("prediction", "data.frame"), 
              row.names = seq_len(length(pred[["fitted"]])),
              model.class = class(model),
              type = NULL)
}

