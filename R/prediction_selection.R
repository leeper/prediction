#' @rdname prediction
#' @export
prediction.selection <- function(model, data = find_data(model, parent.frame()), type = "response", ...) {
    
    # extract predicted value at input value
    if (missing(data)) {
        pred <- data.frame(fitted = predict(model, ...), 
                           se.fitted = NA_real_)
    } else {
        pred <- data.frame(fitted = predict(model, newdata = data, ...), 
                           se.fitted = NA_real_)
    }
    
    # obs-x-(ncol(data)+2) data.frame of predictions
    data <- data
    structure(if (!length(data)) pred else cbind(data, pred), 
              class = c("prediction", "data.frame"), 
              row.names = seq_len(length(pred[["fitted"]])),
              type = type)
}
