#' @rdname prediction
#' @export
prediction.ivreg <- function(model, data = find_data(model, parent.frame()), ...) {
    
    # extract predicted values
    if (missing(data)) {
        pred <- data.frame(fitted = predict(model, ...),
                           se.fitted = NA_real_)
    } else {
        pred <- data.frame(fit = predict(model, newdata = data, ...),
                           se.fitted = NA_real_)
    }
    
    # obs-x-(ncol(data)+2) data.frame of predictions
    data <- data
    structure(if (!length(data)) pred else cbind(data, pred), 
              class = c("prediction", "data.frame"), 
              row.names = seq_len(length(pred[["fitted"]])),
              model.class = class(model),
              type = NULL)
}

