#' @rdname prediction
#' @export
prediction.glmx <- function(model, data = find_data(model, parent.frame()), at = NULL, type = c("response", "link"), ...) {
    # setup data
    data <- data
    
    type <- match.arg(type)
    
    # extract predicted value at input value (value can only be 1 number)
    pred <- data.frame(fitted = predict(model, newdata = data, type = type, ...),
                       se.fitted = NA_real_)
    
    # obs-x-(ncol(data)+2) data.frame of predictions
    structure(if (missing(data)) pred else cbind(data, pred), 
              class = c("prediction", "data.frame"), 
              row.names = seq_len(length(pred[["fitted"]])),
              model.class = class(model),
              type = type)
}
