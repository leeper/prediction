#' @rdname prediction
#' @export
prediction.hxlr <- 
function(model, 
         data = find_data(model), 
         type = c("class", "probability", "cumprob", "location", "scale"), 
         ...) {
    
    type <- match.arg(type)

    # extract predicted values
    if (missing(data)) {
        pred <- data.frame(fitted = predict(model, type = type, ...),
                           se.fitted = NA_real_)
    } else {
        pred <- data.frame(fitted = predict(model, newdata = data, type = type, ...),
                           se.fitted = NA_real_)
    }
    
    # obs-x-(ncol(data)+2) data.frame of predictions
    data <- data
    structure(if (!length(data)) pred else cbind(data, pred), 
              class = c("prediction", "data.frame"), 
              row.names = seq_len(length(pred[["fitted"]])),
              type = type)
}
