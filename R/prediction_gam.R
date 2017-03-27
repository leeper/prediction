#' @rdname prediction
#' @export
prediction.gam <- 
function(model, 
         data = find_data(model, parent.frame()), 
         at = NULL, 
         type = c("response", "link", "terms"), 
         ...) {
    
    type <- match.arg(type)
    
    # extract predicted value
    if (missing(data)) {
        pred <- predict(model, type = type, se.fit = TRUE, ...)
        pred <- data.frame(fitted = pred[["fit"]], se.fitted = pred[["fit"]])
    } else {
        pred <- data.frame(fitted = predict(model, newdata = data, type = type, se.fit = FALSE, ...), 
                           se.fitted = NA_real_)
    }
    
    # obs-x-(ncol(data)+2) data.frame of predictions
    data <- data
    structure(if (!length(data)) pred else cbind(data, pred), 
              class = c("prediction", "data.frame"), 
              row.names = seq_len(length(pred[["fitted"]])),
              model.class = class(model),
              type = type)
}
