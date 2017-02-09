#' @rdname prediction
#' @export
prediction.gam <- function(model, data = find_data(model, parent.frame()), type = c("response", "link", "terms"), ...) {
    
    type <- match.arg(type)
    
    # extract predicted value at input value (value can only be 1 number)
    if (missing(data)) {
        pred <- predict(model, type = type, se.fit = TRUE, ...)
        pred <- data.frame(fitted = pred[["fit"]], se.fitted = pred[["fit"]])
    } else {
        data <- data
        pred <- data.frame(fitted = predict(model, newdata = data, type = type, se.fit = FALSE, ...), 
                           se.fitted = NA_real_)
    }
    names(pred) <- c("fitted", "se.fitted")
    class(pred[["fitted"]]) <- c("fit", "numeric")
    class(pred[["se.fitted"]]) <- c("se.fit", "numeric")
    
    # obs-x-(ncol(data)+2) data.frame of predictions
    structure(cbind(data, pred), 
              class = c("prediction", "data.frame"), 
              row.names = seq_len(length(pred[["fitted"]])),
              model.class = class(model),
              type = type)
}
