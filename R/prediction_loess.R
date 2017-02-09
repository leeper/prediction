#' @rdname prediction
#' @export
prediction.loess <- function(model, data = find_data(model, parent.frame()), type = "response", ...) {
    
    type <- match.arg(type)
    
    # extract predicted values
    if (missing(data)) {
        pred <- predict(model, type = type, se = TRUE, ...)
    } else {
        data <- data
        pred <- predict(model, newdata = data, type = type, se = TRUE, ...)
    }
    class(pred[["fit"]]) <- c("fit", "numeric")
    class(pred[["se.fit"]]) <- c("se.fit", "numeric")
    names(pred)[names(pred) == "fit"] <- "fitted"
    names(pred)[names(pred) == "se.fit"] <- "se.fitted"
    
    # obs-x-(ncol(data)+2) data.frame of predictions
    structure(cbind(data, pred), 
              class = c("prediction", "data.frame"), 
              row.names = seq_len(length(pred[["fitted"]])),
              model.class = class(model),
              type = type)
}

