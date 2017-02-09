#' @rdname prediction
#' @export
prediction.svyglm <- 
function(model, 
         data = find_data(model, parent.frame()), 
         type = c("response", "link"), 
         ...) {
    
    type <- match.arg(type)
    
    # extract predicted values
    if (missing(data)) {
        pred <- predict(model, type = type, se.fit = TRUE, ...)
    } else {
        data <- data
        pred <- predict(model, newdata = data, type = type, se.fit = TRUE, ...)
    }
    pred <- list(fitted = unclass(pred), 
                 se.fitted = sqrt(unname(attributes(pred)[["var"]])))
    attributes(pred[["fitted"]]) <- NULL
    class(pred[["fitted"]]) <- c("fit", "numeric")
    class(pred[["se.fitted"]]) <- c("se.fit", "numeric")
    
    # obs-x-(ncol(data)+2) data.frame of predictions
    structure(if (missing(data)) data.frame(pred) else cbind(data, pred), 
              class = c("prediction", "data.frame"), 
              row.names = seq_len(length(pred[["fitted"]])),
              model.class = class(model),
              type = type)
}

