#' @rdname prediction
#' @export
prediction.hxlr <- 
function(model, 
         data = find_data(model), 
         type = c("class", "probability", "cumprob", "location", "scale"), 
         ...) {
    # setup data
    data <- data

    type <- match.arg(type)

    # extract predicted value at input value (value can only be 1 number)
    pred <- data.frame(fitted = predict(model, newdata = data, type = type, ...))
    pred[["se.fitted"]] <- NA_real_
    class(pred[["fitted"]]) <- c("fit", "numeric")
    class(pred[["se.fitted"]]) <- c("se.fit", "numeric")
    names(pred)[names(pred) == "fit"] <- "fitted"
    names(pred)[names(pred) == "se.fit"] <- "se.fitted"

    # obs-x-(ncol(data)+2) data.frame of predictions
    structure(cbind(data, pred), 
              class = c("prediction", "data.frame"), 
              row.names = seq_len(length(pred[["fitted"]])),
              type = type)
}
