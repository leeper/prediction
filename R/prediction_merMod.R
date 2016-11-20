#' @rdname prediction
#' @export
prediction.merMod <- function(model, data = find_data(model), type = c("response", "link"), ...) {
    # setup data
    data <- data
    
    type <- match.arg(type)
    
    # extract predicted value at input value (value can only be 1 number)
    pred <- data.frame(fitted = predict(model, newdata = data, type = type, ...))
    pred[["se.fitted"]] <- NA_real_
    class(pred[["fitted"]]) <- c("fit", "numeric")
    class(pred[["se.fitted"]]) <- c("se.fit", "numeric")
    
    # obs-x-2 data.frame of predictions
    structure(list(fitted = pred[["fitted"]], 
                   se.fitted = pred[["se.fitted"]]), 
              class = c("prediction", "data.frame"), 
              row.names = seq_len(length(pred[["fitted"]])),
              type = type)
}
