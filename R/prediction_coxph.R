#' @rdname prediction
#' @export
prediction.coxph <- function(model, data = find_data(model, parent.frame()), type = c("risk", "expected", "lp"), ...) {
    # setup data
    data <- data
    
    type <- match.arg(type)
    
    # extract predicted value at input value (value can only be 1 number)
    pred <- predict(model, newdata = data, type = type, se.fit = TRUE, ...)
    class(pred[["fit"]]) <- c("fit", "numeric")
    class(pred[["se.fit"]]) <- c("se.fit", "numeric")
    
    # obs-x-2 data.frame of predictions
    structure(list(fitted = pred[["fit"]], 
                   se.fitted = pred[["se.fit"]]), 
              class = c("prediction", "data.frame"), 
              row.names = seq_len(length(pred[["fit"]])),
              model.class = class(model),
              type = type)
}

