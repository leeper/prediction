#' @rdname prediction
#' @export
prediction.gls <- function(model, data, ...) {
    # setup data
    data <- data
    
    # extract predicted value at input value (value can only be 1 number)
    pred <- data.frame(fit = predict(model, newdata = data, type = "class", ...))
    pred[["se.fit"]] <- NA_real_
    class(pred[["fit"]]) <- c("fit", class(pred[["fit"]]))
    class(pred[["se.fit"]]) <- c("se.fit", "numeric")
    
    # obs-x-2 data.frame of predictions
    structure(list(fitted = pred[["fit"]], 
                   se.fitted = pred[["se.fit"]]),
              class = c("prediction", "data.frame"), 
              row.names = seq_len(length(pred[["fit"]])),
              model.class = class(model),
              type = NULL)
}

