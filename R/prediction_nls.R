#' @rdname prediction
#' @export
prediction.nls <- function(model, data = find_data(model, parent.frame()), ...) {
    # setup data
    data <- data
    
    # extract predicted value at input values
    pred <- data.frame(fit = predict(model, newdata = data, ...))
    pred[["se.fit"]] <- NA_real_
    class(pred[["fit"]]) <- c("fit", "numeric")
    class(pred[["se.fit"]]) <- c("se.fit", "numeric")
    
    # obs-x-2 data.frame of predictions
    structure(list(fitted = pred[["fit"]], 
                   se.fitted = pred[["se.fit"]]), 
              class = c("prediction", "data.frame"), 
              row.names = seq_len(length(pred[["fit"]])),
              model.class = class(model),
              type = NULL)
}

