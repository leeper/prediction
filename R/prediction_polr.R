#' @rdname prediction
#' @export
prediction.polr <- function(model, data = find_data(model, parent.frame()), type = NULL, ...) {
    # setup data
    data <- data
    
    if (!is.null(type)) {
        warning("'type' is ignored for models of class 'polr' and 'multinom'")
    }
    
    # extract predicted value at input value (value can only be 1 number)
    pred <- data.frame(fit = predict(model, newdata = data, type = "class", ...))
    pred[["se.fit"]] <- rep(NA_real_, length(pred[["fit"]]))
    class(pred[["fit"]]) <- c("fit", class(pred[["fit"]]))
    class(pred[["se.fit"]]) <- c("se.fit", "numeric")
    probs <- as.data.frame(predict(model, newdata = data, type = "probs", ...))
    names(probs) <- paste0("Pr(", names(probs), ")")
    
    # obs-x-2 data.frame of predictions
    structure(cbind(list(fitted = pred[["fit"]], 
                         se.fitted = pred[["se.fit"]]),
                    probs),
              class = c("prediction", "data.frame"), 
              row.names = seq_len(length(pred[["fit"]])),
              model.class = class(model),
              type = NULL)
}

#' @rdname prediction
#' @export
prediction.multinom <- prediction.polr

