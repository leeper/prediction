#' @rdname prediction
#' @export
prediction.polr <- function(model, data = find_data(model, parent.frame()), type = NULL, ...) {
    
    if (!is.null(type)) {
        warning("'type' is ignored for models of class 'polr' and 'multinom'")
    }
    
    # extract predicted values
    if (missing(data)) {
        pred <- data.frame(fitted = predict(model, type = "class", ...),
                           se.fitted = NA_real_)
        probs <- as.data.frame(predict(model, type = "probs", ...))
    } else {
        data <- data
        pred <- data.frame(fitted = predict(model, newdata = data, type = "class", ...),
                           se.fitted = NA_real_)
        probs <- as.data.frame(predict(model, newdata = data, type = "probs", ...))
    }
    class(pred[["fitted"]]) <- c("fit", class(pred[["fit"]]))
    class(pred[["se.fitted"]]) <- c("se.fit", "numeric")
    
    names(probs) <- paste0("Pr(", names(probs), ")")
    
    # obs-x-(ncol(data)+2+nlevels(outcome)) data.frame of predictions
    structure(if (missing(data)) cbind(pred, probs) else cbind(data, pred, probs),
              class = c("prediction", "data.frame"), 
              row.names = seq_len(length(pred[["fitted"]])),
              model.class = class(model),
              type = NULL)
}

#' @rdname prediction
#' @export
prediction.multinom <- prediction.polr

