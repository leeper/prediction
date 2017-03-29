#' @rdname prediction
#' @export
prediction.polr <- function(model, data = find_data(model, parent.frame()), at = NULL, type = NULL, ...) {
    
    if (!is.null(type)) {
        warning("'type' is ignored for models of class 'polr' and 'multinom'")
    }
    
    # extract predicted values
    data <- data
    if (missing(data) || is.null(data)) {
        pred <- data.frame(fitted = predict(model, type = "class", ...),
                           se.fitted = NA_real_)
        probs <- as.data.frame(predict(model, type = "probs", ...))
    } else {
        pred <- data.frame(fitted = predict(model, newdata = data, type = "class", ...),
                           se.fitted = NA_real_)
        pred <- cbind(data, pred)
        probs <- as.data.frame(predict(model, newdata = data, type = "probs", ...))
    }
    names(probs) <- paste0("Pr(", names(probs), ")")
    pred <- cbind(pred, probs)
    
    # obs-x-(ncol(data)+2+nlevels(outcome)) data.frame of predictions
    structure(pred,
              class = c("prediction", "data.frame"), 
              row.names = seq_len(length(pred[["fitted"]])),
              at = if (is.null(at)) at else names(at), 
              model.class = class(model),
              type = NULL)
}

#' @rdname prediction
#' @export
prediction.multinom <- prediction.polr

