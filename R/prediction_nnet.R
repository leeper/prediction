#' @rdname prediction
#' @export
prediction.nnet <- function(model, data = find_data(model, parent.frame()), at = NULL, ...) {
    
    # extract predicted values
    data <- data
    if (missing(data) || is.null(data)) {
        pred <- data.frame(fitted = predict(model, type = "class", ...),
                           se.fitted = NA_real_)
        probs <- data.frame(predict(model, type = "raw", ...))
        names(probs) <- paste0("Pr(", seq_len(ncol(probs)), ")")
    } else {
        pred <- data.frame(fitted = predict(model, newdata = data, type = "class", ...),
                           se.fitted = NA_real_)
        pred <- cbind(data, pred)
        probs <- data.frame(predict(model, newdata = data, type = "raw", ...))
        names(probs) <- paste0("Pr(", seq_len(ncol(probs)), ")")
    }
    pred <- cbind(pred, probs)
    
    # obs-x-(ncol(data)+2+nlevels(outcome)) data.frame of predictions
    structure(pred,
              class = c("prediction", "data.frame"), 
              row.names = seq_len(length(pred[["fitted"]])),
              at = if (is.null(at)) at else names(at), 
              model.class = class(model),
              type = NULL)
}
