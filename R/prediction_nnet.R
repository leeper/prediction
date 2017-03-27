#' @rdname prediction
#' @export
prediction.nnet <- function(model, data = find_data(model, parent.frame()), ...) {
    
    # extract predicted values
    if (missing(data)) {
        pred <- data.frame(fitted = predict(model, type = "class", ...),
                           se.fitted = NA_real_)
        probs <- data.frame(predict(model, type = "raw", ...))
        names(probs) <- paste0("Pr(", seq_len(ncol(probs)), ")")
    } else {
        pred <- data.frame(fitted = predict(model, newdata = data, type = "class", ...),
                           se.fitted = NA_real_)
        probs <- data.frame(predict(model, newdata = data, type = "raw", ...))
        names(probs) <- paste0("Pr(", seq_len(ncol(probs)), ")")
    }
    
    # obs-x-(ncol(data)+2+nlevels(outcome)) data.frame of predictions
    data <- data
    structure(if (!length(data)) cbind(pred, probs) else cbind(data, pred, probs),
              class = c("prediction", "data.frame"), 
              row.names = seq_len(length(pred[["fitted"]])),
              model.class = class(model),
              type = NULL)
}
