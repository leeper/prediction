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
        data <- data
        pred <- data.frame(fitted = predict(model, newdata = data, type = "class", ...),
                           se.fitted = NA_real_)
        probs <- data.frame(predict(model, newdata = data, type = "raw", ...))
        names(probs) <- paste0("Pr(", seq_len(ncol(probs)), ")")
    }
    class(pred[["fitted"]]) <- c("fit", class(pred[["fit"]]))
    class(pred[["se.fitted"]]) <- c("se.fit", "numeric")
    
    # obs-x-(ncol(data)+2+nlevels(outcome)) data.frame of predictions
    structure(if (missing(data)) cbind(pred, probs) else cbind(data, pred, probs),
              class = c("prediction", "data.frame"), 
              row.names = seq_len(length(pred[["fitted"]])),
              model.class = class(model),
              type = NULL)
}
