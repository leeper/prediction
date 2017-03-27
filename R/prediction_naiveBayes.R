# @rdname prediction
# @export
prediction.naiveBayes <- function(model, data = find_data(model, parent.frame()), type = NULL, ...) {
    
    data <- data
    if (missing(data)) {
        stop("'data' is required for objects of class 'mlogit'")
    }
    if (!is.null(type)) {
        warning("'type' is ignored for models of class 'naiveBayes'")
    }
    
    # extract predicted values
    pred <- data.frame(fitted = predict(model, newdata = data, type = "class", ...),
                       se.fitted = NA_real_)
    probs <- as.data.frame(predict(model, newdata = data, type = "raw", ...))
    
    names(probs) <- paste0("Pr(", names(probs), ")")
    
    # obs-x-(ncol(data)+2+nlevels(outcome)) data.frame of predictions
    structure(if (!length(data)) cbind(pred, probs) else cbind(data, pred, probs),
              class = c("prediction", "data.frame"), 
              row.names = seq_len(length(pred[["fitted"]])),
              model.class = class(model),
              type = NULL)
}
