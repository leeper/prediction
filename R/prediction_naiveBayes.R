# @rdname prediction
# @export
prediction.naiveBayes <- function(model, data = find_data(model, parent.frame()), at = NULL, type = NULL, ...) {
    
    # extract predicted values
    data <- data
    if (missing(data) || is.null(data)) {
        stop("'data' is required for objects of class 'mlogit'")
    }
    if (!is.null(type)) {
        warning("'type' is ignored for models of class 'naiveBayes'")
    }
    
    # setup data
    out <- build_datalist(data, at = at)
    for (i in seq_along(out)) {
        tmp <- predict(model, 
                       newdata = out[[i]], 
                       type = "class", 
                       ...)
        probs <- as.data.frame(predict(model, newdata = data, type = "raw", ...))
        names(probs) <- paste0("Pr(", names(probs), ")")
        out[[i]] <- cbind(out[[i]], fitted = tmp, se.fitted = rep(NA_real_, length(tmp)), probs)
        rm(tmp, probs)
    }
    pred <- do.call("rbind", out)
    
    # obs-x-(ncol(data)+2+nlevels(outcome)) data.frame of predictions
    structure(pred,
              class = c("prediction", "data.frame"), 
              row.names = seq_len(length(pred[["fitted"]])),
              at = if (is.null(at)) at else names(at), 
              model.class = class(model),
              type = NULL)
}
