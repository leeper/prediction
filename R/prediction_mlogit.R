# @rdname prediction
# @export
prediction.mlogit <- function(model, data = find_data(model, parent.frame()), at = NULL, ...) {
    
    # extract predicted values
    data <- data
    if (missing(data) || is.null(data)) {
        stop("'data' is required for objects of class 'mlogit'")
    }
    # setup data
    out <- build_datalist(data, at = at)
    for (i in seq_along(out)) {
        tmp <- data.frame(predict(model, newdata = out[[i]], ...))
        names(tmp) <- paste0("Pr(", seq_len(ncol(tmp)), ")")
        out[[i]] <- cbind(out[[i]], tmp, fitted = rep(NA_real_, nrow(tmp)), se.fitted = rep(NA_real_, nrow(tmp)))
        rm(tmp)
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
