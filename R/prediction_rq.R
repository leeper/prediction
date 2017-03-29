#' @rdname prediction
#' @export
prediction.rq <- 
function(model, 
         data = find_data(model, parent.frame()), 
         at = NULL, 
         ...) {
    
    # extract predicted value at input value
    data <- data
    if (missing(data) || is.null(data)) {
        pred <- data.frame(fitted = predict(model, ...), 
                           se.fitted = NA_real_)
    } else {
        # setup data
        out <- build_datalist(data, at = at)
        for (i in seq_along(out)) {
            tmp <- predict(model, 
                           newdata = out[[i]], 
                           ...)
            out[[i]] <- cbind(out[[i]], fitted = tmp, se.fitted = rep(NA_real_, length(tmp)))
            rm(tmp)
        }
        pred <- do.call("rbind", out)
    }
    
    # obs-x-(ncol(data)+2) data.frame of predictions
    structure(pred, 
              class = c("prediction", "data.frame"), 
              row.names = seq_len(length(pred[["fitted"]])),
              at = if (is.null(at)) at else names(at), 
              model.class = class(model),
              type = type)
}
