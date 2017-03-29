#' @rdname prediction
#' @export
prediction.gam <- 
function(model, 
         data = find_data(model, parent.frame()), 
         at = NULL, 
         type = c("response", "link", "terms"), 
         ...) {
    
    type <- match.arg(type)
    
    # extract predicted value
    data <- data
    if (missing(data) || is.null(data)) {
        pred <- predict(model, type = type, se.fit = TRUE, ...)
        pred <- data.frame(fitted = pred[["fit"]], se.fitted = pred[["se.fit"]][,1L])
    } else {
        # setup data
        out <- build_datalist(data, at = at)
        for (i in seq_along(out)) {
            # no standard errors when using `newdata`
            tmp <- predict(model, 
                           newdata = out[[i]], 
                           type = type, 
                           se.fit = FALSE,
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
