#' @rdname prediction
#' @export
prediction.glmx <- 
function(model, 
         data = find_data(model, parent.frame()), 
         at = NULL, 
         type = c("response", "link"), 
         ...) {
    
    type <- match.arg(type)
    
    # extract predicted values
    data <- data
    if (missing(data) || is.null(data)) {
        pred <- data.frame(fitted = predict(model, newdata = data, type = type, ...),
                           se.fitted = NA_real_)
    } else {
        # reduce memory profile
        model[["model"]] <- NULL
        attr(model[["terms"]], ".Environment") <- NULL
    
        # setup data
        out <- build_datalist(data, at = at)
        for (i in seq_along(out)) {
            tmp <- predict(model, 
                           newdata = out[[i]], 
                           type = type, 
                           ...)
            out[[i]] <- cbind(out[[i]], fitted = tmp[["fit"]], se.fitted = tmp[["se.fit"]])
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
