#' @rdname prediction
#' @export
prediction.betareg <- 
function(model, 
         data = find_data(model, parent.frame()),
         at = NULL, 
         type = c("response", "link", "precision", "variance", "quantile"), 
         ...) {
    
    type <- match.arg(type)
    
    # extract predicted value
    data <- data
    if (missing(data) || is.null(data)) {
        pred <- data.frame(fitted = predict(model, type = type, ...), 
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
            out[[i]] <- cbind(out[[i]], fitted = tmp, se.fitted = rep(NA_real_, length(tmp)))
            rm(tmp)
        }
        pred <- do.call("rbind", out)
        names(pred)[names(pred) == "fit"] <- "fitted"
        names(pred)[names(pred) == "se.fit"] <- "se.fitted"
    }
    
    # obs-x-(ncol(data)+2) data frame
    structure(pred, 
              class = c("prediction", "data.frame"), 
              row.names = seq_len(nrow(pred)),
              at = if (is.null(at)) at else names(at), 
              model.class = class(model),
              type = type)
}
