#' @rdname prediction
#' @export
prediction.ar <- function(model, data, at = NULL, ...) {
    
    # extract predicted values
    data <- data
    if (missing(data)) {
        pred <- predict(object = model, se.fit = TRUE, ...)
        pred <- data.frame(fitted = pred[["fit"]], se.fitted = pred[["se.fit"]])
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
                           se.fit = TRUE,
                           ...)
            out[[i]] <- cbind(out[[i]], fitted = tmp[["fit"]], se.fitted = tmp[["se.fit"]])
            rm(tmp)
        }
        pred <- do.call("rbind", out)
    }
    
    # obs-x-(ncol(data)+2) data.frame of predictions
    structure(pred, 
              class = c("prediction", "data.frame"), 
              row.names = seq_len(nrow(pred)),
              at = if (is.null(at)) at else names(at), 
              model.class = class(model),
              type = NULL)
}

#' @rdname prediction
#' @export
prediction.arima0 <- prediction.ar
