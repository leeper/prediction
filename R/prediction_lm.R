#' @rdname prediction
#' @export
prediction.lm <- 
function(model, 
         data = find_data(model, parent.frame()), 
         at = NULL, 
         type = "response", 
         ...) {
    
    # extract predicted values
    data <- data
    if (missing(data) || is.null(data)) {
        pred <- predict(model, type = type, se.fit = TRUE, ...)
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
            out[[i]] <- cbind(out[[i]], fit = tmp[["fit"]], se.fit = tmp[["se.fit"]])
            rm(tmp)
        }
        pred <- do.call("rbind", out)
        names(pred)[names(pred) == "fit"] <- "fitted"
        names(pred)[names(pred) == "se.fit"] <- "se.fitted"
    }
    
    # obs-x-(ncol(data)+2) data.frame of predictions
    structure(pred, 
              class = c("prediction", "data.frame"),
              row.names = seq_len(nrow(pred)),
              at = if (is.null(at)) at else names(at), 
              model.class = class(model),
              type = type)
}
