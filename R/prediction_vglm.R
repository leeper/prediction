# @rdname prediction
# @export
prediction.vglm <- 
function(model, 
         data = find_data(model, parent.frame()), 
         at = NULL, 
         type = c("response", "link"), 
         category,
         ...) {
    
    type <- match.arg(type)
    
    # extract predicted values
    data <- data
    arg <- list(...)
    if (missing(data) || is.null(data)) {
        if ("se.fit" %in% names(arg)) {
            tmp <- predict(model, type = type, ...)
            pred <- as.data.frame(tmp[["fitted.values"]], tmp[["se.fit"]])
        } else {
            pred <- as.data.frame(predict(model, type = type, se.fit = FALSE, ...))
        }
    } else {
        # setup data
        out <- build_datalist(data, at = at)
        for (i in seq_along(out)) {
            if ("se.fit" %in% names(arg)) {
                tmp <- predict(model, 
                               newdata = out[[i]], 
                               type = type, 
                               ...)
                out[[i]] <- cbind(out[[i]], tmp[["fitted.values"]], tmp[["se.fit"]])
            } else {
                tmp <- predict(model, 
                               newdata = out[[i]], 
                               type = type, 
                               ...)
                out[[i]] <- cbind(out[[i]], tmp[["fitted.values"]])
            }
            rm(tmp)
        }
        pred <- do.call("rbind", out)
    }
    
    # handle category argument
    if (missing(category)) {
        pred[["fitted"]] <- names(pred)[!names(pred) %in% names(data)][1L]
    } else {
        w <- grep(category, names(pred))
        if (!length(w)) {
            stop(sprintf("category %s not found", category))
        }
        pred[["fitted"]] <- pred[[ w[1L] ]]
    }
    
    # obs-x-(ncol(data)+2) data frame
    structure(pred, 
              class = c("prediction", "data.frame"), 
              row.names = seq_len(nrow(pred)),
              at = if (is.null(at)) at else names(at), 
              model.class = class(model),
              type = type)
}
