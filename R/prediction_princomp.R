#' @rdname prediction
#' @export
prediction.princomp <- function(model, data = find_data(model, parent.frame()), at = NULL, ...) {
    
    # extract predicted values
    data <- data
    if (missing(data) || is.null(data)) {
        pred <- data.frame(predict(model, ...))
    } else {
        # setup data
        out <- build_datalist(data, at = at)
        for (i in seq_along(out)) {
            tmp <- predict(model, 
                           newdata = out[[i]], 
                           ...)
            out[[i]] <- cbind(out[[i]], tmp)
            rm(tmp)
        }
        pred <- do.call("rbind", out)
    }
    pred[["fitted"]] <- NA_real_
    pred[["se.fitted"]] <- NA_real_
    
    # obs-x-(ncol(data)+2) data.frame of predictions
    structure(pred, 
              class = c("prediction", "data.frame"), 
              row.names = seq_len(length(pred[["fitted"]])),
              at = if (is.null(at)) at else names(at), 
              model.class = class(model),
              type = NA_character_)
}
