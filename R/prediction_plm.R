#' @rdname prediction
#' @export
prediction.plm <- 
function(model, 
         data = find_data(model, parent.frame()), 
         at = NULL, 
         ...) {
    
    # extract predicted values
    data <- data
    if (missing(data) || is.null(data)) {
        pred <- data.frame(fitted = predict(model, ...))
    } else {
        # setup data
        out <- build_datalist(data, at = at)
        for (i in seq_along(out)) {
            out[[i]] <- cbind(out[[i]], fitted = predict(model, newdata = out[[i]], ...))
        }
        pred <- do.call("rbind", out)
    }
    pred[["se.fitted"]] <- NA_real_
    
    # obs-x-(ncol(data)+2) data frame
    structure(pred, 
              class = c("prediction", "data.frame"),
              row.names = seq_len(nrow(pred)),
              at = if (is.null(at)) at else names(at), 
              model.class = class(model),
              type = NULL)
}
