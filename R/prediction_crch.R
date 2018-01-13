#' @rdname prediction
#' @export
prediction.crch <- 
function(model, 
         data = find_data(model), 
         at = NULL, 
         type = c("response", "location", "scale", "quantile"), 
         se.fitted = TRUE,
         ...) {
    
    type <- match.arg(type)

    # extract predicted values
    data <- data
    if (missing(data) || is.null(data)) {
        pred <- data.frame(fitted = predict(model, type = type, ...),
                           se.fitted = NA_real_)
    } else {
        # setup data
        if (is.null(at)) {
            out <- data
        } else {
            out <- build_datalist(data, at = at, as.data.frame = TRUE)
        }
        # calculate predictions
        tmp <- predict(model, 
                       newdata = out, 
                       type = type, 
                       ...)
        # cbind back together
        pred <- cbind(out, fitted = tmp, se.fitted = rep(NA_real_, length(tmp)))
    }
    
    # obs-x-(ncol(data)+2) data frame
    structure(pred, 
              class = c("prediction", "data.frame"), 
              row.names = seq_len(nrow(pred)),
              at = if (is.null(at)) at else names(at), 
              model.class = class(model),
              type = type)
}
