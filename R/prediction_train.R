#' @rdname prediction
#' @export
prediction.train <- 
function(model, 
         data = find_data(model),
         at = NULL, 
         type = c("raw", "prob"),
         ...) {
    
    type <- match.arg(type)
    
    # extract predicted values
    data <- data
    if (missing(data) || is.null(data)) {
        pred <- predict(model, type = type, se.fit = FALSE, ...)
        pred <- make_data_frame(fitted = pred, se.fitted = rep(NA_real_, length(pred)))
    } else {
        # setup data
        data <- build_datalist(data, at = at, as.data.frame = TRUE)
        # calculate predictions
        tmp <- predict(model, newdata = data, type = type, se.fit = FALSE, ...)
        # cbind back together
        pred <- make_data_frame(data, fitted = tmp, se.fitted = rep(NA_real_, nrow(data)))
    }
    
    # obs-x-(ncol(data)+2) data frame
    structure(pred, 
              class = c("prediction", "data.frame"),
              row.names = seq_len(nrow(pred)),
              at = if (is.null(at)) at else names(at), 
              model.class = class(model),
              type = type)
}
