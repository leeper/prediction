#' @rdname prediction
#' @export
prediction.bigLm <- 
function(model, 
         data = NULL,
         calculate_se = FALSE,
         ...) {
    
    # extract predicted values
    data <- data
    if (missing(data) || is.null(data)) {
        pred <- predict(model, ...)
        pred <- make_data_frame(fitted = pred, se.fitted = rep(NA_real_, length(pred)))
    } else {
        # setup data
        #data <- build_datalist(data, at = at, as.data.frame = TRUE)
        #at_specification <- attr(data, "at_specification")
        # calculate predictions
        tmp <- predict(model, newdata = data, ...)
        # cbind back together
        pred <- make_data_frame(data, fitted = tmp, se.fitted = rep(NA_real_, nrow(data)))
    }
    
    # obs-x-(ncol(data)+2) data frame
    structure(pred, 
              class = c("prediction", "data.frame"),
              row.names = seq_len(nrow(pred)),
              at = NULL,
              model.class = class(model),
              type = NULL)
}
