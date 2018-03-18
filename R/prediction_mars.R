#' @rdname prediction
#' @export
prediction.mars <- 
function(model,
         data = NULL,
         at = NULL,
         type = "fitted",
         calculate_se = FALSE,
         ...) {
    
    type <- match.arg(type)
    
    # extract predicted values
    data <- data
    if (missing(data) || is.null(data)) {
        pred <- predict(model, type = type, ...)
        pred <- make_data_frame(fitted = pred[,1L], se.fitted = rep(NA_real_, length(pred)))
    } else {
        # setup data
        data <- build_datalist(data, at = at, as.data.frame = TRUE)
        at_specification <- attr(data, "at_specification")
        # calculate predictions
        if (!is.matrix(data)) {
            data <- as.matrix(data)
        }
        tmp <- predict(model, newdata = data, type = type, ...)
        # cbind back together
        pred <- make_data_frame(data, fitted = tmp[,1L], se.fitted = rep(NA_real_, nrow(data)))
    }
    
    # obs-x-(ncol(data)+2) data frame
    structure(pred, 
              class = c("prediction", "data.frame"),
              row.names = seq_len(nrow(pred)),
              at = if (is.null(at)) at else at_specification,
              model.class = class(model),
              type = type)
}
