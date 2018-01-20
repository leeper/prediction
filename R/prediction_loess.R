#' @rdname prediction
#' @export
prediction.loess <- function(model, data = find_data(model, parent.frame()), at = NULL, type = "response", calculate_se = TRUE, ...) {
    
    type <- match.arg(type)
    
    # extract predicted values
    data <- data
    if (missing(data) || is.null(data)) {
        pred <- predict(model, type = type, se = TRUE, ...)
        pred <- make_data_frame(fitted = pred[["fit"]], se.fitted = pred[["se.fit"]])
    } else {
        # setup data
        if (is.null(at)) {
            out <- data
        } else {
            out <- build_datalist(data, at = at, as.data.frame = TRUE)
        }
        # calculate predictions
        tmp <- predict(model, newdata = out, type = type, se = TRUE, ...)
        # cbind back together
        pred <- make_data_frame(out, fitted = tmp[["fit"]], se.fitted = tmp[["se.fit"]])
    }
    
    # obs-x-(ncol(data)+2) data frame
    structure(pred, 
              class = c("prediction", "data.frame"), 
              row.names = seq_len(nrow(pred)),
              at = if (is.null(at)) at else names(at), 
              model.class = class(model),
              type = type)
}

