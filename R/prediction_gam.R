#' @rdname prediction
#' @export
prediction.Gam <- 
function(model, 
         data = find_data(model, parent.frame()), 
         at = NULL, 
         type = c("response", "link", "terms"), 
         calculate_se = TRUE,
         ...) {
    
    type <- match.arg(type)
    
    # extract predicted value
    data <- data
    if (missing(data) || is.null(data)) {
        if (isTRUE(calculate_se)) {
            pred <- predict(model, type = type, se.fit = TRUE, ...)
            pred <- make_data_frame(fitted = pred[["fit"]], se.fitted = pred[["se.fit"]][,1L])
        } else {
            pred <- predict(model, type = type, se.fit = FALSE, ...)
            pred <- make_data_frame(fitted = pred, se.fitted = rep(NA_real_, length(pred)))
        }
    } else {
        # setup data
        if (is.null(at)) {
            out <- data
        } else {
            out <- build_datalist(data, at = at, as.data.frame = TRUE)
            at_specification <- attr(out, "at_specification")
        }
        # calculate predictions
        if (isTRUE(calculate_se)) {
            pred <- predict(model, newdata = out, type = type, se.fit = FALSE, ...)
            pred <- make_data_frame(out, fitted = pred, se.fitted = rep(NA_real_, length(pred)))
        } else {
            pred <- predict(model, newdata = out, type = type, se.fit = FALSE, ...)
            pred <- make_data_frame(out, fitted = pred, se.fitted = rep(NA_real_, length(pred)))
        }
    }
    
    # obs-x-(ncol(data)+2) data frame
    structure(pred, 
              class = c("prediction", "data.frame"), 
              row.names = seq_len(nrow(pred)),
              at = if (is.null(at)) at else at_specification,
              model.class = class(model),
              type = type)
}
