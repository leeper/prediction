#' @rdname prediction
#' @export
prediction.lm <- 
function(model, 
         data = find_data(model, parent.frame()), 
         at = NULL, 
         type = "response", 
         calculate_se = TRUE,
         ...) {
    
    # extract predicted values
    data <- data
    if (missing(data) || is.null(data)) {
        if (isTRUE(calculate_se)) {
            pred <- predict(model, type = type, se.fit = TRUE, ...)
            pred <- make_data_frame(fitted = pred[["fit"]], se.fitted = pred[["se.fit"]])
        } else {
            pred <- predict(model, type = type, se.fit = FALSE, ...)
            pred <- make_data_frame(fitted = pred, se.fitted = rep(NA_real_, length(pred)))
        }
    } else {
        # reduce memory profile
        model[["model"]] <- NULL
        attr(model[["terms"]], ".Environment") <- NULL
        
        # setup data
        data <- build_datalist(data, at = at, as.data.frame = TRUE)
        # calculate predictions
        if (isTRUE(calculate_se)) {
            tmp <- predict(model, newdata = data, type = type, se.fit = TRUE, ...)
            # cbind back together
            pred <- make_data_frame(data, fitted = tmp[["fit"]], se.fitted = tmp[["se.fit"]])
        } else {
            tmp <- predict(model, newdata = data, type = type, se.fit = FALSE, ...)
            # cbind back together
            pred <- make_data_frame(data, fitted = tmp, se.fitted = rep(NA_real_, nrow(data)))
        }
    }
    
    # obs-x-(ncol(data)+2) data frame
    structure(pred, 
              class = c("prediction", "data.frame"),
              row.names = seq_len(nrow(pred)),
              at = if (is.null(at)) at else names(at), 
              model.class = class(model),
              type = type)
}
