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
    
    # variance(s) of average predictions
    vc <- NA_real_
    
    # output
    structure(pred, 
              class = c("prediction", "data.frame"),
              at = NULL,
              type = "response",
              call = if ("call" %in% names(model)) model[["call"]] else NULL,
              model_class = class(model),
              row.names = seq_len(nrow(pred)),
              vcov = vc,
              jacobian = NULL,
              weighted = FALSE)
}
