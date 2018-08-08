#' @rdname prediction
#' @export
prediction.bruto <- 
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
    
    # variance(s) of average predictions
    vc <- NA_real_
    
    # output
    structure(pred, 
              class = c("prediction", "data.frame"),
              at = if (is.null(at)) at else at_specification,
              type = type,
              call = if ("call" %in% names(model)) model[["call"]] else NULL,
              model_class = class(model),
              row.names = seq_len(nrow(pred)),
              vcov = vc,
              jacobian = NULL,
              weighted = FALSE)
}
