#' @rdname prediction
#' @export
prediction.hurdle <- 
function(model, 
         data = find_data(model, parent.frame()), 
         at = NULL, 
         type = c("response", "count", "prob", "zero"), 
         calculate_se = FALSE,
         ...) {
    
    type <- match.arg(type)
    
    # extract predicted values
    data <- data
    if (missing(data) || is.null(data)) {
        pred <- predict(model, type = type, ...)
        pred <- make_data_frame(fitted = pred[["fit"]])
    } else {
        # setup data
        if (is.null(at)) {
            out <- data
        } else {
            out <- build_datalist(data, at = at, as.data.frame = TRUE)
            at_specification <- attr(out, "at_specification")
        }
        # calculate predictions
        pred <- predict(model, newdata = out, type = type, ...)
        pred <- make_data_frame(out, fitted = pred, se.fitted = rep(NA_real_, nrow(out)))
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
