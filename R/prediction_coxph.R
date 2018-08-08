#' @rdname prediction
#' @export
prediction.coxph <- 
function(model, 
         data = find_data(model, parent.frame()), 
         at = NULL, 
         type = c("risk", "expected", "lp"), 
         calculate_se = TRUE,
         ...) {
    
    type <- match.arg(type)
    
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
        # setup data
        if (is.null(at)) {
            out <- data
        } else {
            out <- build_datalist(data, at = at, as.data.frame = TRUE)
            at_specification <- attr(out, "at_specification")
        }
        # calculate predictions
        if (isTRUE(calculate_se)) {
            pred <- predict(model, newdata = out, type = type, se.fit = TRUE, ...)
            pred <- make_data_frame(out, fitted = pred[["fit"]], se.fitted = pred[["se.fit"]])
        } else {
            pred <- predict(model, newdata = out, type = type, se.fit = FALSE, ...)
            pred <- make_data_frame(out, fitted = pred, se.fitted = rep(NA_real_, length(pred)))
        }
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
