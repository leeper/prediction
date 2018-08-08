#' @rdname prediction
#' @export
prediction.glimQL <- 
function(model, 
         data = find_data(model, parent.frame()), 
         at = NULL, 
         type = c("response", "link"),
         calculate_se = TRUE,
         ...) {
    
    requireNamespace("aod")
    
    type <- match.arg(type)
    
    # extract predicted values
    data <- data
    if (missing(data) || is.null(data)) {
        if (isTRUE(calculate_se)) {
            pred <- aod::predict(model, type = type, se.fit = TRUE, ...)
            pred <- make_data_frame(fitted = pred[["fit"]], se.fitted = pred[["se.fit"]])
        } else {
            pred <- aod::predict(model, type = type, se.fit = FALSE, ...)
            pred <- make_data_frame(fitted = pred, se.fitted = rep(NA_real_, length(pred)))
        }
    } else {
        # setup data
        data <- build_datalist(data, at = at, as.data.frame = TRUE)
        at_specification <- attr(data, "at_specification")
        # calculate predictions
        if (isTRUE(calculate_se)) {
            tmp <- aod::predict(model, newdata = data, type = type, se.fit = TRUE, ...)
            # cbind back together
            pred <- make_data_frame(data, fitted = tmp[["fit"]], se.fitted = tmp[["se.fit"]])
        } else {
            tmp <- aod::predict(model, newdata = data, type = type, se.fit = FALSE, ...)
            # cbind back together
            pred <- make_data_frame(data, fitted = tmp, se.fitted = rep(NA_real_, nrow(data)))
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
