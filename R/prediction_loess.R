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
            at_specification <- attr(out, "at_specification")
        }
        # calculate predictions
        tmp <- predict(model, newdata = out, type = type, se = TRUE, ...)
        # cbind back together
        pred <- make_data_frame(out, fitted = tmp[["fit"]], se.fitted = tmp[["se.fit"]])
    }
    
    # variance(s) of average predictions
    J <- NULL
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
              jacobian = J,
              weighted = FALSE)
}
