#' @rdname prediction
#' @param re.form An argument passed forward to \code{\link[lme4]{predict.merMod}}.
#' @export
prediction.merMod <- 
function(model, data = find_data(model), at = NULL, type = c("response", "link"), re.form = NULL, calculate_se = FALSE, ...) {
    
    type <- match.arg(type)
    
    # extract predicted values
    data <- data
    if (missing(data) || is.null(data)) {
        pred <- make_data_frame(fitted = predict(model, type = type, re.form = re.form, ...))
    } else {
        # setup data
        if (is.null(at)) {
            out <- data
        } else {
            out <- build_datalist(data, at = at, as.data.frame = TRUE)
            at_specification <- attr(out, "at_specification")
        }
        # calculate predictions
        tmp <- predict(model, newdata = out, type = type, re.form = re.form, ...)
        pred <- make_data_frame(out, fitted = tmp, se.fitted = rep(NA_real_, nrow(out)))
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
