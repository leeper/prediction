#' @rdname prediction
#' @export
prediction.ar <- function(model, data, at = NULL, calculate_se = TRUE,...) {
    
    # extract predicted values
    if (missing(data) || is.null(data)) {
        if (isTRUE(calculate_se)) {
            tmp <- predict(object = model, se.fit = TRUE, ...)
            pred <- make_data_frame(fitted = tmp[[1L]], se.fitted = tmp[[2L]])
        } else {
            tmp <- predict(object = model, se.fit = FALSE, ...)
            pred <- make_data_frame(fitted = tmp, se.fitted = rep(NA_real_, length(tmp)))
        }
    } else {
        # setup data
        if (is.null(at)) {
            data <- data
        } else {
            data <- build_datalist(data, at = at, as.data.frame = TRUE)
            at_specification <- attr(data, "at_specification")
        }
        if (isTRUE(calculate_se)) {
            tmp <- predict(model, newdata = data, se.fit = TRUE, ...)
            pred <- make_data_frame(fitted = tmp[[1L]], se.fitted = tmp[[2L]])
        } else {
            tmp <- predict(model, newdata = data, se.fit = FALSE, ...)
            pred <- make_data_frame(fitted = tmp, se.fitted = rep(NA_real_, length(tmp)))
        }
    }
    
    # variance(s) of average predictions
    vc <- NA_real_
    
    # output
    structure(pred, 
              class = c("prediction", "data.frame"),
              at = if (is.null(at)) at else at_specification,
              type = NA_character_,
              call = if ("call" %in% names(model)) model[["call"]] else NULL,
              model_class = class(model),
              row.names = seq_len(nrow(pred)),
              vcov = vc,
              jacobian = NULL,
              weighted = FALSE)
}
