#' @rdname prediction
#' @export
prediction.Arima <- function(model, calculate_se = TRUE,...) {
    
    # extract predicted values
    if (isTRUE(calculate_se)) {
        tmp <- predict(object = model, se.fit = TRUE, ...)
        pred <- make_data_frame(fitted = tmp[[1L]], se.fitted = tmp[[2L]])
    } else {
        tmp <- predict(object = model, se.fit = FALSE, ...)
        pred <- make_data_frame(fitted = tmp, se.fitted = rep(NA_real_, length(tmp)))
    }
    
    # variance(s) of average predictions
    vc <- NA_real_
    
    # output
    structure(pred, 
              class = c("prediction", "data.frame"),
              at = NULL,
              type = NA_character_,
              call = if ("call" %in% names(model)) model[["call"]] else NULL,
              model_class = class(model),
              row.names = seq_len(nrow(pred)),
              vcov = vc,
              jacobian = NULL,
              weighted = FALSE)
}
