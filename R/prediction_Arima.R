#' @rdname prediction
#' @export
prediction.Arima <- function(model, se.fitted = TRUE,...) {
    
    # extract predicted values
    if (isTRUE(se.fitted)) {
        tmp <- predict(object = model, se.fit = TRUE, ...)
        pred <- make_data_frame(fitted = tmp[[1L]], se.fitted = tmp[[2L]])
    } else {
        tmp <- predict(object = model, se.fit = FALSE, ...)
        pred <- make_data_frame(fitted = tmp, se.fitted = rep(NA_real_, length(tmp)))
    }
    
    
    # obs-x-(ncol(data)+2) data frame
    structure(pred, 
              class = c("prediction", "data.frame"), 
              row.names = seq_len(nrow(pred)),
              at = NULL, 
              model.class = class(model),
              type = NA_character_)
}
