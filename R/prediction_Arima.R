#' @rdname prediction
#' @export
prediction.Arima <- function(model, ...) {
    
    # extract predicted values
    pred <- predict(object = model, se.fit = TRUE, ...)
    
    # obs-x-(ncol(data)+2) data.frame of predictions
    structure(data.frame(fitted = pred[[1L]], se.fitted = pred[[2L]]), 
              class = c("prediction", "data.frame"), 
              row.names = seq_len(nrow(pred)),
              at = NULL, 
              model.class = class(model),
              type = NULL)
}
