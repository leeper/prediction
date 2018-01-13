#' @rdname prediction
#' @export
prediction.kqr <- function(model, data, at = NULL, se.fitted = TRUE, ...) {
    
    # extract predicted values
    if (missing(data) || is.null(data)) {
        pred <- data.frame(fitted = predict(object = model, ...)[,1L])
    } else {
        pred <- data.frame(fitted = predict(model, newdata = data,...)[,1L])
    }
    pred[["se.fitted"]] <- NA_real_
    
    # obs-x-(ncol(data)+2) data frame
    structure(pred, 
              class = c("prediction", "data.frame"), 
              row.names = seq_len(nrow(pred)),
              at = if (is.null(at)) at else names(at), 
              model.class = class(model),
              type = NA_character_)
}
