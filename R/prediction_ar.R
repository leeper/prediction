#' @rdname prediction
#' @export
prediction.ar <- function(model, data, at = NULL, ...) {
    
    # extract predicted values
    if (missing(data) || is.null(data)) {
        tmp <- predict(object = model, se.fit = TRUE, ...)
        
    } else {
        tmp <- predict(model, newdata = data, se.fit = TRUE, ...)
    }
    pred <- data.frame(fitted = tmp[[1L]], se.fitted = tmp[[2L]])
    
    # obs-x-(ncol(data)+2) data frame
    structure(pred, 
              class = c("prediction", "data.frame"), 
              row.names = seq_len(nrow(pred)),
              at = if (is.null(at)) at else names(at), 
              model.class = class(model),
              type = NA_character_)
}
