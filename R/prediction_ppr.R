#' @rdname prediction
#' @export
prediction.ppr <- function(model, data = find_data(model, parent.frame()), at = NULL, ...) {
    
    # extract predicted values
    data <- data
    if (missing(data) || is.null(data)) {
        pred <- data.frame(fitted = predict(model, ...),
                           se.fitted = NA_real_)
    } else {
        # setup data
        out <- build_datalist(data, at = at, as.data.frame = TRUE)
        # calculate predictions
        tmp <- predict(model, 
                       newdata = out, 
                       ...)
        # cbind back together
        pred <- cbind(out, fitted = tmp, se.fitted = rep(NA_real_, nrow(out)))
    }
    
    # obs-x-(ncol(data)+2) data frame
    structure(pred, 
              class = c("prediction", "data.frame"), 
              row.names = seq_len(nrow(pred)),
              at = if (is.null(at)) at else names(at), 
              model.class = class(model),
              type = NA_character_)
}
