#' @rdname prediction
#' @export
prediction.gls <- 
function(model, 
         data = find_data(model), 
         at = NULL, 
         se.fitted = TRUE,
         ...) {
    
    # extract predicted values
    data <- data
    if (missing(data) || is.null(data)) {
        pred <- data.frame(fitted = predict(model, type = "class", ...),
                           se.fitted = NA_real_)
    } else {
        # setup data
        if (is.null(at)) {
            out <- data
        } else {
            out <- build_datalist(data, at = at, as.data.frame = TRUE)
        }
        # calculate predictions
        tmp <- predict(model, 
                       newdata = out, 
                       type = "class", 
                       ...)
        # cbind back together
        pred <- cbind(out, fitted = tmp, se.fitted = rep(NA_real_, length(tmp)))
    }
    
    # obs-x-(ncol(data)+2) data frame
    structure(pred,
              class = c("prediction", "data.frame"), 
              row.names = seq_len(nrow(pred)),
              at = if (is.null(at)) at else names(at), 
              model.class = class(model),
              type = NA_character_)
}

