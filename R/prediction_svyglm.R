#' @rdname prediction
#' @export
prediction.svyglm <- 
function(model, 
         data = find_data(model, parent.frame()), 
         at = NULL, 
         type = c("response", "link"), 
         ...) {
    
    type <- match.arg(type)
    
    # extract predicted values
    data <- data
    if (missing(data) || is.null(data)) {
        pred <- predict(model, type = type, se.fit = TRUE, ...)
        pred <- data.frame(fitted = unclass(pred), 
                           se.fitted = sqrt(unname(attributes(pred)[["var"]])))
    } else {
        # setup data
        out <- build_datalist(data, at = at, as.data.frame = TRUE)
        # calculate predictions
        tmp <- predict(model, 
                       newdata = out, 
                       type = type, 
                       se.fit = TRUE,
                       ...)
        # cbind back together
        pred <- cbind(out, fitted = unclass(tmp), se.fitted = sqrt(unname(attributes(tmp)[["var"]])))
    }
    
    # obs-x-(ncol(data)+2) data frame
    structure(pred, 
              class = c("prediction", "data.frame"), 
              row.names = seq_len(nrow(pred)),
              at = if (is.null(at)) at else names(at), 
              model.class = class(model),
              type = type)
}
