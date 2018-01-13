#' @rdname prediction
#' @export
prediction.mca <- 
function(model, 
         data = find_data(model), 
         at = NULL, 
         ...) {
    
    # extract predicted values
    # setup data
    out <- build_datalist(data, at = at, as.data.frame = TRUE)
    # calculate predictions
    tmp <- predict(model, 
                   newdata = out, 
                   ...)
    # cbind back together
    pred <- cbind(out, tmp)
    pred[["fitted"]] <- NA_real_
    pred[["se.fitted"]] <- NA_real_
    
    # obs-x-(ncol(data)+2) data frame
    structure(pred, 
              class = c("prediction", "data.frame"), 
              row.names = seq_len(nrow(pred)),
              at = if (is.null(at)) at else names(at), 
              model.class = class(model),
              type = NA_character_)
}
