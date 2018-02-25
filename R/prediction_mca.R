#' @rdname prediction
#' @export
prediction.mca <- 
function(model, 
         data = find_data(model), 
         at = NULL, 
         calculate_se = FALSE,
         ...) {
    
    # extract predicted values
    # setup data
    if (is.null(at)) {
        out <- data
    } else {
        out <- build_datalist(data, at = at, as.data.frame = TRUE)
        at_specification <- attr(out, "at_specification")
    }
    # calculate predictions
    tmp <- predict(model, newdata = out, ...)
    # cbind back together
    pred <- make_data_frame(out, tmp)
    pred[["fitted"]] <- NA_real_
    pred[["se.fitted"]] <- NA_real_
    
    # obs-x-(ncol(data)+2) data frame
    structure(pred, 
              class = c("prediction", "data.frame"), 
              row.names = seq_len(nrow(pred)),
              at = if (is.null(at)) at else at_specification,
              model.class = class(model),
              type = NA_character_)
}
