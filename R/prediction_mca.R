#' @rdname prediction
#' @export
prediction.mca <- 
function(model, 
         data = find_data(model), 
         at = NULL, 
         ...) {
    
    # extract predicted values
    # setup data
    out <- build_datalist(data, at = at)
    for (i in seq_along(out)) {
        tmp <- predict(model, 
                       newdata = out[[i]], 
                       ...)
        out[[i]] <- cbind(out[[i]], tmp)
        rm(tmp)
    }
    pred <- do.call("rbind", out)
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
