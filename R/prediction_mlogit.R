# @rdname prediction
# @export
prediction.mlogit <- 
function(model, 
         data = find_data(model, parent.frame()), 
         at = NULL, 
         category, 
         ...) {
    
    # extract predicted values
    data <- data
    if (missing(data) || is.null(data)) {
        warning(sprintf("'data' is ignored for models of class '%s'", class(model)))
    }
    # setup data
    out <- build_datalist(data, at = at)
    for (i in seq_along(out)) {
        tmp <- data.frame(predict(model, newdata = out[[i]], ...))
        names(tmp) <- paste0("Pr(", seq_len(ncol(tmp)), ")")
        out[[i]] <- cbind(out[[i]], tmp)
        rm(tmp)
    }
    pred <- do.call("rbind", out)

    # handle category argument
    if (missing(category)) {
        pred[["fitted"]] <- pred[[grep("^Pr\\(", names(pred))[1L]]]
    } else {
        w <- which(names(pred) == paste0("Pr(", category, ")"))
        if (!length(w)) {
            stop(sprintf("category %s not found", category))
        }
        pred[["fitted"]] <- pred[[ w[1L] ]]
    }
    pred[["se.fitted"]] <- NA_real_
    
    # obs-x-(ncol(data)+2+nlevels(outcome)) data.frame of predictions
    structure(pred,
              class = c("prediction", "data.frame"), 
              row.names = seq_len(nrow(pred)),
              at = if (is.null(at)) at else names(at), 
              model.class = class(model),
              type = NA_character_,
              category = category)
}
