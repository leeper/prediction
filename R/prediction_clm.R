#' @rdname prediction
#' @export
prediction.clm <- 
function(model, 
         data = find_data(model, parent.frame()), 
         at = NULL, 
         ...) {
    
    # extract predicted values
    data <- data
    if (missing(data) || is.null(data)) {
        pred <- data.frame(fitted = predict(model, type = "class", se.fit = FALSE, ...),
                           se.fitted = NA_real_)
        problist <- predict(model, newdata = data, type = "prob", se.fit = TRUE, ...)
        probs <- as.data.frame(problist[["fit"]])
        probs.se <- as.data.frame(problist[["fit"]])
        names(probs) <- paste0("Pr(", seq_len(ncol(probs)), ")")
        names(probs.se) <- paste0("se.Pr(", seq_len(ncol(probs)), ")")
        pred <- cbind(pred, probs, probs.se)
    } else {
        # setup data
        out <- build_datalist(data, at = at)
        for (i in seq_along(out)) {
            tmp <- predict(model, 
                           newdata = out[[i]], 
                           type = "class", 
                           se.fit = FALSE,
                           ...)
            problist <- predict(model, newdata = out[[i]], type = "prob", se.fit = TRUE, ...)
            probs <- as.data.frame(problist[["fit"]])
            probs.se <- as.data.frame(problist[["fit"]])
            names(probs) <- paste0("Pr(", seq_len(ncol(probs)), ")")
            names(probs.se) <- paste0("se.Pr(", seq_len(ncol(probs)), ")")    
            out[[i]] <- cbind(out[[i]], fitted = tmp, se.fitted = rep(NA_real_, length(tmp)), probs, probs.se)
            rm(tmp, probslist, probs, probs.se)
        }
        pred <- do.call("rbind", out)
    }
    
    # obs-x-(ncol(data)+2) data.frame of predictions
    structure(pred,
              class = c("prediction", "data.frame"), 
              row.names = seq_len(length(pred[["fitted"]])),
              at = if (is.null(at)) at else names(at), 
              model.class = class(model),
              type = NULL)
}

