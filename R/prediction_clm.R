#' @rdname prediction
#' @export
prediction.clm <- function(model, data = find_data(model, parent.frame()), at = NULL, ...) {
    
    # extract predicted values
    if (missing(data)) {
        pred <- data.frame(fitted = predict(model, type = "class", se.fit = FALSE, ...),
                           se.fitted = NA_real_)
    } else {
        pred <- data.frame(fitted = predict(model, newdata = data, type = "class", se.fit = FALSE, ...),
                           se.fitted = NA_real_)
    }
    
    problist <- predict(model, newdata = data, type = "prob", se.fit = TRUE, ...)
    probs <- as.data.frame(problist[["fit"]])
    probs.se <- as.data.frame(problist[["fit"]])
    names(probs) <- paste0("Pr(", seq_len(ncol(probs)), ")")
    names(probs.se) <- paste0("se.Pr(", seq_len(ncol(probs)), ")")    
    
    # obs-x-(ncol(data)+2) data.frame of predictions
    data <- data
    structure(if (!length(data)) cbind(pred, probs, probs.se) else cbind(data, pred, probs, probs.se),
              class = c("prediction", "data.frame"), 
              row.names = seq_len(length(pred[["fitted"]])),
              model.class = class(model),
              type = NULL)
}

