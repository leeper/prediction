#' @rdname prediction
#' @export
prediction.svm <- function(model, data = find_data(model, parent.frame()), ...) {
    # setup data
    data <- data
    
    # extract predicted value
    if (any(grepl("prob.+", names(model)))) {
        pred <- data.frame(fitted = predict(model, newdata = data, probability = TRUE, ...))
        pred[["se.fit"]] <- NA_real_
        pred <- cbind(pred, attributes(pred[["fitted"]])[["probabilities"]])
        attr(pred[["fitted"]], "probabilities") <- NULL
        names(pred) <- c("fitted", "se.fitted", paste0("Pr(", names(pred)[-c(1L:2L)], ")"))
    } else {
        pred <- data.frame(fitted = predict(model, newdata = data, probability = FALSE, ...))
        pred[["se.fitted"]] <- NA_real_
    }
    class(pred[["fitted"]]) <- c("fit", class(pred[["fit"]]))
    class(pred[["se.fit"]]) <- c("se.fit", "numeric")
    
    
    # obs-x-(ncol(data)+2+nlevels(outcome)) data.frame of predictions
    structure(cbind(data, pred),
              class = c("prediction", "data.frame"), 
              row.names = seq_len(length(pred[["fit"]])),
              model.class = class(model),
              type = NULL)
}

