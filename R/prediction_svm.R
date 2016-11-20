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
        names(pred) <- c("fitted", "se.fit", paste0("Pr(", names(pred)[-c(1L:2L)], ")"))
    } else {
        pred <- data.frame(fitted = predict(model, newdata = data, probability = FALSE, ...))
        pred[["se.fit"]] <- NA_real_
    }
    class(pred[["fitted"]]) <- c("fit", class(pred[["fit"]]))
    class(pred[["se.fit"]]) <- c("se.fit", "numeric")
    
    # obs-x-(2+nlevels) data.frame of predictions
    structure(pred,
              class = c("prediction", "data.frame"), 
              row.names = seq_len(length(pred[["fit"]])),
              model.class = class(model),
              type = NULL)
}

