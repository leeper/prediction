#' @rdname prediction
#' @export
prediction.svm <- function(model, data = find_data(model, parent.frame()), at = NULL, ...) {

    # extract predicted value
    if (any(grepl("prob.+", names(model)))) {
        if (missing(data)) {
            pred <- data.frame(fitted = predict(model, probability = TRUE, ...),
                               se.fitted = NA_real_)
        } else {
            pred <- data.frame(fitted = predict(model, newdata = data, probability = TRUE, ...),
                               se.fitted = NA_real_)
        }
        pred <- cbind(pred, attributes(pred[["fitted"]])[["probabilities"]])
        attr(pred[["fitted"]], "probabilities") <- NULL
        names(pred) <- c("fitted", "se.fitted", paste0("Pr(", names(pred)[-c(1L:2L)], ")"))
    } else {
        if (missing(data)) {
            pred <- data.frame(fitted = predict(model, probability = FALSE, ...),
                               se.fitted = NA_real_)
        } else {
            pred <- data.frame(fitted = predict(model, newdata = data, probability = FALSE, ...),
                               se.fitted = NA_real_)
        }
    }
    
    # obs-x-(ncol(data)+2+nlevels(outcome)) data.frame of predictions
    data <- data
    structure(if (!length(data)) pred else cbind(data, pred),
              class = c("prediction", "data.frame"), 
              row.names = seq_len(length(pred[["fitted"]])),
              model.class = class(model),
              type = NULL)
}
