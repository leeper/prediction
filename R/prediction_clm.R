#' @rdname prediction
#' @export
prediction.clm <- function(model, data = find_data(model, parent.frame()), ...) {
    # setup data
    data <- data
    
    # extract predicted value at input value
    pred <- predict(model, newdata = data, type = "class", se.fit = FALSE, ...)
    pred[["se.fit"]] <- rep(NA_real_, length(pred[["fit"]]))
    class(pred[["fit"]]) <- c("fit", class(pred[["fit"]]))
    class(pred[["se.fit"]]) <- c("se.fit", "numeric")
    problist <- predict(model, newdata = data, type = "prob", se.fit = TRUE, ...)
    probs <- as.data.frame(problist[["fit"]])
    probs.se <- as.data.frame(problist[["fit"]])
    names(probs) <- paste0("Pr(", seq_len(ncol(probs)), ")")
    names(probs.se) <- paste0("se.Pr(", seq_len(ncol(probs)), ")")
    
    # obs-x-2 data.frame of predictions
    structure(cbind(list(fitted = pred[["fit"]], 
                         se.fitted = pred[["se.fit"]]),
                    probs,
                    probs.se),
              class = c("prediction", "data.frame"), 
              row.names = seq_len(length(pred[["fit"]])),
              model.class = class(model),
              type = NULL)
}

