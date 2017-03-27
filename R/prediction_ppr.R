#' @rdname prediction
#' @export
prediction.ppr <- function(model, data = find_data(model, parent.frame()), ...) {
    
    # extract predicted values
    if (missing(data)) {
        pred <- data.frame(fitted = predict(model, ...))
    } else {
        pred <- data.frame(fitted = predict(model, newdata = data, ...))
    }
    pred[["se.fitted"]] <- NA_real_
    
    # obs-x-(ncol(data)+2) data.frame of predictions
    data <- data
    structure(if (!length(data)) {
                  pred
              } else { 
                  cbind(data, pred)
              }, 
              class = c("prediction", "data.frame"), 
              row.names = seq_len(length(pred[["fitted"]])),
              type = NA_character_)
}
