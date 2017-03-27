#' @rdname prediction
#' @export
prediction.princomp <- function(model, data = find_data(model, parent.frame()), ...) {
    
    # extract predicted values
    if (missing(data)) {
        pred <- data.frame(predict(model, ...))
    } else {
        pred <- data.frame(predict(model, newdata = data, ...))
    }
    pred[["fitted"]] <- NA_real_
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
