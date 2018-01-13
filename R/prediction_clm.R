#' @rdname prediction
#' @export
prediction.clm <- 
function(model, 
         data = find_data(model, parent.frame()), 
         at = NULL, 
         type = NULL, 
         category, 
         ...) {
    
    if (!is.null(type)) {
        warning(sprintf("'type' is ignored for models of class '%s'", class(model)))
    }
    
    # extract predicted values
    data <- data
    if (missing(data) || is.null(data)) {
        pred <- data.frame(fitted.class = predict(model, type = "class", se.fit = FALSE, ...)[["fit"]])
        problist <- predict(model, newdata = data, type = "prob", se.fit = TRUE, ...)
        probs <- as.data.frame(problist[["fit"]])
        probs.se <- as.data.frame(problist[["se.fit"]])
        names(probs) <- paste0("Pr(", seq_len(ncol(probs)), ")")
        names(probs.se) <- paste0("se.Pr(", seq_len(ncol(probs)), ")")
        pred <- cbind(pred, probs, probs.se)
    } else {
        # setup data
        out <- build_datalist(data, at = at, as.data.frame = TRUE)
        # calculate predictions
        tmp <- predict(model, 
                       newdata = out, 
                       type = "class", 
                       se.fit = FALSE,
                       ...)[["fit"]]
        problist <- predict(model, newdata = out, type = "prob", se.fit = TRUE, ...)
        probs <- as.data.frame(problist[["fit"]])
        probs.se <- as.data.frame(problist[["se.fit"]])
        names(probs) <- paste0("Pr(", seq_len(ncol(probs)), ")")
        names(probs.se) <- paste0("se.Pr(", seq_len(ncol(probs)), ")")    
        # cbind back together
        pred <- cbind(out, fitted.class = tmp, probs, probs.se)
        rm(tmp, problist, probs, probs.se)
    }
    
    # handle category argument
    if (missing(category)) {
        w <- grep("^Pr\\(", names(pred))[1L]
        category <- names(pred)[w]
        pred[["fitted"]] <- pred[[w]]
    } else {
        w <- which(names(pred) == paste0("Pr(", category, ")"))
        if (!length(w)) {
            stop(sprintf("category %s not found", category))
        }
        pred[["fitted"]] <- pred[[ w[1L] ]]
    }
    pred[["se.fitted"]] <- NA_real_
    
    # obs-x-(ncol(data)+2) data.frame of predictions
    structure(pred,
              class = c("prediction", "data.frame"), 
              row.names = seq_len(nrow(pred)),
              at = if (is.null(at)) at else names(at), 
              model.class = class(model),
              type = NA_character_,
              category = category)
}

