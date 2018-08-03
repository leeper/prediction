#' @import stats
#' @export
summary.prediction <- function(object, level = 0.95, ...) {
    # summary method
    
    # gather metadata
    f <- object[["fitted"]]
    fc <- object[["fitted.class"]]
    vc <- attributes(object)[["vcov"]]
    if (is.null(vc)) {
        vc <- NA_real_
    }
    
    # convert 'at_specification' into data frame
    at <- attributes(object)[["at"]]
    # aggregate average predictions from data
    if (is.null(at)) {
        objectby <- list(rep(1L, nrow(object)))
        out <- aggregate(object[["fitted"]], objectby, FUN = mean, na.rm = TRUE)
        out[["Group.1"]] <- NULL
    } else {
        objectby <- object[ , setdiff(names(at), "index"), drop = FALSE]
        
        out <- aggregate(object[["fitted"]], objectby, FUN = mean, na.rm = TRUE)
    }
    
    # extract calculated variance from object
    out[["SE"]] <- sqrt(vc)
    
    # cleanup output
    names(out)[names(out) == "x"] <- "Prediction"
    at_names <- names(out)[!names(out) %in% c("Prediction", "SE")]
    at_names <- if (length(at_names)) paste0("at(", at_names, ")") else NULL    
    names(out)[!names(out) %in% c("Prediction", "SE")] <- at_names
    
    # add z and p
    out[["z"]] <- out[,"Prediction"]/out[,"SE"]
    out[["p"]] <- 2 * pnorm(abs(out[,"z"]), lower.tail = FALSE)
    
    # add CI
    a <- (1 - level)/2
    a <- c(a, 1 - a)
    fac <- qnorm(a)
    ci <- array(NA_real_, dim = c(nrow(out), 2L))
    ci[] <- out[["Prediction"]] + out[["SE"]] %o% fac
    colnames(ci) <- c("lower", "upper")
    out <- cbind(out, ci)
    
    # return
    structure(out[, c(at_names, "Prediction", "SE", "z", "p", "lower", "upper"), drop = FALSE],
              class = c("summary.prediction", "data.frame"))
}

#' @export
print.summary.prediction <- function(x, digits = 4, ...) {
    print(`class<-`(x, "data.frame"), digits = digits, row.names = FALSE, ...)
}

#' @rdname prediction
#' @param level A numeric value specifying the confidence level for calculating p-values and confidence intervals.
#' @export
prediction_summary <- function(model, ..., level = 0.95) {
    predictions <- prediction(model, ...)
    summary(predictions, level = 0.95)
}
