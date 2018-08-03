#' @importFrom utils head
#' @importFrom stats aggregate
#' @export
summary.prediction <- function(object, digits = 4, ...) {
    # summary method
    # function also called by `print.prediction()` if object has an 'at' specification
    
    # gather metadata
    f <- object[["fitted"]]
    fc <- object[["fitted.class"]]
    vc <- attributes(object)[["vcov"]]
    
    # convert 'at_specification' into data frame
    at <- attributes(object)[["at"]]
    if (is.null(at)) {
        objectby <- list(rep(1L, nrow(object)))
    } else {
        objectby <- object[ , setdiff(names(at), "index"), drop = FALSE]
    }
    
    # calculate average/modal predictions
    if (!"fitted.class" %in% names(object) || is.list(fc)) {
        # numeric outcome
        ## aggregate average predictions from data
        out <- aggregate(object[["fitted"]], objectby, FUN = mean, na.rm = TRUE)
        ## extract calculated variance from object
        out$SE <- sqrt(vc)
        
        # message
        message(paste0("Data frame with average ", ngettext(nrow(out), "prediction", "predictions"), 
                " for ", length(f)/nrow(out), " ", 
                ngettext(nrow(out), "observation", "observations"), ":"))
    } else {
        # factor outcome
        out <- aggregate(object[["fitted.class"]], objectby, FUN = function(set) names(sort(table(set), decreasing = TRUE))[1L])
        
        # message
        message(paste0("Data frame with modal ", ngettext(nrow(out), "prediction", "predictions"), 
                " (of ", nlevels(factor(fc)), " ", ngettext(nlevels(factor(fc)), "level", "levels"), 
                ") for ", length(fc), " ", ngettext(length(fc), "observation", "observations"), ": "))
    }
    
    # cleanup output
    names(out)[!names(out) %in% c("x", "SE")] <- paste0("at(", names(out)[!names(out) %in% c("x", "SE")], ")")
    names(out)[names(out) == "x"] <- "Prediction"
    if (is.null(at)) {
        out <- out[, c("Prediction", "SE"), drop = FALSE]
    }
    
    # print and return
    print(out, digits = digits, row.names = FALSE, ...)
    invisible(out)
}

#' @export
print.prediction <- function(x, digits = 4, ...) {
    
    # gather metadata
    f <- x[["fitted"]]
    fc <- x[["fitted.class"]]
    at <- attributes(x)[["at"]]
    vc <- attributes(x)[["vcov"]]
    
    # calculate overall predictions
    ## if no 'at_specification', simply calculate overall average/mode and print
    if (is.null(at)) {
        # object is a single replication with no 'at' specification
        if (!"fitted.class" %in% names(x) || is.list(fc)) {
            # numeric outcome
            m <- sprintf(paste0("%0.", digits, "f"), mean(f, na.rm = TRUE))
            message(paste0("Data frame with average prediction for ", length(f), " ", ngettext(length(f), "observation", "observations"),
                           ": ", m, " (se = ", sprintf(paste0("%0.", digits, "f"), sqrt(vc)), ")"))
        } else {
            # factor outcome
            m <- sort(table(x[["fitted.class"]]), decreasing = TRUE)[1L]
            message(paste0("Data frame with modal prediction (of ", nlevels(factor(fc)), " ", ngettext(nlevels(f), "level", "levels"), 
                    ") for ", length(fc), " ", ngettext(length(fc), "observation", "observations"), ": ", shQuote(names(m))))
        }
    } else {
        # otherwise, object has an 'at' specification, reflecting multiple requested predictions
        summary(object = x, ...)
    }
    
    # return invisibly
    invisible(x)
}
