#' @importFrom utils head
#' @importFrom stats aggregate
#' @export
summary.prediction <- function(object, digits = 4, ...) {
    f <- object[["fitted"]]
    fc <- object[["fitted.class"]]
    at <- attributes(object)[["at"]]
    if (is.null(at)) {
        objectby <- list(rep(1L, nrow(object)))
    } else {
        objectby <- object[ , setdiff(names(at), "index"), drop = FALSE]
    }
    if (!"fitted.class" %in% names(object) || is.list(fc)) {
        # numeric outcome
        out <- aggregate(object[["fitted"]], objectby, FUN = mean, na.rm = TRUE)
        message(paste0("Average ", ngettext(nrow(out), "prediction", "predictions"), 
                " for ", length(f)/nrow(out), " ", 
                ngettext(nrow(out), "observation", "observations"), ":"))
    } else {
        # factor outcome
        out <- aggregate(object[["fitted.class"]], objectby, FUN = function(set) names(sort(table(set), decreasing = TRUE))[1L])
        message(paste0("Modal ", ngettext(nrow(out), "prediction", "predictions"), 
                " (of ", nlevels(factor(fc)), " ", ngettext(nlevels(factor(fc)), "level", "levels"), 
                ") for ", length(fc), " ", ngettext(length(fc), "observation", "observations"), ": "))
    }
    names(out)[names(out) != "x"] <- paste0("at(", names(out)[names(out) != "x"], ")")
    names(out)[names(out) == "x"] <- "value"
    if (is.null(at)) {
        out <- out[, "value", drop = FALSE]
    }
    print(out, digits = digits, row.names = FALSE, ...)
    invisible(out)
}

#' @export
print.prediction <- function(x, digits = 4, ...) {
    f <- x[["fitted"]]
    fc <- x[["fitted.class"]]
    at <- attributes(x)[["at"]]
    if (is.null(at)) {
        if (!"fitted.class" %in% names(x) || is.list(fc)) {
            # numeric outcome
            m <- sprintf(paste0("%0.", digits, "f"), mean(f, na.rm = TRUE))
            message(paste0("Average prediction for ", length(f), " ", ngettext(length(f), "observation", "observations"), ": ", m))
        } else {
            # factor outcome
            m <- sort(table(x[["fitted.class"]]), decreasing = TRUE)[1L]
            message(paste0("Modal prediction (of ", nlevels(factor(fc)), " ", ngettext(nlevels(f), "level", "levels"), 
                    ") for ", length(fc), " ", ngettext(length(fc), "observation", "observations"), ": ", shQuote(names(m))))
        }
    } else {
        summary(object = x, ...)
    }
    invisible(x)
}
