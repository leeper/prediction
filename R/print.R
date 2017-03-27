#' @importFrom utils head
#' @export
print.prediction <- function(x, digits = 4, ...) {
    f <- x[["fitted"]]
    if (is.null(attributes(x)[["at"]])) {
        if (is.numeric(f)) {
            m <- sprintf(paste0("%0.", digits, "f"), mean(f, na.rm = TRUE))
            message(paste0("Average prediction: ", m, ", for ", length(f), " ", ngettext(length(f), "observation", "observations")))
        } else {
            m <- sort(table(x[["fitted"]]), decreasing = TRUE)[1L]
            cat(paste0("Modal prediction: ", shQuote(names(m)), " for ", m, " of ", length(f), " ", 
                ngettext(length(f), "observation", "observations"),
                " with total ", nlevels(f), " ", ngettext(nlevels(f), "level", "levels") ))
        }
    } else {
        xby <- x[ , attributes(x)[["at"]], drop = FALSE]
        if (is.numeric(f)) {
            out <- aggregate(x[["fitted"]], xby, FUN = mean, na.rm = TRUE)
            message(paste0("Average ", ngettext(nrow(out), "prediction", "predictions"), 
                    " for ", length(f)/nrow(out), " ", 
                    ngettext(nrow(out), "observation", "observations"), ":"))
        } else {
            out <- aggregate(x[["fitted"]], xby, FUN = function(set) sort(table(set), decreasing = TRUE)[1L])
            message(paste0("Modal ", ngettext(nrow(out), "prediction", "predictions"), 
                    " for ", m, " of ", length(f), " ", 
                    ngettext(length(f), "observation", "observations"),
                    " with total ", nlevels(f), " ", ngettext(nlevels(f), "level", "levels"), ":"))
        }
        names(out)[names(out) != "x"] <- paste0("at(", names(out)[names(out) != "x"], ")")
        names(out)[names(out) == "x"] <- "value"
        print(out, digits = digits, row.names = FALSE, ...)
    }
    invisible(x)
}

