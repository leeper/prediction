# internal function that overrides the defaults of `data.frame()`
make_data_frame <- function(...) {
    data.frame(..., check.rows = FALSE, check.names = FALSE, fix.empty.names = FALSE, stringsAsFactors = FALSE)
}
