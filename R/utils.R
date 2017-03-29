#' @importFrom utils head
#' @export
head.prediction <- function(x, ...) {
    head(`class<-`(x, "data.frame"), ...)
}

#' @importFrom utils tail
#' @export
tail.prediction <- function(x, ...) {
    tail(`class<-`(x, "data.frame"), ...)
}
