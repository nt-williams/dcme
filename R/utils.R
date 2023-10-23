revert_list <- function(ls) {
    # @Josh O'Brien StackOverflow
    x <- lapply(ls, `[`, names(ls[[1]]))
    apply(do.call(rbind, x), 2, as.list)
}

reorder_cv <- function(x, folds) {
    i <- Reduce(c, lapply(folds, function(x) x$validation_set))
    Reduce(c, x)[order(i)]
}

gl <- glue::glue
