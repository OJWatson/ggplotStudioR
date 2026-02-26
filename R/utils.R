`%||%` <- function(lhs, rhs) {
  if (is.null(lhs) || identical(lhs, "")) rhs else lhs
}

studio_null_if_empty <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }

  if (is.character(x) && length(x) == 1L && !nzchar(trimws(x))) {
    return(NULL)
  }

  x
}

studio_num_or_null <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x)) {
    return(NULL)
  }

  as.numeric(x)
}

studio_code <- function(x) {
  paste(deparse(x), collapse = "")
}
