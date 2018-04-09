is.scalar <- function(x, typestr) {
  (typeof(x) == typestr) && is.atomic(x) && length(x) == 1L
}