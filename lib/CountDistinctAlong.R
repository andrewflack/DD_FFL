CountDistinctAlong <- function(var) sapply(seq_along(var), function(x) length(unique(head(var, x))))
