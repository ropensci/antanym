## internal functions, not exported to the user

## wrapper on na.omit that silences warnings when applied to NULL objects
no_na <- function(z) if (is.null(z)) suppressWarnings(na.omit(z)) else na.omit(z)
