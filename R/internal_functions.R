## internal functions, not exported to the user

## wrapper on na.omit that silences warnings when applied to NULL objects
no_na <- function(z) suppressWarnings(na.omit(x))
