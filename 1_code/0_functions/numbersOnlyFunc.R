# Check that a string doesn't match any non-number
numbersOnlyFunc <- function(x) !grepl("\\D", x)