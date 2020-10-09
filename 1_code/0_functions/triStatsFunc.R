# function to return the stats about chainladder
# Give the function your final triangle
triStatsFunc <- function(finTri) {

# Sum of the diagonal
latest <- finTri %>%  pracma::fliplr() %>% diag() %>% sum()

# Sum of the full years 
ultimate <- finTri[,ncol(finTri)] %>% sum()

# Calculate the IBNR
IBNR <- ultimate - latest

results <- list(paste0("Latest = ", latest), paste0("Ultimate = ", ultimate), paste0("IBNR = ", IBNR))
return(results)

}
