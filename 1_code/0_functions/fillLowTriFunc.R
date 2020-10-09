# function to create the age to age factors and create the classic chainladder
# Give the function your cumulative triangle post fixes to nonfull months
fillLowTriFunc <- function(cumTri) {
  
  # Calculate Age to Age factors - Triangle
  ageToAge <- ChainLadder::ata(Triangle = cumTri)
  ageToAge[is.infinite(ageToAge)] <- 0
  
  # Volume weighted averages
  vwtd <- attr(ChainLadder::ata(cumTri), "vwtd")
  
  # For loop to fill in the lower triangle
  for (jj in 2:ncol(cumTri)) {
    cumTri[, jj][is.na(cumTri[, jj])] <- cumTri[, jj - 1][is.na(cumTri[, jj])] * vwtd[jj - 1]
  }
  
  # Sum of the diagonal
  latest <- cumTri %>%  pracma::fliplr() %>% diag() %>% sum()
  
  # Sum of the full years 
  ultimate <- cumTri[,ncol(cumTri)] %>% sum()
  
  # Calculate the IBNR
  IBNR <- ultimate - latest

  return(cumTri)
}
