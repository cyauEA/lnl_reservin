# function to create the cumulative triangle
# Must have claimDate, paidDate, paidAmount

cumtriangleFunc <- function(data) {
  # prep the dates and calculating the development period
  prep <- data %>% 
    dplyr::mutate(claimQY = lubridate::quarter(claimDate, with_year = TRUE),
                  claimQ = lubridate::quarter(claimDate),
                  claimYear = lubridate::year(claimDate),
                  paymentQ = lubridate::quarter(paidDate),
                  yearDiff = lubridate::year(paidDate) - lubridate::year(claimDate),
                  quarterDiff = paymentQ - claimQ,
                  devPeriod = ((yearDiff * 4) + quarterDiff)) %>%
    dplyr::group_by(claimYear, claimQY, devPeriod) %>%
    dplyr::summarise(paidAmount = sum(paidAmount))
  # Create a Mapping table for the claim years
  year <- sort(unique(prep$claimYear), decreasing = FALSE)
  yearmapping <- seq(0,length(unique(prep$claimYear)) - 1, by = 1)
  yearMultiplier <-  cbind.data.frame(year, yearmapping)
  
  prep <- prep %>%
    dplyr::group_by(claimQY, devPeriod) %>%
    dplyr::summarise(paidAmount = sum(paidAmount, na.rm = TRUE))
  
  minDt <- data$claimDate %>% min()
  maxDt <- data$claimDate %>% max()
  
  quarterRef <- tibble(dates = seq(minDt, maxDt, by = "month"),
                       claimQY = lubridate::quarter(dates, with_year = TRUE)) %>%
    select(claimQY) %>%
    distinct() %>%
    mutate(qtr = row_number())
  
  tri <- tibble(claimQY = quarterRef$claimQY %>% rep(nrow(quarterRef))) %>%
    arrange(claimQY) %>%
    mutate(devPeriod = seq(0, nrow(quarterRef) - 1, 1) %>% rep(nrow(quarterRef))) %>%
    left_join(prep) %>%
    mutate(paidAmount = ifelse(is.na(paidAmount), 0, paidAmount)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(claimQY) %>%
    left_join(quarterRef) %>%
    ChainLadder::as.triangle(origin = "claimQY", dev = "devPeriod", value = "paidAmount")
  
  # Flip triangle from left to right to get lower left triangle to be NA
  tri <- pracma::fliplr(tri)
  # Make the lower left triangle NA
  tri[lower.tri(tri)] <- NA
  # Flip triangle back to the right way
  tri <- pracma::fliplr(tri)
  
  #cum tri
  cumTri <- ChainLadder::incr2cum(Triangle = tri)
  return(cumTri)
}