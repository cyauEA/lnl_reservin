kablePretty <- function(dataIn) {
  return(dataIn %>% knitr::kable(format = "html") %>% kableExtra::kable_styling() %>% kableExtra::scroll_box(width = "100%", height = "100%"))
}
