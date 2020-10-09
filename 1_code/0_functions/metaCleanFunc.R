metaCleanFunc <- function(datIn, metaInstructions) {
  
  pipeCols <- metaInstructions %>% 
    filter(!is.na(tidyEvalCondition)) %>% 
    select(tidyEvalCondition)
  
  if (nrow(pipeCols) > 0) {
    for (pipe in pipeCols$tidyEvalCondition) eval(parse(text = paste0("datIn <- datIn %>% ", pipe)))
  }
  
  # First get the actual class types and check if it matches before parsing
  origClassess <- lapply(datIn, class) %>% unlist() 
  origClassess <- tibble(varName  = names(origClassess),
                         origClass = origClassess %>% tolower() %>% ifelse(.=="integer",yes = "numeric", .))
  varTypeInstructions <- metaInstructions %>% 
    left_join(origClassess) %>%
    filter(origClass != varType) %>%
    select(-origClass)
  
  # First convert all variables that needs to be converted to character type, since it is easiest to manipulate from there
  datIn <- datIn %>% mutate_at(vars(one_of(varTypeInstructions$varName)), funs(as.character))
  
  # Find the different types of conversions applicable
  types <- unique(varTypeInstructions$varType)
  
  for (tp in types) {
    vars <- varTypeInstructions$varName[varTypeInstructions$varType == tp]
    if (tp == "numeric") {
      datIn <- datIn %>% mutate_at(vars(one_of(vars)), funs(as.numeric))
    } else if (tp == "date") {
      datIn <- datIn %>% mutate_at(vars(one_of(vars)), funs(parsedate::parse_date))
    } 
  }
  
  filterCols <- metaInstructions %>% 
    filter(!is.na(acceptedValues)) %>% 
    select(c(varName, acceptedValues))
  
  # Adding filter for Last vision
  if (nrow(filterCols) > 0) {
    for (ii in 1:nrow(filterCols)) {
      fv <- filterCols$varName[ii]
      fc <- filterCols$acceptedValues[ii]
      datIn <- datIn[datIn[[fv]] %in% fc, ]
    }
  }

  namesIn <- data.frame(varName = names(datIn),
                        inDF    = TRUE,
                        stringsAsFactors = FALSE)
  metaInstructions <- metaInstructions %>% 
    left_join(namesIn) %>% 
    filter(!is.na(inDF))
  
  # Second KeepEnd filter
  datIn <- datIn[, metaInstructions$varName[metaInstructions$keepEnd]]
    
  return(datIn)
}


