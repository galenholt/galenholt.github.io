selectnames <- function(data, selector, failmissing = TRUE) {
  
  if (is.character(selector)) {
    if (failmissing) {
      whichg <- expr(all_of(selector))
    } else {
      whichg <- expr(any_of(selector))
    }
    
  } else {
    whichg <- selector
  }
  
  selnames <- whichg %>% 
    tidyselect::eval_select(data, strict = failmissing) %>% 
    names()
  
  return(selnames)
}

gtidysimple <- function(data, groupers, sumcols) {
  
  gnames <- selectnames(data, groupers)
  snames <- selectnames(data, sumcols)
  
  gm <- data %>%
    group_by(across({{gnames}})) %>%
    summarise(across({{snames}}, mean, .names = 'mean_{.col}')) %>%
    ungroup()
  return(gm)
  
}

gtidyquo <- function(data, groupers, sumcols, failmissing = TRUE) {
  
  gnames <- selectnames(data, enquo(groupers), failmissing)
  snames <- selectnames(data, enquo(sumcols), failmissing)
  
  gm <- data %>%
    group_by(across({{gnames}})) %>%
    summarise(across({{snames}}, mean, .names = 'mean_{.col}')) %>%
    ungroup()
  return(gm)
  
}