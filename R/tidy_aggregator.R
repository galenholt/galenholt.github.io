funbrace <- function(data, groupers, sumcols,
                     FUNS, ...) {
  
  gm <- data %>%
    group_by(across({{groupers}})) %>%
    summarise(across({{sumcols}}, {{FUNS}}, 
                     .names = 'prefix_{.fn}_{.col}')) %>%
    ungroup()
  return(gm)
}

fundefuse <- function(data, groupers, sumcols,
                      FUNS, ...) {
  
  gm <- data %>%
    group_by(across({{groupers}})) %>%
    summarise(across({{sumcols}}, !!FUNS, 
                     .names = 'prefix_{.fn}_{.col}')) %>%
    ungroup()
  return(gm)
}

funchar <- function(data, groupers, sumcols,
                     FUNS, ...) {
  
  FUNS <- eval(parse(text = FUNS))
  
  gm <- data %>%
    group_by(across({{groupers}})) %>%
    summarise(across({{sumcols}}, {{FUNS}}, 
                     .names = 'prefix_{.fn}_{.col}')) %>%
    ungroup()
  return(gm)
}

funbracechar <- function(data, groupers, sumcols,
                     FUNS, ...) {
  if (is.character(FUNS)) {
    FUNS <- mget(FUNS, inherits = TRUE)
  }
  gm <- try(data %>%
    group_by(across({{groupers}})) %>%
    summarise(across({{sumcols}}, {{FUNS}}, 
                     .names = 'prefix_{.fn}_{.col}')) %>%
    ungroup(), silent = TRUE)
  
  if (inherits(gm, 'try-error')) {
    fchar <- paste0(c("rlang::quo(", deparse(FUNS), ")"), collapse = '')
    # FUNS2 <- eval(parse(text = fchar)) # base R
    FUNS3 <- rlang::eval_tidy(rlang::parse_expr(fchar)) # rlang claims to be faster?
    
    gm <- data %>%
      group_by(across({{groupers}})) %>%
      summarise(across({{sumcols}}, {{FUNS3}}, 
                       .names = 'prefix_{.fn}_{.col}')) %>%
      ungroup()
  }
  

  
  return(gm)
}


# Trying to get eval_tidy to work and failing

# with_data <- function(data, expr) {
#   quo <- enquo(expr)
#   eval_tidy(quo, data)
# }
# 
# 
# gm <- data %>%
#   group_by(across({{groupers}})) %>%
#   summarise(across({{sumcols}}, eval_tidy(quo(weighted.mean(.data$mpg, .data$cyl)), data), 
#                    .names = 'prefix_{.fn}_{.col}')) %>%
#   ungroup()

# gm <- data %>%
#   group_by(across({{groupers}})) %>%
#   summarise(across({{sumcols}}, {{FUNS2}}, 
#                    .names = 'prefix_{.fn}_{.col}')) %>%
#   ungroup()
# 
# gm <- data %>%
#   group_by(across({{groupers}})) %>%
#   summarise(across({{sumcols}}, !!FUNS2, 
#                    .names = 'prefix_{.fn}_{.col}')) %>%
#   ungroup()
