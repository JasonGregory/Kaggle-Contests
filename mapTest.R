i_data <- readr::read_rds(normalizePath("data/zillow/propertyData.rds"))
library(dplyr)

f1 <- function(data) {
  f = list(
    total_count = function(x) length(x),
    class = class,
    null_count = function(x) sum(is.na(x)),
    distinct = function(x) length(unique(x))
  )
  
  smry <- sapply(f, function(fn) {data %>% purrr::map(~fn(.))}) %>% 
    as.data.frame %>% 
    tibble::rownames_to_column("variable") %>% 
    tidyr::unnest()
  
  return(smry)
}

f2 <- function(data) {
  f = list(
    total_count = function(x) length(x),
    class = class,
    null_count = function(x) sum(is.na(x)),
    distinct = function(x) length(unique(x))
  )
  
  smry <- as.data.frame(sapply(f, function(fn) {data %>% summarise_all(fn)})) %>% 
    tibble::rownames_to_column(var = "variable") %>% 
    tidyr::unnest()
  
  return(smry)
}

f3 <- function(data) {
  df <- tibble::tibble(
    variable = names(data),
   # values = map(data, ~tibble(value = .x)),
    class = map_chr(data, ~class(.x)),
    total_count = map_dbl(data, ~length(.x)),
    null_count = map_dbl(data, ~sum(is.na(.x))),
    dist_count = map_dbl(data, ~length(unique(.x)))
  )
  return(df)
}

#f3 seems to be consistently faster as well is more simple in coding.
system.time(
  temp <- f3(i_data)
  #  f3(mtcars)
)

system.time(
  f1(i_data)
#  f1(mtcars)  
)

system.time(
  f2(i_data)
#  f2(mtcars)  
)

temp %>% print(n = Inf)

# work on new function ---------------------------------------------------------
  # Skew and Kurtosis add about 10 sections. Possibly do an extreme advanced to include the longer functions that are not used a whole lot.
f4 <- function(data, type = "basic") {
  if (type == "basic") {
    describe <- tibble::tibble(
      variable = names(data),
      class = purrr::map_chr(data, ~class(.x)),
      null_count = purrr::map_dbl(data, ~sum(is.na(.x))),
      distinct_count = purrr::map_dbl(data, ~data.table::uniqueN(.x)),
      total_count = purrr::map_dbl(data, ~length(.x))
    )    
    describe <- mutate(describe, 
                       null_perc = round(null_count/total_count, 6),
                       distinct_perc = round(distinct_count/total_count, 6)
    ) %>%
      select(variable, class, null_perc, distinct_perc, null_count, distinct_count, total_count)
  } else {
    
    numeric <- select_if(data, is.numeric)
    
    describe1 <- tibble::tibble(
      variable = names(data),
      class = purrr::map_chr(data, ~class(.x)),
      null_count = purrr::map_dbl(data, ~sum(is.na(.x))),
      distinct_count = purrr::map_dbl(data, ~data.table::uniqueN(.x)),
      total_count = purrr::map_dbl(data, ~length(.x))
    )   
    
    describe2 <- tibble::tibble(
      variable = names(numeric),
      max = purrr::map_dbl(numeric, ~max(.x, na.rm = TRUE)),
      min = purrr::map_dbl(numeric, ~min(.x, na.rm = TRUE)),
      standard_deviation = purrr::map_dbl(numeric, ~sd(.x, na.rm = TRUE)),      
      mean = purrr::map_dbl(numeric, ~mean(.x, na.rm = TRUE)),
      median = purrr::map_dbl(numeric, ~median(.x, na.rm = TRUE))#,
#      kurtosis = purrr::map_dbl(numeric, ~moments::kurtosis(.x, na.rm = TRUE)),
#      skew = purrr::map_dbl(numeric, ~moments::skewness(.x, na.rm = TRUE))       
    )
    
    describe <- describe1 %>%
      left_join(describe2, by = "variable") %>%
      mutate(null_perc = round(null_count/total_count, 6),
             distinct_perc = round(distinct_count/total_count, 6)) %>%
    #  select(variable, class, null_perc, distinct_perc, max, min, mean, median, skew, standard_deviation, kurtosis, null_count, distinct_count, total_count)
      select(variable, class, null_perc, distinct_perc, max, min, mean, median, standard_deviation, null_count, distinct_count, total_count)  
  }
  return(describe)
}

# Adds in table logic for the advanced pull. Need to figure out how to add a name attribute for each list. Also possibly add a third column for rank.
f5 <- function(data, type = "basic") {
  if (type == "basic") {
    describe <- tibble::tibble(
      variable = names(data),
      class = purrr::map_chr(data, ~class(.x)),
      null_count = purrr::map_dbl(data, ~sum(is.na(.x))),
      distinct_count = purrr::map_dbl(data, ~data.table::uniqueN(.x)),
      total_count = purrr::map_dbl(data, ~length(.x))
    )    
    describe <- mutate(describe, 
                       null_perc = round(null_count/total_count, 6),
                       distinct_perc = round(distinct_count/total_count, 6)
    ) %>%
      select(variable, class, null_perc, distinct_perc, null_count, distinct_count, total_count)
  } else {
    
    numeric <- select_if(data, is.numeric)
    
    describe1 <- tibble::tibble(
      variable = names(data),
      class = purrr::map_chr(data, ~class(.x)),
      null_count = purrr::map_dbl(data, ~sum(is.na(.x))),
      distinct_count = purrr::map_dbl(data, ~data.table::uniqueN(.x)),
      total_count = purrr::map_dbl(data, ~length(.x)),
      table = purrr::map(data, ~data.table::data.table(.x)[, .N, keyby = .x] %>% as_tibble %>% rename(count = N, value = .x) %>% arrange(desc(count)))
    ) 
    
    describe2 <- tibble::tibble(
      variable = names(numeric),
      max = purrr::map_dbl(numeric, ~max(.x, na.rm = TRUE)),
      min = purrr::map_dbl(numeric, ~min(.x, na.rm = TRUE)),
      standard_deviation = purrr::map_dbl(numeric, ~sd(.x, na.rm = TRUE)),      
      mean = purrr::map_dbl(numeric, ~mean(.x, na.rm = TRUE)),
      median = purrr::map_dbl(numeric, ~median(.x, na.rm = TRUE))#,
      #      kurtosis = purrr::map_dbl(numeric, ~moments::kurtosis(.x, na.rm = TRUE)),
      #      skew = purrr::map_dbl(numeric, ~moments::skewness(.x, na.rm = TRUE))       
    )
    
    describe <- describe1 %>%
      left_join(describe2, by = "variable") %>%
      mutate(null_perc = round(null_count/total_count, 6),
             distinct_perc = round(distinct_count/total_count, 6)) %>%
      #  select(variable, class, null_perc, distinct_perc, max, min, mean, median, skew, standard_deviation, kurtosis, null_count, distinct_count, total_count)
      select(variable, class, null_perc, distinct_perc, table, max, min, mean, median, standard_deviation, null_count, distinct_count, total_count)  
  }
  return(describe)
}

# Adds a top column (possibly add a top_1 and top_2 columns)
f6 <- function(data, type = "basic") {
  if (type == "basic") {
    describe <- tibble::tibble(
      variable = names(data),
      class = purrr::map_chr(data, ~class(.x)),
      null_count = purrr::map_dbl(data, ~sum(is.na(.x))),
      distinct_count = purrr::map_dbl(data, ~data.table::uniqueN(.x)),
      total_count = purrr::map_dbl(data, ~length(.x))
    )    
    describe <- mutate(describe, 
                       null_perc = round(null_count/total_count, 6),
                       distinct_perc = round(distinct_count/total_count, 6)
    ) %>%
      select(variable, class, null_perc, distinct_perc, null_count, distinct_count, total_count)
  } else {
    
    numeric <- select_if(data, is.numeric)
    
    describe1 <- tibble::tibble(
      variable = names(data),
      class = purrr::map_chr(data, ~class(.x)),
      null_count = purrr::map_dbl(data, ~sum(is.na(.x))),
      distinct_count = purrr::map_dbl(data, ~data.table::uniqueN(.x)),
      total_count = purrr::map_dbl(data, ~length(.x)),
      table = purrr::map(data, ~data.table::data.table(.x)[, .N, keyby = .x] %>% as_tibble %>% rename(count = N, value = .x) %>% arrange(desc(count)))
    ) 
    
    describe2 <- tibble::tibble(
      variable = names(numeric),
      max = purrr::map_dbl(numeric, ~max(.x, na.rm = TRUE)),
      min = purrr::map_dbl(numeric, ~min(.x, na.rm = TRUE)),
      standard_deviation = purrr::map_dbl(numeric, ~sd(.x, na.rm = TRUE)),      
      mean = purrr::map_dbl(numeric, ~mean(.x, na.rm = TRUE)),
      median = purrr::map_dbl(numeric, ~median(.x, na.rm = TRUE))#,
      #      kurtosis = purrr::map_dbl(numeric, ~moments::kurtosis(.x, na.rm = TRUE)),
      #      skew = purrr::map_dbl(numeric, ~moments::skewness(.x, na.rm = TRUE))       
    )
    
    describe <- describe1 %>%
      left_join(describe2, by = "variable") %>%
      mutate(null_perc = round(null_count/total_count, 6),
             distinct_perc = round(distinct_count/total_count, 6),
             top_value = purrr::map_chr(table, ~data.table::first(.x) %$% value)
             ) %>%
      #  select(variable, class, null_perc, distinct_perc, max, min, mean, median, skew, standard_deviation, kurtosis, null_count, distinct_count, total_count)
      select(variable, class, null_perc, distinct_perc, table, top_value, max, min, mean, median, standard_deviation, null_count, distinct_count, total_count)  
  }
  return(describe)
}

#Messing around with the table list column
describe <- f6(i_data, type = "advanced")

describe %>% filter(null_perc < .5) %>% select(variable, class, null_perc, distinct_perc, table, top_value, distinct_count) %>% print(n = Inf)
select(describe, table) %>% print(n = Inf) %>% tidyr::unnest() # doesn't work.

library(magrittr)
describe %>% filter(null_perc < .1) %$% table

system.time(
  f4(i_data, type = "advanced")
)

system.time(
  f5(i_data, type = "advanced")
)

system.time(
  f6(i_data, type = "advanced")
)

system.time(
  prep_describe(i_data)
)


# profile the functions
Rprof(tmp <- tempfile())
prep_describe(i_data)
Rprof()
summaryRprof(tmp)

Rprof(tmp <- tempfile())
f4(i_data, type = "advanced")
Rprof()
summaryRprof(tmp)

Rprof(tmp <- tempfile())
f5(i_data, type = "advanced")
Rprof()
summaryRprof(tmp)

#find most frequent value (this isn't what I want!)
# base it on the list column. Just grab the 1st.
library(magrittr)
purrr::map(describe$table, ~data.table::first(.x))

purrr::map_chr(describe$table, ~data.table::first(.x) %$% value)

map(i_data, ~which.max(.x))
x <- c(1:4, 0:5, 11)
which.min(x)
which.max(x)


# possibly use this method later
fncts <- list(
  length = list(function(x) length(x)),
  max = list(function(x) max(x)),
  name = list(function(x) class(x))
)

invk <- function(func, data) {
  invoke_map(rep(func, ncol(mtcars)), map(data, ~list(.x)))
}

test <- map(fncts, ~ invk(.x, data = mtcars))





