# i_data <- readr::read_rds(normalizePath("data/zillow/propertyData.rds"))
(i_data <- readr::read_rds("C:/Users/jgregor1/Desktop/propertyData.rds"))
library(dplyr)
library(magrittr)
library(rlang)

# Function to select data frame lists
lselect <- function(lst) {
  tibble::enframe(lst) %>% tidyr::unnest()
}


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

# Adds in table logic for the advanced pull.
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
      table = purrr::map(data, ~data.table::data.table(.x)[, .N, keyby = .x] %>% 
                           as_tibble %>% 
                           rename(count = N, value = .x) %>%
                           mutate(count_rank = min_rank(desc(count))) %>%
                           arrange(desc(count)))
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
    
    names(describe$table) <- describe$variable
  }
  return(describe)
}

# Converts the table value column to a character value. Takes quite a bit longer though.
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
      table = purrr::map(data, ~ data.table::data.table(.x)[, .N, keyby = .x] %>% 
                           as_tibble %>% 
                           rename(count = N, value = .x) %>%
                           mutate(value = as.character(value)) %>%
                           arrange(desc(count)))
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
    
    names(describe$table) <- describe$variable
  }
  return(describe)
}


# Adds a top column (possibly add a top_1 and top_2 columns)
f7 <- function(data, type = "basic") {
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

# Test stuff
system.time(
  f5(i_data, type = "advanced")
)

system.time(
  f6(i_data, type = "advanced")
)

describe <- f5(mtcars, type = "advanced")

# it
describe %>% filter(distinct_perc < .1) %>% select_df(table) %>% filter(value_rank <= 3) %>% print(n = Inf)

select_df(describe$table) %>% print(n = Inf)


# create a function that will allow you to select in this manner from tibble lists...
  # not sure why but dplyr removes names when filtering.
lselect <- function(data, column) {
  column <- enquo(column)
  # use something like "data %>% select(!!column) %>% tidyr::unnest(.id = "name") %>% class" to determine if it's a data frame.
  data %>% select(!!column) %>% tidyr::unnest(.id = "name")
}




describe %>% select(table) %>% tidyr::unnest(.id = "name") %>% class

data %>% select(table)

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





