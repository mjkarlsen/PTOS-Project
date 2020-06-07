
column_cleaner <- function(colname) {
  #Return NA if the # of underscores don't match or if the column starts with a different letter
  col_split <- str_split(colname, '[_]')
  # 
  # if (substr(str1, start = 1, stop = 1) != substr(str2, start = 1, stop = 1) || 
  #     (str_count(str1, '_') != str_count(str2, '_'))) {
  #   return(NA)
  # } 
  
  clean_results <- col_split
  return(clean_results)  
}


column_name_matcher <- function(str1, str2) {
  #Return NA if the # of underscores don't match or if the column starts with a different letter
  if (substr(str1, start = 1, stop = 1) != substr(str2, start = 1, stop = 1) || 
      (str_count(str1, '_') != str_count(str2, '_'))) {
    return(NA)
  } 
  
  # Matches the first two splits of characters 
  if ((str_split(str1, '[_]')[[1]][1] == str_split(str2, '[_]')[[1]][1]) & 
      (str_split(str1, '[_]')[[1]][2] == str_split(str2, '[_]')[[1]][2]))  {
    search_results <- paste0(str_split(str1, '[_]')[[1]][1], 
                             "_", 
                             str_split(str1, '[_]')[[1]][2], 
                             "_")
  } 
  else if(str_split(str1, '[_]')[[1]][1] == str_split(str2, '[_]')[[1]][1]) {
    search_results <- paste0(str_split(str1, '[_]')[[1]][1], "_")
  }
  else search_results <- NA
  
  return(search_results)  
  
}

column_name_matcher('a_md_da', 'a_md_ta')
column_name_matcher('ais_07', 'ais_09')
column_name_matcher('edp05_pgy', 'edp04_pgy')
column_name_matcher('edp04_pgy', 'edp04_pgy')
column_name_matcher('c01_c_da', 'c01_c_ta')


str_split('ais_07', '[_]')[[1]][1] == str_split('ais_08', '[_]')[[1]][1]

# col_names_grps %>% 
#   filter(V1 == 'a_md_da') %>%
#   mutate(some_name = column_name_matcher(V1, V2))


empty_as_na <- function(x){
  if("factor" %in% class(x)) x <- as.character(x) ## since ifelse wont work with factors
  ifelse(as.character(x)!="", x, NA)
}
