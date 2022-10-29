library(rvest)
library(httr)
library(stringr)
library(tidyverse)
options(digits = 3)

clean_second_round_dataframe = function(dataframe) {
  remove_number_from_pollster_name = function(pollster_name) {
    return(str_replace(pollster_name, regex("\\[[:digit:]+]"), ""))
  }
  remove_header_row = function() {
    return(dataframe[-1,])
  }
  remove_empty_rows = function() {
    return(dataframe %>% filter(!str_detect(spread, 'debate')))
  }
  percentage_value_to_decimal = function(percentage_value) {
    decimal_value = str_replace(percentage_value, "%", "")
    decimal_value = str_replace(decimal_value, ",", ".")
    return(as.numeric(decimal_value)/ 100)
  }
  format_period_column = function() {
    return(dataframe %>% mutate(period = 
                           str_replace_all(period, "[0-9]+–", "")))
  }
  format_pollster_name = function() {
    return(dataframe %>% mutate(pollster = remove_number_from_pollster_name(pollster)))
  }
  one_poll_per_pollster = function() {
    return(dataframe %>% 
             group_by(pollster) %>% 
             filter(period == max(period)) %>%
             ungroup())
  }
  dataframe_headers = c("pollster", 
                        "period", 
                        "samplesize", 
                        "lula", 
                        "bolsonaro",
                        "blank",
                        "spread")
  numeric_columns = c("lula", "bolsonaro", "blank", "spread")
  
  colnames(dataframe) = dataframe_headers
  dataframe = remove_header_row()
  dataframe = remove_empty_rows()
  dataframe = format_period_column()
  dataframe = format_pollster_name()
  for (column in numeric_columns) {
    dataframe[column] = sapply(dataframe[column], percentage_value_to_decimal)
  }
  dataframe = one_poll_per_pollster()
  return(dataframe)
}

save_dataframe = function(dataframe, filename) {
  write.csv(dataframe, file = filename, row.names=FALSE)
  print("Dataframe saved")
}

url = "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2022_Brazilian_presidential_election"
web_page =  url %>% httr::GET(config = httr::config(ssl_verifypeer = FALSE)) %>% 
  read_html()

second_round_table_css_selector = "table.wikitable:nth-child(38)"
second_round_table = web_page %>% html_element(second_round_table_css_selector)
second_round_tibble = html_table(second_round_table)
second_round_dataframe = as.data.frame(second_round_tibble)
clean_second_round_dataframe = clean_second_round_dataframe(second_round_dataframe)
save_dataframe(clean_second_round_dataframe, "brazilian-presidential-election-polling-data.csv")
