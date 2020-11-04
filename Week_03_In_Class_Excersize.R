library(tidyverse)
library(lubridate)
library(readxl)
raw_df = read_xlsx("C:\\data\\EVDS_istanbul_property_data\\EVDS_istanbul_property_data.xlsx")
#raw_df
row_id_df = raw_df %>% transmute(id = row_number(), raw_df)
#row_id_df
description_row_id = row_id_df %>% filter(is.na(Tarih)) %>% summarise(description_row_id = min(id))
minimum_description_row_id = description_row_id$description_row_id[1]
clean_df = row_id_df %>% filter(id < minimum_description_row_id)
clean_df