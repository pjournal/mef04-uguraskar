#library(readxl)
#library(ggplot2)
library(tidyverse)
library(lubridate)
library(zoo)

raw_df = read_xlsx("C:\\data\\EVDS_istanbul_property_data\\EVDS_istanbul_property_data.xlsx")
#raw_df
row_id_df = raw_df %>% transmute(id = row_number(), raw_df)
#row_id_df
description_row_id = row_id_df %>% filter(is.na(Tarih)) %>% summarise(description_row_id = min(id))
minimum_description_row_id = description_row_id$description_row_id[1]
clean_df = row_id_df %>%
  filter(id < minimum_description_row_id) %>%
  drop_na() %>%
  filter(id < minimum_description_row_id) %>% 
  dplyr::rename_all(funs(make.names(.))) %>% 
  transmute(TARIH = as.yearmon(Tarih), TOPLAM_SATIS = as.numeric(TP.AKONUTSAT1.T40), IPOTEK_SATIS = as.numeric(TP.AKONUTSAT1.T40), ILK_EL_SATIS = TP.AKONUTSAT3.T40, IKINCI_EL_SATIS = TP.AKONUTSAT4.T40, YABANCI_SATIS = TP.DISKONSAT.ISTANBUL, YENI_KONUT_FIYAT_ENDEKS = TP.HEDONIKYKFE.IST, TR10 = TP.HKFE02, KONUT_BIRIM_FIYAT = TP.TCBF02.ISTANBUL)
#https://stackoverflow.com/questions/10688137/how-to-fix-spaces-in-column-names-of-a-data-frame-remove-spaces-inject-dots
#not_nulls_df = clean_df %>% drop_na()

str(clean_df)

analytical_df = clean_df %>% 
  transmute(TARIH, ILK_EL_SATIS, ILK_EL_ORAN = (ILK_EL_SATIS / TOPLAM_SATIS), IKINCI_EL_SATIS, IKINCI_EL_ORAN = (IKINCI_EL_SATIS / TOPLAM_SATIS), YABANCI_SATIS, YABANCI_SATIS_ORAN = (YABANCI_SATIS / TOPLAM_SATIS), TC_SATIS = TOPLAM_SATIS - YABANCI_SATIS, TC_SATIS_ORAN = (TOPLAM_SATIS - YABANCI_SATIS)/TOPLAM_SATIS)

analytical_df

#not_nulls_df %>% 
#transmute(ILK_EL_SATIS = TP.AKONUTSAT3.T40, ILK_EL_SATIS_YUZDE = (TP.AKONUTSAT3.T40/as.numeric(TP.AKONUTSAT1.T40)), IKINCI_EL_SATIS = TP.AKONUTSAT4.T40, TOPLAM_SATIS = TP.AKONUTSAT1.T40)

#str(not_nulls_df)

ggplot(analytical_df, aes(x=as.yearmon(TARIH), group=1))+
  geom_line(aes(y = ILK_EL_ORAN*100), color = "red") +
  geom_line(aes(y = IKINCI_EL_ORAN*100), color = "green", linetype = "twodash")
#https://www.datanovia.com/en/blog/how-to-create-a-ggplot-with-multiple-lines/