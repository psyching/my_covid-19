# テストコメント

library(tabulizer)
library(purrr)
library(dplyr)
library(stringr)

df_combined <- tabulizer::extract_tables("1_downloadFiles/20200424-sitrep-95-covid-19.pdf") %>%
                   purrr::map_dfr(as.data.frame)

df_combined <- df_combined %>%
  mutate(V1 = str_replace(V1, "\r", " ")) %>%
  mutate(V1 = str_replace(V1, "ã", "a")) %>%
  mutate(V1 = str_replace(V1, "í", "i")) %>%
  mutate(V1 = str_replace(V1, "é", "e")) %>%
  mutate(V1 = str_replace(V1, "ô", "o")) %>%
  mutate(V1 = str_replace(V1, "ç", "c")) %>%
  mutate(V1_prev = lag(V1, n = 1)) %>%
  mutate(V1 = ifelse(V1_prev == "Lao People's" & V1 == "Democratic Republic", "Lao People's Democratic Republic", V1)) %>%
  mutate(V1 = ifelse(V1_prev == "(Commonwealth of" & V1 == "the)", "Northern Mariana Islands (Commonwealth of the)", V1)) %>%
  mutate(V1 = ifelse(V1_prev == "United Republic of" & V1 == "Tanzania", "United Republic of Tanzania", V1)) %>%
  mutate(V1 = ifelse(V1_prev == "Central African" & V1 == "Republic", "Central African Republic", V1)) %>%
  mutate(V1 = ifelse(V1_prev == "Sao Tome and" & V1 == "Principe", "Sao Tome and Principe", V1)) %>%
  mutate(V1 = gsub("Kosovo.*", "Kosovo", V1)) %>%
  mutate(V1 = gsub(".*d’Ivoire", "Cote d'Ivoire", V1)) %>%
  mutate(V1 = gsub(".*conveyance \\(Diamond.*", "International conveyance (Diamond Princess)", V1)) %>%
  mutate(V6 = str_replace(V6, "\r", " ")) %>%
  mutate(V6 = ifelse(V6 == "transmission", "Community transmission", V6)) %>%
  filter(V1 != "") %>%
  filter(V1 != "Grand total") %>%
  filter(V1 != "Territory/Area  †") %>%
  filter(V2 != "") %>%
  select(c("V1", "V2", "V3", "V4", "V5", "V6", "V7")) %>%
  rename("Reporting Country/Territory/Area"=V1
        ,"Total confirmed cases"=V2
        ,"Total confirmed new cases"=V3
        ,"Total deaths"=V4
        ,"Total new deaths"=V5
        ,"Transmission classification"=V6
        ,"Days since last reported case"=V7)

write.csv(df_combined, "20200424-sitrep-95-covid-19.csv", row.names = FALSE)
