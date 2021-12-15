# btw02
btw02_kerg <- read.csv2("data/btw02/wahlkreis_data/btw2002_kerg.csv",
                          encoding = "UTF-8-SIG") %>%
  select(Nr, Name) %>% 
  rename("WKR_NR" = Nr,
         "WKR_NAME" = Name) %>% 
  mutate(session = 15)

# btw05
btw05_kerg <- read.csv2("data/btw05/wahlkreis_data/btw2005_kerg.csv",
                        encoding = "UTF-8-SIG") %>%
  select(Nr, Gebiet) %>% 
  rename("WKR_NR" = Nr,
         "WKR_NAME" = Gebiet) %>% 
  mutate(session = 16)

# btw09
btw09_kerg <- read.csv2("data/btw09/wahlkreis_data/btw2009_kerg.csv",
                        encoding = "UTF-8-SIG") %>%
  select(Nr, Gebiet) %>% 
  rename("WKR_NR" = Nr,
         "WKR_NAME" = Gebiet) %>% 
  mutate(session = 17)

# btw13
btw13_kerg <- read.csv2("data/btw13/wahlkreis_data/btw2013_kerg.csv",
                        encoding = "UTF-8-SIG") %>%
  select(Nr, Gebiet) %>% 
  rename("WKR_NR" = Nr,
         "WKR_NAME" = Gebiet) %>% 
  mutate(session = 18)

# btw17
btw17_kerg <- read_delim("data/btw17/wahlkreis_data/btw2017_kerg.csv", 
                           delim = ";", escape_double = FALSE) %>%
  select(Nr, Gebiet) %>% 
  rename("WKR_NR" = Nr,
         "WKR_NAME" = Gebiet) %>% 
  mutate(session = 19) %>% 
  drop_na()

# btw21
btw21_kerg <- read_delim("data/btw21/wahlkreis_data/btw2021_kerg.csv", 
                         delim = ";", escape_double = FALSE) %>%
  select(Nr, Gebiet) %>% 
  rename("WKR_NR" = Nr,
         "WKR_NAME" = Gebiet) %>% 
  mutate(WKR_NR = as.numeric(WKR_NR)) %>% 
  mutate(session = 20) %>% 
  drop_na()


# Bind alls dataframe together and create column for joining with legislator df
kerg_full <- bind_rows(btw02_kerg, 
                       btw05_kerg,
                       btw09_kerg,
                       btw13_kerg,
                       btw17_kerg,
                       btw21_kerg) %>% 
  mutate(WKR_NAME_join = str_replace_all(WKR_NAME, char_to_replace_for_join, "") %>% 
           str_to_lower(),
         session = as.integer(session)) %>%
  mutate(
    WKR_NAME_join = case_when(WKR_NAME_join == "brandenburganderhavelpotsdammittelmarkihavellandiiiteltowflämingi" & session==15 ~ "brandenburgadhavelpotsdammittelmarkihavellandiiiteltowflämingi",
                              WKR_NAME_join == "herfordmindenlübbeckeii"& session==15 ~ "herfordmindenlübbeckei",
                              WKR_NAME_join == "berlinlichtenberg" & session==15 ~ "berlinlichtenberghohenschönhausen",
                              WKR_NAME_join == "sanktwendel" & session==17 ~ "stwendel",
                              WKR_NAME_join == "dahmespreewaldteltowflämingiiioberspreewaldlausitzi"  & session==17 ~ "dahmespreewald",
                              WKR_NAME_join == "brandenburganderhavelpotsdammittelmarkihavellandiiiteltowflämingi"  & session==17 ~ "brandenburganderhavel",
                              WKR_NAME_join == "brandenburganderhavelpotsdammittelmarkihavellandiiiteltowflämingi"  & session==18 ~ "brandenburganderhavel",
                              WKR_NAME_join == "güterslohi"  & session==18 ~ "gütersloh",
                              WKR_NAME_join == "frieslandwilhelmshavenwittmund"  & session==18 ~ "frieslandwilhelmshaven",
                              WKR_NAME_join == "paderborngüterslohiii" & session==18  ~ "paderborn",
                              WKR_NAME_join == "wartburgkreiskreisfreiestadteisenachunstruthainichkreisi" & session==15 ~ "eisenachwartburgkreisunstruthainichkreisi",
                              WKR_NAME_join == "sanktwendel" & session==15 ~ "stwendel",
                              WKR_NAME_join == "euskirchenrheinerftkreisii" & session==18 ~ "euskirchenerftkreisii",
                              TRUE ~ WKR_NAME_join)
  ) %>%
  distinct()
  
  