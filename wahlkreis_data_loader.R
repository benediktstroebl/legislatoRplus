# btw02
btw02_kerg <- read.csv2("data/btw02/wahlkreis_data/btw2002_kerg.csv",
                          encoding = "UTF-8-SIG") %>%
  select(Nr, Name) %>% 
  rename("WKR_NR" = Nr,
         "WKR_NAME" = Name)

# btw05
btw05_kerg <- read.csv2("data/btw05/wahlkreis_data/btw2005_kerg.csv",
                        encoding = "UTF-8-SIG") %>%
  select(Nr, Gebiet) %>% 
  rename("WKR_NR" = Nr,
         "WKR_NAME" = Gebiet)

# btw09
btw09_kerg <- read.csv2("data/btw09/wahlkreis_data/btw2009_kerg.csv",
                        encoding = "UTF-8-SIG") %>%
  select(Nr, Gebiet) %>% 
  rename("WKR_NR" = Nr,
         "WKR_NAME" = Gebiet)

# btw13
btw13_kerg <- read.csv2("data/btw13/wahlkreis_data/btw2013_kerg.csv",
                        encoding = "UTF-8-SIG") %>%
  select(Nr, Gebiet) %>% 
  rename("WKR_NR" = Nr,
         "WKR_NAME" = Gebiet)

# btw17
btw17_kerg <- read_delim("data/btw17/wahlkreis_data/btw2017_kerg.csv", 
                           delim = ";", escape_double = FALSE) %>%
  select(Nr, Gebiet) %>% 
  rename("WKR_NR" = Nr,
         "WKR_NAME" = Gebiet) %>% 
  drop_na()

# btw21
btw21_kerg <- read_delim("data/btw21/wahlkreis_data/btw2021_kerg.csv", 
                         delim = ";", escape_double = FALSE) %>%
  select(Nr, Gebiet) %>% 
  rename("WKR_NR" = Nr,
         "WKR_NAME" = Gebiet) %>% 
  drop_na()

  