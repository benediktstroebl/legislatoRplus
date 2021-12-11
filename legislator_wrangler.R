# Political
deu_political <- get_political(legislature = "deu")

# Core
deu_core <- get_core(legislature = "deu")

# Portraits
deu_portraits <- get_portrait(legislature = "deu")


# Join with wahlkreis_data
deu_final <- deu_political %>% 
  mutate(constituency_join = str_replace_all(constituency, "-|–|[:blank:]", "") %>% str_to_lower()) %>% 
  filter(session >= 15) %>% 
  filter(constituency != "Landesliste") %>% 
  left_join(kerg_full, by = c("constituency_join" = "WKR_NAME_join"))
  
  
  
  
  btw17_wahlkreisnamen <- read_csv2(
    "https://www.bundeswahlleiter.de/dam/jcr/90ae9719-97bb-43f9-8e26-3e9ef0f18609/btw17_wahlkreisnamen.csv",
    locale = locale("de", encoding = "latin1")
  ) %>%
  slice(-c(1:4)) %>%
  row_to_names(1) %>%
  mutate(wkr_merge = str_replace_all(WKR_NAME, "-|–|[:blank:]", "") %>% str_to_lower())

