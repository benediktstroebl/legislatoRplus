# Political
deu_political <- get_political(legislature = "deu")

# Core
deu_core <- get_core(legislature = "deu")

# Portraits
deu_portraits <- get_portrait(legislature = "deu")


# Join with wahlkreis_data
deu_final <- deu_political %>% 
  
