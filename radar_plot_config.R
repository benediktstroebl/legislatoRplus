# coord function for plot
coord_radar <- function (theta = "x", start = 0, direction = 1) {
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") 
    "y"
  else "x"
  ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start, 
          direction = sign(direction),
          is_linear = function(coord) TRUE)
}

# normalize variables
normalize <- function(x) {
  if (is.numeric(x)) {
    (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
  } else {
    print("Vector is not numeric")
  }
}

# metrics for radar plot --------------------------------------------------

# traffic per MP
traffic_per_mp_de <- deu_traffic %>%
  group_by(pageid) %>% 
  summarise(
    min_date_traffic = min(date),
    max_date_traffic = max(date),
    sum_traffic = sum(traffic),
    date_range_years_traffic = difftime(max_date_traffic, min_date_traffic) %>% lubridate::time_length("years")
  ) %>% 
  ungroup

# sessions per MP
sessions_per_mp_de <- deu_political %>% 
  group_by(pageid) %>% 
  summarise(session_n = n()) %>% 
  ungroup

# entry age first session
entry_age_first_session_de <- deu_political %>% 
  left_join(deu_core) %>% 
  group_by(pageid) %>% 
  filter(session == min(session)) %>% 
  ungroup() %>% 
  mutate(
    age_session_start = if_else(
      session_start > birth,
      time_length(difftime(session_start, birth), "years") %>% round(3),
      NA_real_
    )
  ) %>% 
  select(pageid, wikidataid, age_session_start) %>% 
  distinct(pageid, .keep_all = T)

# age at death
age_mp_de <- deu_core %>% 
  mutate(
    age = case_when(
      death > birth ~ time_length(difftime(death, birth), "years") %>% round(3),
      is.na(death) ~ time_length(difftime(Sys.Date(), birth), "years") %>% round(3),
      TRUE ~ NA_real_
    )
  ) %>% 
  select(pageid, wikidataid, age)

# final df
deu_metrics <- deu_core %>% 
  distinct(pageid) %>% 
  left_join(traffic_per_mp_de) %>% 
  left_join(sessions_per_mp_de) %>% 
  left_join(entry_age_first_session_de) %>% 
  left_join(age_mp_de) 
