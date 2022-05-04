# desc  : Import tidy rtgs data
# author: Hanzholah Shobri
# date  : 2021-01-18





library(here)
library(tidyverse)



# Import Data -------------------------------------------------------------

df_total <- read_csv(here("data/tidy/tidy_total.csv")) %>%
  mutate(
    waktu = factor(waktu, ordered = T) %>%
      fct_relevel(">= 19:00", after = Inf),
    bulan = as.yearmon(bulan)
  ) %>% 
  arrange(bulan, waktu)


df_by_kelompok <- read_csv(here("data/tidy/tidy_by_kelompok.csv")) %>%
  group_by(kelompok) %>% 
  filter(sum(nominal) != 0 && sum(volume) != 0) %>%
  ungroup() %>%
  mutate(
    kelompok = factor(kelompok) %>% #fct_drop %>%
      fct_recode(`Bank Swasta` = "Bank Swasta Nasional",
                 `Other FI` = "Other Financial Institutions"),
    waktu = factor(waktu, ordered = T) %>% fct_relevel(">= 19:00", after = Inf),
    bulan = as.yearmon(bulan)
  ) %>% 
  arrange(kelompok, bulan, waktu)


df_by_buku <- read_csv(here("data/tidy/tidy_by_buku.csv")) %>%
  mutate(
    kelompok = factor(kelompok),
    waktu = factor(waktu, ordered = T) %>% fct_relevel(">= 19:00", after = Inf),
    bulan = as.yearmon(bulan)
  ) %>% 
  arrange(kelompok, bulan, waktu)


df_by_peserta <- read_csv(here("data/tidy/tidy_by_peserta.csv")) %>%
  mutate(
    kelompok = factor(kelompok),
    waktu = factor(waktu, ordered = T) %>% fct_relevel(">= 19:00", after = Inf),
    bulan = as.yearmon(bulan)
  ) %>%
  arrange(kelompok, bulan, waktu)


df_by_trx <- read_csv(here("data/tidy/tidy_by_trx.csv")) %>%
  mutate(
    kelompok = factor(kelompok),
    waktu = factor(waktu, ordered = T) %>% fct_relevel(">= 19:00", after = Inf),
    bulan = as.yearmon(bulan)
  ) %>%
  arrange(kelompok, bulan, waktu)


df_by_subtrx <- read_csv(here("data/tidy/tidy_by_subtrx.csv")) %>%
  mutate(
    kelompok = factor(kelompok),
    waktu = factor(waktu, ordered = T) %>% fct_relevel(">= 19:00", after = Inf),
    bulan = as.yearmon(bulan)
  ) %>%
  arrange(kelompok, bulan, waktu) 





