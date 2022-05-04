# desc  : Tidying data transaksi RTGS total
# author: Hanzholah Shobri
# date  : 2021-01-11

library(here)
library(tidyverse)
library(readxl)
library(scales)
library(zoo)


# A. Import data ----------------------------------------------------------

.path <- here("data/raw/Data Transaksi Per Jam R2.xlsx")
excel_sheets(.path)

.raw_total <- read_excel(.path, sheet = "Total", skip = 4)



# B. Tidy Data ------------------------------------------------------------

df_total <- .raw_total %>%
  rename(waktu = ...1) %>%
  slice(-1, -31, -32) %>% 
  mutate_at(vars(starts_with(month.abb)), as.numeric)

df_total_nom <- df_total %>% 
  select(1:73) %>% # nominal Jan16 - Des21
  pivot_longer(-waktu, names_to = "bulan", values_to = "nominal")
df_total_vol <- df_total %>% 
  select(1, 74:145) %>% # volume Jan16 - Des21
  pivot_longer(-waktu, names_to = "bulan", values_to = "volume")

df_total <- df_total_nom %>%
  mutate(
    bulan = str_replace(bulan, "(\\.){1,3}\\d*", "") %>% as.yearmon(),
    waktu = factor(waktu) %>%
      fct_relevel(">= 19:00", after = Inf),
    volume = df_total_vol$volume,
    volume = ifelse(!is.na(volume), volume, 0),
    nominal = ifelse(!is.na(nominal), nominal, 0)
  ) %>% 
  select(bulan, waktu, volume, nominal) %>%
  arrange(bulan, waktu)



# C. Export Data ----------------------------------------------------------

write_csv(df_total, here("data/tidy/tidy_total.csv"))




# D. Test Accuracy --------------------------------------------------------
# 
# source(here("scripts/01_tidy_by_buku.R"))
# source(here("scripts/01_tidy_by_kelompok.R"))
# source(here("scripts/01_tidy_by_peserta.R"))
# sum_nom <- map(
#   list(df_total, df_by_buku, df_by_kelompok, df_by_peserta),
#   ~group_by(., bulan) %>% summarise(nominal = sum(nominal)) %>% pull
# )
# sum_vol <- map(
#   list(df_total, df_by_buku, df_by_kelompok, df_by_peserta),
#   ~group_by(., bulan) %>% summarise(volume = sum(volume)) %>% pull
# )
# 
# near(sum_nom[[1]], sum_nom[[3]], tol = 15) %>% mean
# near(sum_nom[[1]], sum_nom[[4]], tol = 15) %>% mean
# near(sum_nom[[3]], sum_nom[[4]], tol = 5) %>% mean
# near(sum_vol[[1]], sum_vol[[4]], tol = 0) %>% mean
# near(sum_vol[[1]], sum_vol[[4]], tol = 0) %>% mean
# near(sum_vol[[3]], sum_vol[[4]], tol = 0) %>% mean



# E. Data Exploration -----------------------------------------------------
# 
# df_total %>%
#   group_by(waktu) %>%
#   summarise(
#     nominal = sum(nominal, na.rm = T),
#     volume = sum(volume, na.rm = T)
#   ) %>%
#   print(n=Inf)
# 
# df_total %>%
#   group_by(waktu) %>%
#   summarise(nominal = sum(nominal)) %>%
#   ggplot(aes(x = waktu, y = nominal)) +
#   geom_col() +
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 30, hjust = 1))
# 
# df_total %>%
#   group_by(waktu) %>%
#   summarise(volume = sum(volume)) %>%
#   ggplot(aes(x = waktu, y = volume)) +
#   geom_col() +
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 30, hjust = 1))