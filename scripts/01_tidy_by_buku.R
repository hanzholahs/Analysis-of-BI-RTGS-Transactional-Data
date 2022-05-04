# desc  : Tidying data transaksi RTGS total by buku bank
# author: Hanzholah Shobri
# date  : 2021-01-12

library(here)
library(tidyverse)
library(readxl)
library(zoo)
library(scales)
library(lubridate)



# A. Import data ----------------------------------------------------------

.path <- here("data/raw/Data Transaksi Per Jam R2.xlsx")
excel_sheets(.path)

.raw_by_buku <- read_excel(.path, sheet = "BUKU", skip = 4)



# B. Tidy Data ------------------------------------------------------------
.del_buku <- (which(.raw_by_buku[,1] == "Total"):nrow(.raw_by_buku))
df_by_buku <- .raw_by_buku  %>%
  rename(kelompok = ...1, waktu = ...2) %>%
  slice(-.del_buku) %>% 
  mutate_at(vars(matches(month.abb)),as.numeric)

df_by_buku <- df_by_buku %>%
  tidyr::fill(kelompok) %>%
  filter(waktu != "0000", waktu != "All")

df_by_buku %>% names()
df_by_buku_nom <- df_by_buku %>%
  select(1:74) %>% 
  pivot_longer(-(1:2), names_to = "bulan", values_to = "nominal") %>%
  select(kelompok, bulan, everything())
df_by_buku_vol <- df_by_buku %>%
  select(1:2, 75:146) %>%
  pivot_longer(-(1:2), names_to = "bulan", values_to = "volume") %>%
  select(kelompok, bulan, everything())

df_by_buku <- df_by_buku_nom %>%
  mutate(
    bulan = str_replace(bulan, "(\\.){1,3}\\d*", "") %>% 
      as.yearmon(),
    kelompok = factor(kelompok),
    waktu = factor(waktu) %>%
      fct_relevel(">= 19:00", after = Inf),
    volume = df_by_buku_vol$volume,
    volume = ifelse(!is.na(volume), volume, 0),
    nominal = ifelse(!is.na(nominal), nominal, 0)
  ) %>% 
  select(kelompok, bulan, waktu, everything()) %>%
  arrange(kelompok, bulan, waktu)



# C. Export Data ----------------------------------------------------------

write_csv(df_by_buku, here("data/tidy/tidy_by_buku.csv"))



# D. Data Exploration -----------------------------------------------------
# 
# df_by_buku %>%
#   group_by(waktu) %>%
#   summarise(
#     nominal = sum(nominal, na.rm = T),
#     volume = sum(volume, na.rm = T)
#   ) %>%
#   print(n=Inf)
# 
# df_by_buku %>%
#   group_by(waktu) %>%
#   summarise(nominal = sum(nominal)) %>%
#   ggplot(aes(x = waktu, y = nominal)) +
#   geom_col() +
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 30, hjust = 1))
# 
# df_by_buku %>%
#   group_by(waktu) %>%
#   summarise(volume = sum(volume)) %>%
#   ggplot(aes(x = waktu, y = volume)) +
#   geom_col() +
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 30, hjust = 1))
# 
# df_by_buku %>%
#   group_by(tahun = year(bulan), waktu) %>%
#   summarise(volume = sum(volume)) %>%
#   ggplot(aes(x = waktu, y = volume)) +
#   geom_col() +
#   scale_x_discrete(
#     breaks = unique(df_by_buku$waktu)[c(seq(1,28,3))]
#   ) +
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
#   facet_wrap(.~tahun, scales = "free_y")
# 
# df_by_buku %>%
#   group_by(kelompok, tahun = year(bulan), waktu) %>%
#   summarise(volume = sum(volume)) %>%
#   filter(kelompok %in% levels(kelompok)[-(8:11)]) %>%
#   ggplot(aes(x = waktu, y = volume)) +
#   geom_col() +
#   scale_x_discrete(
#     breaks = unique(df_by_buku$waktu)[c(seq(1,28,3))]
#   ) +
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
#   facet_wrap(.~kelompok, scales = "free_y")
# 
# df_by_buku %>%
#   group_by(kelompok, tahun = year(bulan), waktu) %>%
#   summarise(nominal = sum(nominal)) %>%
#   filter(kelompok %in% levels(kelompok)[-(8:11)]) %>%
#   ggplot(aes(x = waktu, y = nominal)) +
#   geom_col() +
#   scale_x_discrete(
#     breaks = unique(df_by_buku$waktu)[c(seq(1,28,3))]
#   ) +
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
#   facet_wrap(.~kelompok, scales = "free_y", ncol = 2)
