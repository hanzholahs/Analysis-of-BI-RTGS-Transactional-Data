# desc  : Tidying data transaksi RTGS by kelompok peserta
# author: Hanzholah Shobri
# date  : 2021-01-12

library(here)
library(tidyverse)
library(zoo)
library(readxl)
library(scales)
library(lubridate)



# A. Import data ----------------------------------------------------------

.path <- here("data/raw/Data Transaksi Per Jam R2.xlsx")
excel_sheets(.path)

.raw_by_kelompok <- read_excel(.path, sheet = "Kelompok Bank", skip = 4)



# B. Tidy data ------------------------------------------------------------

# menghapus row yang tidak relevan
.del_kel <- (which(.raw_by_kelompok[,1] == "All"):nrow(.raw_by_kelompok))
df_by_kelompok <- .raw_by_kelompok %>%
  rename(kelompok = ...1, waktu = ...2) %>%
  slice(- .del_kel) %>% 
  mutate_at(vars(matches(month.abb)), as.numeric)

# memfilter data yang relevan
df_by_kelompok <- df_by_kelompok %>%
  tidyr::fill(kelompok) %>% 
  filter(waktu != "0000", waktu != "All")

df_by_kelompok %>% names
# Membagi data berdasarkan tipe angka (nominal dan volume)
df_by_kelompok_nom <- df_by_kelompok %>%
  select(1:74) %>% 
  pivot_longer(-(1:2), names_to = "bulan", values_to = "nominal") 
df_by_kelompok_vol <- df_by_kelompok %>%
  select(1:2, 75:146) %>%
  pivot_longer(-(1:2), names_to = "bulan", values_to = "volume")


df_by_kelompok <- df_by_kelompok_nom %>%
  mutate(
    bulan = str_replace(bulan, "(\\.){1,3}\\d*", "") %>% 
      as.yearmon(),
    kelompok = factor(kelompok),
    waktu = factor(waktu) %>%
      fct_relevel(">= 19:00", after = Inf),
    volume = df_by_kelompok_vol$volume,
    volume = ifelse(!is.na(volume), volume, 0),
    nominal = ifelse(!is.na(nominal), nominal, 0)
  ) %>% 
  select(kelompok, bulan, waktu, everything()) %>%
  arrange(kelompok, bulan, waktu) %>%
  group_by(kelompok, bulan, waktu) %>% 
  summarise(nominal = sum(nominal),  # kalkulasi untuk merger col 'Others'
            volume = sum(volume))    # karena pada data raw, ada dua



# C. Export Data ----------------------------------------------------------

write_csv(df_by_kelompok, here("data/tidy/tidy_by_kelompok.csv"))



# D. Data Exploration -----------------------------------------------------

# df_by_kelompok %>%
#   group_by(waktu) %>%
#   summarise(
#     nominal = sum(nominal, na.rm = T),
#     volume = sum(volume, na.rm = T)
#   ) %>%
#   print(n=Inf)
# 
# df_by_kelompok %>%
#   filter(
#     kelompok %in% c("BPD", "Bank Asing", "Bank Campuran", "Bank Pemerintah",
#                     "Bank Swasta Nasional", "Bank Syariah")
#   ) %>%
#   group_by(bulan) %>%
#   summarise(nominal = sum(nominal))
# 
# df_by_kelompok %>%
#   group_by(waktu) %>%
#   summarise(nominal = sum(nominal)) %>%
#   ggplot(aes(x = waktu, y = nominal)) +
#   geom_col() +
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 30, hjust = 1))
# 
# df_by_kelompok %>%
#   group_by(waktu) %>%
#   summarise(volume = sum(volume)) %>%
#   ggplot(aes(x = waktu, y = volume)) +
#   geom_col() +
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 30, hjust = 1))
# 
# df_by_kelompok %>%
#   group_by(tahun = year(bulan), waktu) %>%
#   summarise(volume = sum(volume)) %>%
#   ggplot(aes(x = waktu, y = volume)) +
#   geom_col() +
#   scale_x_discrete(
#     breaks = unique(df_by_kelompok$waktu)[c(seq(1,28,3))]
#   ) +
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
#   facet_wrap(.~tahun, scales = "free_y")
# 
# df_by_kelompok %>%
#   group_by(kelompok, tahun = year(bulan), waktu) %>%
#   summarise(volume = sum(volume)) %>%
#   filter(kelompok %in% levels(kelompok)[-(8:11)]) %>%
#   ggplot(aes(x = waktu, y = volume)) +
#   geom_col() +
#   scale_x_discrete(
#     breaks = unique(df_by_kelompok$waktu)[c(seq(1,28,3))]
#   ) +
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
#   facet_wrap(.~kelompok, scales = "free_y")
# 
# df_by_kelompok %>%
#   group_by(kelompok, tahun = year(bulan), waktu) %>%
#   summarise(nominal = sum(nominal)) %>%
#   ggplot(aes(x = waktu, y = nominal)) +
#   geom_col() +
#   scale_x_discrete(
#     breaks = unique(df_by_kelompok$waktu)[c(seq(1,28,3))]
#   ) +
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
#   facet_wrap(.~kelompok, scales = "free_y", ncol = 2)
