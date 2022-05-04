# desc  : Tidying data transaksi RTGS total by peserta
# author: Hanzholah Shobri
# date  : 2021-01-14

library(here)
library(tidyverse)
library(readxl)
library(zoo)
library(scales)
library(lubridate)



# A. Import data ----------------------------------------------------------

.path <- here("data/raw/Data Transaksi Per Jam R2.xlsx")
excel_sheets(.path)

.raw_by_peserta <- read_excel(.path, sheet = "Peserta", skip = 4)



# B. Tidy Data ------------------------------------------------------------

.del_psrta <- (which(.raw_by_peserta[,1] == "Total"):nrow(.raw_by_peserta))
df_by_peserta <- .raw_by_peserta %>%
  rename(kelompok = ...1, waktu = ...2) %>%
  slice(-.del_psrta) %>% 
  mutate_at(vars(matches(month.abb)), as.numeric)

df_by_peserta <- df_by_peserta %>%
  fill(kelompok) %>%
  filter(waktu != "0000", waktu != "All")

df_by_peserta %>% names
df_by_peserta_nom <- df_by_peserta %>%
  select(1:74) %>% 
  pivot_longer(-(1:2), names_to = "bulan", values_to = "nominal") 
df_by_peserta_vol <- df_by_peserta %>%
  select(1:2, 76:147) %>%
  pivot_longer(-(1:2), names_to = "bulan", values_to = "volume")

df_by_peserta <- df_by_peserta_nom %>%
  mutate(
    bulan = str_replace(bulan, "(\\.){1,3}\\d*", "") %>% 
      as.yearmon(),
    kelompok = factor(kelompok),
    waktu = factor(waktu) %>%
      fct_relevel(">= 19:00", after = Inf),
    volume = df_by_peserta_vol$volume,
    volume = ifelse(!is.na(volume), volume, 0),
    nominal = ifelse(!is.na(nominal), nominal, 0)
  ) %>% 
  select(kelompok, bulan, waktu, volume, nominal) %>%
  arrange(kelompok, bulan, waktu)



# C. Export Data ----------------------------------------------------------

write_csv(df_by_peserta, here("data/tidy/tidy_by_peserta.csv"))
