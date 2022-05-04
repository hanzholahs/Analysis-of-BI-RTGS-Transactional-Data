# desc  : Tidying data transaksi RTGS total by jenis transaksi
# author: Hanzholah Shobri
# date  : 2021-06-02

library(here)
library(tidyverse)
library(readxl)
library(zoo)
library(scales)
library(lubridate)



# A. Import data ----------------------------------------------------------

.path <- here("data/raw/Data Transaksi per Jam Jenis R1.xlsx")
excel_sheets(.path)

.raw_by_trx <- read_excel(.path, sheet = "Jenis Transaksi", skip = 4)



# B. Tidy Data ------------------------------------------------------------

# Filter data
.del_trx <- which(.raw_by_trx[,1] == "All"):nrow(.raw_by_trx)

df_by_trx <- .raw_by_trx  %>%
  rename(kelompok = ...1, waktu = ...2) %>%
  slice(-.del_trx) %>%
  tidyr::fill(kelompok) %>%
  filter(waktu != "0000", waktu != "All") %>% 
  mutate_at(vars(matches(month.abb)), as.numeric) # krn col 51 raw data bkn num

# Pivot data berdasarkan nominal dan volume
df_by_trx_nom <- df_by_trx %>%
  select(1:74) %>%
  pivot_longer(-(1:2), names_to = "bulan", values_to = "nominal") %>%
  select(kelompok, bulan, everything())
df_by_trx_vol <- df_by_trx %>%
  select(1:2, 76:147) %>%
  pivot_longer(-(1:2), names_to = "bulan", values_to = "volume") %>%
  select(kelompok, bulan, everything())

# Menggabungkan data nominal dan volume
df_by_trx <- df_by_trx_nom %>%
  mutate(
    # Menyesuaikan tipe data
    kelompok = factor(kelompok),
    waktu = factor(waktu) %>% fct_relevel(">= 19:00", after = Inf),
    bulan = str_replace(bulan, "(\\.){1,3}\\d*", "") %>% as.yearmon(),
    
    # Menggabungkan dengan data volume
    volume = df_by_trx_vol$volume,
    
    # Mengganti NA dengan nol (0)
    volume = ifelse(!is.na(volume), volume, 0), 
    nominal = ifelse(!is.na(nominal), nominal, 0)
  ) %>% 
  # Mengatur urutan kolom dan baris
  select(kelompok, bulan, waktu, everything()) %>%
  arrange(kelompok, bulan, waktu)



# C. Export Data ----------------------------------------------------------

write_csv(df_by_trx, here("data/tidy/tidy_by_trx.csv"))

