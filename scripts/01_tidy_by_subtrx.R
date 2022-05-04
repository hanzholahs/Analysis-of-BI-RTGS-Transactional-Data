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

.raw_by_subtrx <- read_excel(.path, sheet = "Sub Jenis Transaksi", skip = 4)



# B. Tidy Data ------------------------------------------------------------

# Filter data
.del_subtrx <- which(.raw_by_subtrx[,1] == "Total"):nrow(.raw_by_subtrx)

df_by_subtrx <- .raw_by_subtrx  %>%
  rename(kelompok = ...1, waktu = ...2) %>%
  slice(-.del_subtrx) %>%
  tidyr::fill(kelompok) %>%
  filter(waktu != "0000", waktu != "All") %>% 
  mutate_at(vars(matches(month.abb)), as.numeric) # krn col 51 raw data bkn num

# Pivot data berdasarkan nominal dan volume
df_by_subtrx_nom <- df_by_subtrx %>%
  select(kelompok, waktu, 3:74) %>%
  pivot_longer(-(1:2), names_to = "bulan", values_to = "nominal") %>%
  select(kelompok, bulan, everything())
df_by_subtrx_vol <- df_by_subtrx %>%
  select(kelompok, waktu, 76:147) %>%
  pivot_longer(-(1:2), names_to = "bulan", values_to = "volume") %>%
  select(kelompok, bulan, everything())

# Menggabungkan data nominal dan volume
df_by_subtrx <- df_by_subtrx_nom %>%
  mutate(
    # Menyesuaikan tipe data
    kelompok = factor(kelompok),
    waktu = factor(waktu) %>% fct_relevel(">= 19:00", after = Inf),
    bulan = str_replace(bulan, "(\\.){1,3}\\d*", "") %>% as.yearmon(),
    
    # Menggabungkan dengan data volume
    volume = df_by_subtrx_vol$volume,
    
    # Mengganti NA dengan nol (0)
    volume = ifelse(!is.na(volume), volume, 0), 
    nominal = ifelse(!is.na(nominal), nominal, 0)
  ) %>% 
  # Mengatur urutan kolom dan baris
  select(kelompok, bulan, waktu, everything()) %>%
  arrange(kelompok, bulan, waktu)



# C. Export Data ----------------------------------------------------------

write_csv(df_by_subtrx, here("data/tidy/tidy_by_subtrx.csv"))

