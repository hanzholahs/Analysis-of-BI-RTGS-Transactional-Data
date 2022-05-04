# desc  : 
#   Analisis pola transaksi Sistem BI-RTGS pada periode Jan 2016 - Apr 2021
#   berdasarkan jenis kepesertaan dan jenis transaksi
#
# author: Hanzholah Shobri
# creation date : 2021-06-21







# Set Up ------------------------------------------------------------------

# a. Import Libraries -----------------------------------------------------

library(readxl)
library(here)
library(tidyverse)
library(scales)
library(lubridate)
library(cowplot)
library(zoo)
library(patchwork)
library(ggthemes)
library(ggrepel)





# b. Custom Functions -----------------------------------------------------

# Berdasarkan definisi PADG
fct_wkt2zn <- function(f) {
  fct_collapse(
    f, "Zona 1" = c(
      #"<05:30", "05:30 - <06:00", "06:00 - <06:30", 
      "06:30 - <07:00",
      "07:00 - <07:30", "07:30 - <08:00", "08:00 - <08:30",
      "08:30 - <09:00", "09:00 - <09:30", "09:30 - <10:00"
    ), "Zona 2" = c(
      "10:00 - <10:30", "10:30 - <11:00", "11:00 - <11:30",
      "11:30 - <12:00", "12:00 - <12:30", "12:30 - <13:00",
      "13:00 - <13:30", "13:30 - <14:00"
    ), "Zona 3" = c(
      "14:00 - <14:30", "14:30 - <15:00", "15:00 - <15:30",
      "15:30 - <16:00", "16:00 - <16:30", "16:30 - <17:00",
      "17:00 - <17:30", "17:30 - <18:00" #, "18:00 - <18:30",
      #"18:30 - <19:00", ">= 19:00" 
    )
  )
}



# Menghitung semua transaksi tersedia
fct_wkt2zn_all <- function(f) {
  fct_collapse(
    f, "Zona 1" = c(
      "<05:30", "05:30 - <06:00", "06:00 - <06:30", 
      "06:30 - <07:00",
      "07:00 - <07:30", "07:30 - <08:00", "08:00 - <08:30",
      "08:30 - <09:00", "09:00 - <09:30", "09:30 - <10:00"
    ), "Zona 2" = c(
      "10:00 - <10:30", "10:30 - <11:00", "11:00 - <11:30",
      "11:30 - <12:00", "12:00 - <12:30", "12:30 - <13:00",
      "13:00 - <13:30", "13:30 - <14:00"
    ), "Zona 3" = c(
      "14:00 - <14:30", "14:30 - <15:00", "15:00 - <15:30",
      "15:30 - <16:00", "16:00 - <16:30", "16:30 - <17:00",
      "17:00 - <17:30", "17:30 - <18:00", "18:00 - <18:30",
      "18:30 - <19:00", ">= 19:00" 
    )
  )
}



# mengganti batas waktu Zona 1
fct_wkt2zn_mod <- function(f) { 
  fct_collapse(
    f, "Zona 1" = c(
      "06:30 - <07:00",
      "07:00 - <07:30", "07:30 - <08:00", "08:00 - <08:30",
      "08:30 - <09:00", "09:00 - <09:30", "09:30 - <10:00",
      "10:00 - <10:30", "10:30 - <11:00"
    ), "Zona 2" = c(
      "11:00 - <11:30",
      "11:30 - <12:00", "12:00 - <12:30", "12:30 - <13:00",
      "13:00 - <13:30", "13:30 - <14:00"
    ), "Zona 3" = c(
      "14:00 - <14:30", "14:30 - <15:00", "15:00 - <15:30",
      "15:30 - <16:00", "16:00 - <16:30", "16:30 - <17:00",
      "17:00 - <17:30", "17:30 - <18:00"
    )
  )
}









# Import and Tidy Data ----------------------------------------------------

# a. Import Data ----------------------------------------------------------

# Import raw data
.path_buk <- here("data/raw_update/",
                  "Data Throughput BUKU Jenis Transaksi R1.xlsx")
.path_kel <- here("data/raw_update/",
                  "Data Throughput Kelompok Bank Jenis Transaksi R2.xlsx")

excel_sheets(.path_buk) # lihat sheet2 yang ada di workbook
excel_sheets(.path_kel)

.raw_buk <- read_excel(.path_buk, sheet = "Page1_1", skip = 4)
.raw_kel <- read_excel(.path_kel, sheet = "Page1_1", skip = 4)





# b. Tidy Data By Buku ----------------------------------------------------

# Membuat df data transaksi berdasarkan kelompok buku
df_buk <- .raw_buk %>%
  rename(kelompok = ...1, transaksi = ...2, waktu = ...3) %>%
  mutate_at(vars(matches(month.abb)), as.numeric) %>% 
  tidyr::fill(kelompok, transaksi) %>%
  slice(-nrow(.)) %>% 
  filter(kelompok != "Total",
         transaksi != "All",
         waktu != "0000", 
         waktu != "All")



# Pivot data berdasarkan nominal dan volume
df_buk_nom <- df_buk %>%
  select(1:75) %>% 
  pivot_longer(-(1:3), names_to = "bulan", values_to = "nominal") 
df_buk_vol <- df_buk %>%
  select(1:3, 77:148) %>%
  pivot_longer(-(1:3), names_to = "bulan", values_to = "volume")



# Menggabungkan data dan cleaning
df_buk <- df_buk_nom %>%
  mutate(
    bulan = str_replace(bulan, "(\\.){1,3}\\d*", "") %>% 
      as.yearmon(),
    kelompok = factor(kelompok),
    transaksi = factor(transaksi),
    waktu = factor(waktu) %>%
      fct_relevel(">= 19:00", after = Inf),
    volume = df_buk_vol$volume,
    volume = ifelse(!is.na(volume), volume, 0),
    nominal = ifelse(!is.na(nominal), nominal, 0)
  ) %>% 
  select(kelompok, transaksi, bulan, waktu, everything()) %>%
  arrange(kelompok, transaksi, bulan, waktu)



# Menghapus data nominal dan volume
rm(df_buk_nom)
rm(df_buk_vol)





# c. Tidy Data By Kelompok ------------------------------------------------

# Membuat df data transaksi berdasarkan kelompok kepesertaan
df_kel <- .raw_kel %>%
  rename(kelompok = ...1, transaksi = ...2, waktu = ...3) %>%
  mutate_at(vars(matches(month.abb)), as.numeric) %>% 
  tidyr::fill(kelompok, transaksi) %>%
  slice(-nrow(.)) %>% 
  filter(kelompok != "All",
         transaksi != "All",
         waktu != "0000", 
         waktu != "All")



# Pivot data berdasarkan nominal dan volume
df_kel_nom <- df_kel %>%
  select(1:75) %>% 
  pivot_longer(-(1:3), names_to = "bulan", values_to = "nominal") 
df_kel_vol <- df_kel %>%
  select(1:3, 77:148) %>%
  pivot_longer(-(1:3), names_to = "bulan", values_to = "volume")



# Menggabungkan data dan cleaning
df_kel <- df_kel_nom %>%
  mutate(
    bulan = str_replace(bulan, "(\\.){1,3}\\d*", "") %>% 
      as.yearmon(),
    kelompok = factor(kelompok) %>%
      fct_recode(`Bank Swasta` = "Bank Swasta Nasional",
                 `Other FI` = "Other Financial Institutions"),
    transaksi = factor(transaksi),
    waktu = factor(waktu) %>%
      fct_relevel(">= 19:00", after = Inf),
    volume = df_kel_vol$volume,
    volume = ifelse(!is.na(volume), volume, 0),
    nominal = ifelse(!is.na(nominal), nominal, 0)
  ) %>% 
  select(kelompok, transaksi, bulan, waktu, everything()) %>%
  arrange(kelompok, transaksi, bulan, waktu) %>%
  group_by(kelompok, transaksi, bulan, waktu) %>% 
  summarise(nominal = sum(nominal),    # kalkulasi untuk merger col 'Others'
            volume = sum(volume)) %>%  # karena pada data raw, ada dua
  ungroup()



# Menghapus data nominal dan volume
rm(df_kel_nom)
rm(df_kel_vol)





# d. Export Data ----------------------------------------------------------

write_csv(df_buk, here("data/tidy/tidy_by_buk_trx.csv"))
write_csv(df_kel, here("data/tidy/tidy_by_kel_trx.csv"))










# Data Analysis -----------------------------------------------------------

# a. Exploratory Awal -----------------------------------------------------

table(df_kel$kelompok)
table(df_buk$kelompok)
table(df_buk$transaksi)
table(df_kel$transaksi)



# Persentase transaksi per kelompok buku
df_buk %>% 
  group_by(kelompok) %>% 
  summarise(nominal = sum(nominal),
            volume = sum(volume)) %>% 
  mutate(nom_prop = nominal/sum(nominal),
         vol_prop = volume/sum(volume)) 



# Persentase transaksi per kelompok kepesertaan
df_kel %>% 
  group_by(kelompok) %>% 
  summarise(nominal = sum(nominal),
            volume = sum(volume)) %>% 
  mutate(nom_prop = nominal/sum(nominal),
         vol_prop = volume/sum(volume)) 



# Persentase transaksi per jenis transaksi
df_kel %>% 
  group_by(transaksi) %>% 
  summarise(nominal = sum(nominal),
            volume = sum(volume)) %>% 
  mutate(nom_prop = nominal/sum(nominal),
         vol_prop = volume/sum(volume)) 



# Persentase transaksi tanpa "Bank Sentral" dan "Others"
df_kel %>% 
  filter(kelompok != "Bank Sentral", 
         kelompok != "Others") %>% 
  group_by(transaksi) %>% 
  summarise(nominal = sum(nominal),
            volume = sum(volume)) %>% 
  mutate(nom_prop = nominal/sum(nominal),
         vol_prop = volume/sum(volume)) 



# Persebaran waktu per jenis transaksi tanpa "Bank Sentral" dan "Others"
p_jenis <- df_kel %>% 
  filter(!(transaksi %in% c("FPJP", "N/A")),
         !(kelompok %in% c("Bank Sentral", "Others"))) %>% 
  group_by(transaksi, bulan, waktu) %>% 
  summarise(nominal = sum(nominal)) %>% 
  ungroup() %>% 
  ggplot(aes(x = waktu, y = nominal)) +
  geom_col(fill = "royalblue3") +
  scale_x_discrete(
    labels = c(str_extract(unique(df_kel$waktu), "[^-\\s]{6}$")[1:28],
               ">=19:00")[seq(1, 29, 2)],
    breaks = unique(df_kel$waktu)[seq(1, 29, 2)]
  ) +
  scale_y_continuous(labels = unit_format(unit = "", scale = 1e-12)) +
  facet_wrap(~fct_relevel(transaksi, "Lainnya", after = Inf), 
             scales = "free_y", nrow = 4) +
  labs(title = "Pola Transaksi Peserta RTGS (Jan 2016 - Apr 2021)", 
       x = "Waktu Penyelesaian", y = "Nominal (dalam Triliun)") +
  theme_tufte(base_size = 14) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        strip.text = element_text(face = "bold"),
        panel.grid.major.y = element_line(colour = "grey80",
                                          linetype = 2))
p_jenis
ggsave(here("figures/result/jenis_trx.png"), p_jenis, 
       width = 12, height = 7.5)

#
#   NOTES:
#
# Per kelompok kepesertaan, jenis peserta yang tidak memiliki transaksi:
# - Central securities depositories
# - Custodian
# - Government Entities
#
# Per jenis transaksi, jenis transaksi yang tidak dilakukan:
# - N/A
# - FPJP
#
# Hal ini tidak diperhitungkan
#





# b. Set up ---------------------------------------------------------------

# Kelompok yang akan dianalisis (excl. Bank Sentral dan Others)
KELOMPOK <- c("Bank Asing", "Bank Campuran", "Bank Pemerintah", "Bank Swasta", 
              "Bank Syariah", "BPD", "Other FI")



# Range tanggal analisis (Jan 2016 - Apr 2021)
MIN_DATE <- as.yearmon("2016-01")
MAX_DATE <- as.yearmon("2021-04")
ZONA <- c("Zona 1", "Zona 2", "Zona 3")



# variabel pendukung lainnya
ANNOTATION_DATE <- (as.Date(MAX_DATE) - months(4)) %>% as.yearmon
WAKTU_LEVELS <- filter(df_kel, fct_wkt2zn(waktu) %in% ZONA) %>% 
  pull(waktu) %>% unique() %>% fct_drop()





# c. Analisis Throughput Skenario 1 (Incl. All) ---------------------------

# Jenis transaksi Skenario 1
dat_A_type_trx <- c("Nasabah", "PUAB", "Pasar Modal", "FLI", "Pem-IFTSA", 
                    "Pem-xIFTSA", "Valas", "Valas BI", "SKNBI", 
                    "Operasi Moneter", "Lainnya")



# Persiapan Data
dat_A_kum <- df_kel %>% 
  mutate(zona = fct_wkt2zn(waktu)) %>%
  filter(zona %in% ZONA,
         kelompok %in% KELOMPOK,
         bulan <= MAX_DATE,
         transaksi %in% dat_A_type_trx) %>%
  group_by(bulan, zona) %>%
  summarise(volume = sum(volume),
            nominal = sum(nominal)) %>% 
  ungroup()

dat_A_kel <- df_kel %>% 
  mutate(zona = fct_wkt2zn(waktu)) %>%
  filter(zona %in% ZONA,
         kelompok %in% KELOMPOK,
         bulan <= MAX_DATE,
         transaksi %in% dat_A_type_trx) %>%
  group_by(kelompok, bulan, zona) %>%
  summarise(volume = sum(volume),
            nominal = sum(nominal)) %>% 
  ungroup()

dat_A_buk <- df_buk %>%
  mutate(zona = fct_wkt2zn(waktu)) %>%
  filter(zona %in% ZONA,
         bulan <= MAX_DATE,
         transaksi %in% dat_A_type_trx) %>%
  group_by(bulan, zona, kelompok) %>%
  summarise(volume = sum(volume),
            nominal = sum(nominal)) %>% 
  ungroup()
dat_A_jam <- df_kel %>% 
  filter(bulan <= MAX_DATE,
         fct_wkt2zn(waktu) %in% ZONA,
         kelompok %in% KELOMPOK,
         transaksi %in% dat_A_type_trx) %>% 
  mutate(waktu = fct_drop(waktu)) %>% 
  group_by(waktu) %>% 
  summarise(nominal = sum(nominal)) 



# Throughput Peserta secara Aggregat
p1a <- ggplot(dat_A_kum, aes(x = bulan, y = nominal, fill = fct_rev(zona))) +
  geom_area(position = position_fill()) +
  annotate("segment", y = 0.3, yend = 0.3,
           x = MIN_DATE, xend = MAX_DATE, 
           colour = "grey30", size = 1, alpha = .7) +
  annotate("segment", y = 0.6, yend = 0.6,
           x = MIN_DATE, xend = MAX_DATE, 
           colour = "grey30", size = 1, alpha = .7) +
  annotate("text", label = "30%", fontface = "bold", 
           x = ANNOTATION_DATE, y = 0.35) +
  annotate("text", label = "60%", fontface = "bold", 
           x = ANNOTATION_DATE, y = 0.65) +
  scale_x_yearmon(breaks = as.yearmon(seq(as.Date(MIN_DATE), 
                                          as.Date(MAX_DATE), 
                                          "2 month")),
                  expand = c(0, 0)) +
  scale_y_continuous(
    breaks = seq(0,1,0.2),
    labels = scales::percent_format(),
    expand = c(0, 0)
  ) +
  scale_fill_brewer(palette = "Oranges") +
  labs(title = "Pola Transaksi Peserta BI-RTGS (Jan 2016 - Apr 2021)", 
       fill = "Zona Throughput") +
  theme_tufte(base_size = 16) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 60, hjust = 1),
        axis.ticks = element_line(colour = "grey50", size = 0.7))
p1a



# Throughput Peserta Per Kelompok kepesertaan
p1b <- ggplot(dat_A_kel, aes(x = (bulan), y = nominal, fill = fct_rev(zona))) +
  geom_area(position = position_fill()) +
  annotate("segment", y = 0.3, yend = 0.3,
           x = MIN_DATE, xend = MAX_DATE, 
           colour = "grey30", size = 1, alpha = .7) +
  annotate("segment", y = 0.6, yend = 0.6,
           x = MIN_DATE, xend = MAX_DATE, 
           colour = "grey30", size = 1, alpha = .7) +
  annotate("text", label = "30%", fontface = "bold", 
           x = ANNOTATION_DATE, y = 0.35) +
  annotate("text", label = "60%", fontface = "bold", 
           x = ANNOTATION_DATE, y = 0.65) +
  facet_wrap(kelompok~., ncol = 3) +
  scale_x_yearmon(breaks = as.yearmon(seq(as.Date(MIN_DATE), 
                                          as.Date(MAX_DATE), 
                                          "4 month")),
                  expand = c(0, 0)) +
  scale_y_continuous(
    breaks = seq(0,1,0.2),
    labels = scales::percent_format(),
    expand = c(0, 0)
  ) +
  scale_fill_brewer(palette = "Oranges") +
  labs(title = "Pola Transaksi Peserta BI-RTGS (Jan 2016 - Apr 2021)", 
       fill = "Zona Throughput") +
  theme_tufte(base_size = 11) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 60, hjust = 1),
        axis.ticks = element_line(colour = "grey50", size = 0.7))
p1b



# Throughput Peserta Per Kelompok Buku
p1c <- ggplot(dat_A_buk, aes(x = bulan, y = nominal, fill = fct_rev(zona))) +
  geom_area(position = position_fill()) +
  annotate("segment", y = 0.3, yend = 0.3,
           x = MIN_DATE, xend = MAX_DATE, 
           colour = "grey30", size = 1, alpha = .7) +
  annotate("segment", y = 0.6, yend = 0.6,
           x = MIN_DATE, xend = MAX_DATE, 
           colour = "grey30", size = 1, alpha = .7) +
  annotate("text", label = "30%", fontface = "bold", 
           x = ANNOTATION_DATE, y = 0.35) +
  annotate("text", label = "60%", fontface = "bold", 
           x = ANNOTATION_DATE, y = 0.65) +
  facet_wrap(kelompok~.) +
  scale_x_yearmon(breaks = as.yearmon(seq(as.Date(MIN_DATE), 
                                          as.Date(MAX_DATE), 
                                          "3 month")),
                  expand = c(0, 0)) +
  scale_y_continuous(
    breaks = seq(0,1,0.2),
    labels = scales::percent_format(),
    expand = c(0, 0)
  ) +
  scale_fill_brewer(palette = "Oranges") +
  labs(title = "Pola Transaksi Peserta BI-RTGS (Jan 2016 - Apr 2021)", 
       fill = "Zona Throughput") +
  theme_tufte(base_size = 12) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 60, hjust = 1),
        axis.ticks = element_line(colour = "grey50", size = 0.7))
p1c



# Pola Transaksi Harian Peserta
dat_A_jam_scaleY <- 5e15  
dat_A_jam_max_nom <- ceiling(max(dat_A_jam$nominal) / 
                               dat_A_jam_scaleY) * dat_A_jam_scaleY
dat_A_jam <- dat_A_jam %>% 
  mutate(kumulatif = cumsum(nominal),
         # nilai kumulatif yang disesuaikan utk visualisasi
         kumulatif = kumulatif * dat_A_jam_max_nom / max(kumulatif)) 

dat_A_jam_nom <- dat_A_jam %>% 
  group_by(zona = fct_wkt2zn(waktu)) %>% 
  summarise(nominal = sum(nominal)) %>% 
  mutate(nominal = nominal/sum(nominal)) %>% 
  pull(nominal) %>% 
  scales::percent()

dat_A_jam_maxY <- dat_A_jam_max_nom
dat_A_jam_breaksY <- seq(0, dat_A_jam_maxY, length = 11)
dat_A_jam_annotationY <- dat_A_jam_maxY 
dat_A_jam_annotationX <- c(
  0.5 + 7.5/2,
  7.5 + (15.5-7.5)/2,
  15.5 + (23.5-15.5)/2
)

p1d <- ggplot(dat_A_jam, aes(x = waktu)) +
  annotate(geom = "segment", linetype = 2, size = 1, colour = "grey50",
           x = 7.5, xend = 7.5, y = 0, yend = dat_A_jam_maxY) +
  annotate(geom = "segment", linetype = 2, size = 1, colour = "grey50",
           x = 15.5, xend = 15.5, y = 0, yend = dat_A_jam_maxY) +
  annotate("text", label = paste0("Zona 1\n", dat_A_jam_nom[1]),
           fontface = "bold", x = dat_A_jam_annotationX[1],
           y = dat_A_jam_annotationY) +
  annotate("text", label = paste0("Zona 2\n", dat_A_jam_nom[2]),
           fontface = "bold", x = dat_A_jam_annotationX[2],
           y = dat_A_jam_annotationY) +
  annotate("text", label = paste0("Zona 3\n", dat_A_jam_nom[3]),
           fontface = "bold", x = dat_A_jam_annotationX[3],
           y = dat_A_jam_annotationY) +
  geom_col(aes(y = nominal), fill = "grey30") +
  geom_line(aes(y = kumulatif, group = 1), size = 1.5, alpha = 0.7, 
            colour = "red") +
  scale_x_discrete(breaks = WAKTU_LEVELS) +
  scale_y_continuous(
    limits = c(0, dat_A_jam_maxY + dat_A_jam_scaleY),
    breaks = dat_A_jam_breaksY,
    labels = scales::unit_format(unit = "", scale = 1e-12),
    expand = c(0,0),
    sec.axis = sec_axis(~.,
                        name = "Persentase Nominal Transaksi Kumulatif",
                        breaks = dat_A_jam_breaksY,
                        labels = scales::percent(0:10/10))) +
  labs(
    title = "Throughput Peserta RTGS (Periode Jan 2016 - Apr 2021)",
    subtitle = paste("Jenis Transaksi:", 
                     paste(dat_A_type_trx, collapse = ", ")),
    y = "Nominal Transaksi (dalam Triliun)",
    x = "Waktu Transaksi"
  ) +
  theme_tufte(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )
p1d



# Save Output Grafik
ggsave(here("figures/result/kum_1.png"), p1a, width = 12, height = 6)
ggsave(here("figures/result/kel_1.png"), p1b, width = 12, height = 9)
ggsave(here("figures/result/buk_1.png"), p1c, width = 12, height = 7.5)
ggsave(here("figures/result/jam_1.png"), p1d, width = 12, height = 6)



# Save Output Tabel
dat_A_kel %>% 
  group_by(kelompok, zona) %>% 
  summarise(nominal = sum(nominal)) %>%
  mutate(prop = nominal / sum(nominal),
         prop = scales::percent(prop, accuracy = 0.01)) %>%
  select(kelompok, zona, prop) %>% 
  write_csv(here("data/result/kel_1.csv"))
dat_A_buk %>% 
  group_by(kelompok, zona) %>% 
  summarise(nominal = sum(nominal)) %>%
  mutate(prop = nominal / sum(nominal),
         prop = scales::percent(prop, accuracy = 0.01)) %>%
  select(kelompok, zona, prop) %>% 
  write_csv(here("data/result/buk_1.csv"))
dat_A_kum %>% 
  group_by(zona) %>% 
  summarise(nominal = sum(nominal)) %>% 
  mutate(prop = nominal / sum(nominal),
         prop = scales::percent(prop, accuracy = 0.01)) %>%
  select(zona, prop) %>% 
  write_csv(here("data/result/kum_zn_1.csv"))
dat_A_jam %>% 
  mutate(cum_nom = cumsum(nominal),
         prop = cum_nom / sum(nominal),
         prop = scales::percent(prop, accuracy = 0.01)) %>%
  select(waktu, prop) %>% 
  write_csv(here("data/result/kum_jam_1.csv"))





# d. Analisis Throughput Skenario 2 (Excl. OM-Pem) ------------------------

# Jenis transaksi Skenario 2
dat_B_type_trx <- c("Nasabah", "PUAB", "Pasar Modal","FLI", "SKNBI", "Valas", 
                    "Valas BI", "Lainnya")



# Persiapan Data
dat_B_kum <- df_kel %>% 
  mutate(zona = fct_wkt2zn(waktu)) %>%
  filter(zona %in% c("Zona 1", "Zona 2", "Zona 3"),
         kelompok %in% KELOMPOK,
         transaksi %in% dat_B_type_trx,
         bulan <= MAX_DATE) %>%
  group_by(bulan, zona) %>%
  summarise(volume = sum(volume),
            nominal = sum(nominal)) %>% 
  ungroup()
  
dat_B_kel <- df_kel %>% 
  mutate(zona = fct_wkt2zn(waktu)) %>%
  filter(zona %in% c("Zona 1", "Zona 2", "Zona 3"),
         kelompok %in% KELOMPOK,
         transaksi %in% dat_B_type_trx,
         bulan <= MAX_DATE) %>%
  group_by(kelompok, bulan, zona) %>%
  summarise(volume = sum(volume),
            nominal = sum(nominal)) %>% 
  ungroup()

dat_B_buk <- df_buk %>% 
  mutate(zona = fct_wkt2zn(waktu)) %>%
  filter(zona %in% c("Zona 1", "Zona 2", "Zona 3"),
         transaksi %in% dat_B_type_trx,
         bulan <= MAX_DATE) %>%
  group_by(kelompok, bulan, zona) %>%
  summarise(volume = sum(volume),
            nominal = sum(nominal)) %>% 
  ungroup()



# Throughput Peserta secara Aggregat
p2a <- ggplot(dat_B_kum, aes(x = bulan, y = nominal, fill = fct_rev(zona))) +
  geom_area(position = position_fill()) +
  annotate("segment", y = 0.3, yend = 0.3,
           x = MIN_DATE, xend = MAX_DATE, 
           colour = "grey30", size = 1, alpha = .7) +
  annotate("segment", y = 0.6, yend = 0.6,
           x = MIN_DATE, xend = MAX_DATE, 
           colour = "grey30", size = 1, alpha = .7) +
  annotate("text", label = "30%", fontface = "bold", 
           x = ANNOTATION_DATE, y = 0.35) +
  annotate("text", label = "60%", fontface = "bold", 
           x = ANNOTATION_DATE, y = 0.65) +
  scale_x_yearmon(breaks = as.yearmon(seq(as.Date(MIN_DATE), 
                                          as.Date(MAX_DATE), 
                                          "2 month")),
                  expand = c(0, 0)) +
  scale_y_continuous(
    breaks = seq(0,1,0.2),
    labels = scales::percent_format(),
    expand = c(0, 0)
  ) +
  scale_fill_brewer(palette = "Greens") +
  labs(title = "Pola Transaksi Peserta BI-RTGS (Jan 2016 - Apr 2021)", 
       fill = "Zona Throughput") +
  theme_tufte(base_size = 16) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 60, hjust = 1),
        axis.ticks = element_line(colour = "grey50", size = 0.7))
p2a



# Throughput Peserta Per Kelompok Kepesertaan
p2b <- ggplot(dat_B_kel, aes(x = (bulan), y = nominal, fill = fct_rev(zona))) +
  geom_area(position = position_fill()) +
  annotate("segment", y = 0.3, yend = 0.3,
           x = MIN_DATE, xend = MAX_DATE, 
           colour = "grey30", size = 1, alpha = .7) +
  annotate("segment", y = 0.6, yend = 0.6,
           x = MIN_DATE, xend = MAX_DATE, 
           colour = "grey30", size = 1, alpha = .7) +
  annotate("text", label = "30%", fontface = "bold", 
           x = ANNOTATION_DATE, y = 0.35) +
  annotate("text", label = "60%", fontface = "bold", 
           x = ANNOTATION_DATE, y = 0.65) +
  facet_wrap(kelompok~., ncol = 3) +
  scale_x_yearmon(breaks = as.yearmon(seq(as.Date(MIN_DATE), 
                                          as.Date(MAX_DATE), 
                                          "4 month")),
                  expand = c(0, 0)) +
  scale_y_continuous(
    breaks = seq(0,1,0.2),
    labels = scales::percent_format(),
    expand = c(0, 0)
  ) +
  scale_fill_brewer(palette = "Greens") +
  labs(title = "Pola Transaksi Peserta BI-RTGS (Jan 2016 - Apr 2021)", 
       fill = "Zona Throughput") +
  theme_tufte(base_size = 11) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 60, hjust = 1),
        axis.ticks = element_line(colour = "grey50", size = 0.7))
p2b



# Throughput Peserta Per Kelompok Buku
p2c <- ggplot(dat_B_buk, aes(x = bulan, y = nominal, fill = fct_rev(zona))) +
  geom_area(position = position_fill()) +
  annotate("segment", y = 0.3, yend = 0.3,
           x = MIN_DATE, xend = MAX_DATE, 
           colour = "grey30", size = 1, alpha = .7) +
  annotate("segment", y = 0.6, yend = 0.6,
           x = MIN_DATE, xend = MAX_DATE, 
           colour = "grey30", size = 1, alpha = .7) +
  annotate("text", label = "30%", fontface = "bold", 
           x = ANNOTATION_DATE, y = 0.35) +
  annotate("text", label = "60%", fontface = "bold", 
           x = ANNOTATION_DATE, y = 0.65) +
  facet_wrap(kelompok~.) +
  scale_x_yearmon(breaks = as.yearmon(seq(as.Date(MIN_DATE), 
                                          as.Date(MAX_DATE), 
                                          "3 month")),
                  expand = c(0, 0)) +
  scale_y_continuous(
    breaks = seq(0,1,0.2),
    labels = scales::percent_format(),
    expand = c(0, 0)
  ) +
  scale_fill_brewer(palette = "Greens") +
  labs(title = "Pola Transaksi Peserta BI-RTGS (Jan 2016 - Apr 2021)", 
       fill = "Zona Throughput") +
  theme_tufte(base_size = 12) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 60, hjust = 1),
        axis.ticks = element_line(colour = "grey50", size = 0.7))
p2c



# Pola Transaksi Harian Peserta
dat_B_jam <- df_kel %>% 
  filter(bulan <= MAX_DATE,
         fct_wkt2zn(waktu) %in% ZONA,
         kelompok %in% KELOMPOK,
         transaksi %in% dat_B_type_trx) %>% 
  mutate(waktu = fct_drop(waktu)) %>% 
  group_by(waktu) %>% 
  summarise(nominal = sum(nominal)) 

dat_B_jam_scaleY <- 5e15  # kurang-lebih = nominal max dibagi 10
dat_B_jam_max_nom <- ceiling(max(dat_B_jam$nominal) / 
                               dat_B_jam_scaleY) * dat_B_jam_scaleY
dat_B_jam <- dat_B_jam %>% 
  mutate(kumulatif = cumsum(nominal),
         # nilai kumulatif yang disesuaikan utk visualisasi
         kumulatif = kumulatif * dat_B_jam_max_nom / max(kumulatif)) 

dat_B_jam_nom <- dat_B_jam %>% 
  group_by(zona = fct_wkt2zn(waktu)) %>% 
  summarise(nominal = sum(nominal)) %>% 
  mutate(nominal = nominal/sum(nominal)) %>% 
  pull(nominal) %>% 
  scales::percent()

dat_B_jam_maxY <- dat_B_jam_max_nom
dat_B_jam_breaksY <- seq(0, dat_B_jam_maxY, length = 11)
dat_B_jam_annotationY <- dat_B_jam_maxY 
dat_B_jam_annotationX <- c(
  0.5 + 7.5/2,
  7.5 + (15.5-7.5)/2,
  15.5 + (23.5-15.5)/2
)

p2d <- ggplot(dat_B_jam, aes(x = waktu)) +
  annotate(geom = "segment", linetype = 2, size = 1, colour = "grey50",
           x = 7.5, xend = 7.5, y = 0, yend = dat_B_jam_maxY) +
  annotate(geom = "segment", linetype = 2, size = 1, colour = "grey50",
           x = 15.5, xend = 15.5, y = 0, yend = dat_B_jam_maxY) +
  annotate("text", label = paste0("Zona 1\n", dat_B_jam_nom[1]),
           fontface = "bold", x = dat_B_jam_annotationX[1],
           y = dat_B_jam_annotationY) +
  annotate("text", label = paste0("Zona 2\n", dat_B_jam_nom[2]),
           fontface = "bold", x = dat_B_jam_annotationX[2],
           y = dat_B_jam_annotationY) +
  annotate("text", label = paste0("Zona 3\n", dat_B_jam_nom[3]),
           fontface = "bold", x = dat_B_jam_annotationX[3],
           y = dat_B_jam_annotationY) +
  geom_col(aes(y = nominal), fill = "grey30") +
  geom_line(aes(y = kumulatif, group = 1), size = 1.5, alpha = 0.7, 
            colour = "red") +
  scale_x_discrete(breaks = WAKTU_LEVELS) +
  scale_y_continuous(
    limits = c(0, dat_B_jam_maxY + dat_B_jam_scaleY),
    breaks = dat_B_jam_breaksY,
    labels = scales::unit_format(unit = "", scale = 1e-12),
    expand = c(0,0),
    sec.axis = sec_axis(~.,
                        name = "Persentase Nominal Transaksi Kumulatif",
                        breaks = dat_B_jam_breaksY,
                        labels = scales::percent(0:10/10))) +
  labs(
    title = "Throughput Peserta RTGS (Periode Jan 2016 - Apr 2021)",
    subtitle = paste("Jenis Transaksi:", 
                     paste(dat_B_type_trx, collapse = ", ")),
    y = "Nominal Transaksi (dalam Triliun)",
    x = "Waktu Transaksi"
  ) +
  theme_tufte(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )
p2d



# Save Output Grafik
ggsave(here("figures/result/kum_2.png"), p2a, width = 12, height = 6)
ggsave(here("figures/result/kel_2.png"), p2b, width = 12, height = 9)
ggsave(here("figures/result/buk_2.png"), p2c, width = 12, height = 7.5)
ggsave(here("figures/result/jam_2.png"), p2d, width = 12, height = 6)



# Save Output Tabel
dat_B_kel %>% 
  group_by(kelompok, zona) %>% 
  summarise(nominal = sum(nominal)) %>%
  mutate(prop = nominal / sum(nominal),
         prop = scales::percent(prop, accuracy = 0.01)) %>%
  select(kelompok, zona, prop) %>% 
  write_csv(here("data/result/kel_2.csv"))
dat_B_buk %>% 
  group_by(kelompok, zona) %>% 
  summarise(nominal = sum(nominal)) %>%
  mutate(prop = nominal / sum(nominal),
         prop = scales::percent(prop, accuracy = 0.01)) %>%
  select(kelompok, zona, prop) %>% 
  write_csv(here("data/result/buk_2.csv"))
dat_B_kum %>% 
  group_by(zona) %>% 
  summarise(nominal = sum(nominal)) %>% 
  mutate(prop = nominal / sum(nominal),
         prop = scales::percent(prop, accuracy = 0.01)) %>%
  select(zona, prop) %>% 
  write_csv(here("data/result/kum_zn_2.csv"))
dat_B_jam %>% 
  mutate(cum_nom = cumsum(nominal),
         prop = cum_nom / sum(nominal),
         prop = scales::percent(prop, accuracy = 0.01)) %>%
  select(waktu, prop) %>% 
  write_csv(here("data/result/kum_jam_2.csv"))





# e. Analisis Throughput Skenario 3 (Justifikasi Khusus) ------------------

# Jenis transaksi skenario 3
# ket: mengeluarkan OM, SKNBI, dan Valas BI
dat_C_type_trx <- c("Nasabah", "PUAB", "Pasar Modal", "FLI", "Pem-IFTSA",
                    "Pem-xIFTSA", "Valas", "Valas BI", "Lainnya")



# Persiapan Data
dat_C_kum <- df_kel %>% 
  mutate(zona = fct_wkt2zn(waktu)) %>%
  filter(zona %in% c("Zona 1", "Zona 2", "Zona 3"),
         kelompok %in% KELOMPOK,
         transaksi %in% dat_C_type_trx,
         bulan <= MAX_DATE) %>%
  group_by(bulan, zona) %>%
  summarise(volume = sum(volume),
            nominal = sum(nominal)) %>% 
  ungroup()

dat_C_kel <- df_kel %>% 
  mutate(zona = fct_wkt2zn(waktu)) %>%
  filter(zona %in% c("Zona 1", "Zona 2", "Zona 3"),
         kelompok %in% KELOMPOK,
         transaksi %in% dat_C_type_trx,
         bulan <= MAX_DATE) %>%
  group_by(kelompok, bulan, zona) %>%
  summarise(volume = sum(volume),
            nominal = sum(nominal)) %>% 
  ungroup()

dat_C_buk <- df_buk %>% 
  mutate(zona = fct_wkt2zn(waktu)) %>%
  filter(zona %in% c("Zona 1", "Zona 2", "Zona 3"),
         transaksi %in% dat_C_type_trx,
         bulan <= MAX_DATE) %>%
  group_by(kelompok, bulan, zona) %>%
  summarise(volume = sum(volume),
            nominal = sum(nominal)) %>% 
  ungroup()

dat_C_jam <- df_kel %>% 
  filter(bulan <= MAX_DATE,
         fct_wkt2zn(waktu) %in% ZONA,
         kelompok %in% KELOMPOK,
         transaksi %in% dat_C_type_trx) %>% 
  mutate(waktu = fct_drop(waktu)) %>% 
  group_by(waktu) %>% 
  summarise(nominal = sum(nominal)) 



# Throughput Peserta secara Aggregat
p3a <- ggplot(dat_C_kum, aes(x = bulan, y = nominal, fill = fct_rev(zona))) +
  geom_area(position = position_fill()) +
  annotate("segment", y = 0.3, yend = 0.3,
           x = MIN_DATE, xend = MAX_DATE, 
           colour = "grey30", size = 1, alpha = .7) +
  annotate("segment", y = 0.6, yend = 0.6,
           x = MIN_DATE, xend = MAX_DATE, 
           colour = "grey30", size = 1, alpha = .7) +
  annotate("text", label = "30%", fontface = "bold", 
           x = ANNOTATION_DATE, y = 0.35) +
  annotate("text", label = "60%", fontface = "bold", 
           x = ANNOTATION_DATE, y = 0.65) +
  scale_x_yearmon(breaks = as.yearmon(seq(as.Date(MIN_DATE), 
                                          as.Date(MAX_DATE), 
                                          "2 month")),
                  expand = c(0, 0)) +
  scale_y_continuous(
    breaks = seq(0,1,0.2),
    labels = scales::percent_format(),
    expand = c(0, 0)
  ) +
  scale_fill_brewer(palette = "Purples") +
  labs(title = "Pola Transaksi Peserta BI-RTGS (Jan 2016 - Apr 2021)", 
       fill = "Zona Throughput") +
  theme_tufte(base_size = 16) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 60, hjust = 1),
        axis.ticks = element_line(colour = "grey50", size = 0.7))
p3a



# Throughput Peserta Per Kelompok Kepesertaan
p3b <- ggplot(dat_C_kel, aes(x = (bulan), y = nominal, fill = fct_rev(zona))) +
  geom_area(position = position_fill()) +
  annotate("segment", y = 0.3, yend = 0.3,
           x = MIN_DATE, xend = MAX_DATE, 
           colour = "grey30", size = 1, alpha = .7) +
  annotate("segment", y = 0.6, yend = 0.6,
           x = MIN_DATE, xend = MAX_DATE, 
           colour = "grey30", size = 1, alpha = .7) +
  annotate("text", label = "30%", fontface = "bold", 
           x = ANNOTATION_DATE, y = 0.35) +
  annotate("text", label = "60%", fontface = "bold", 
           x = ANNOTATION_DATE, y = 0.65) +
  facet_wrap(kelompok~., ncol = 3) +
  scale_x_yearmon(breaks = as.yearmon(seq(as.Date(MIN_DATE), 
                                          as.Date(MAX_DATE), 
                                          "4 month")),
                  expand = c(0, 0)) +
  scale_y_continuous(
    breaks = seq(0,1,0.2),
    labels = scales::percent_format(),
    expand = c(0, 0)
  ) +
  scale_fill_brewer(palette = "Purples") +
  labs(title = "Pola Transaksi Peserta BI-RTGS (Jan 2016 - Apr 2021)", 
       fill = "Zona Throughput") +
  theme_tufte(base_size = 11) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 60, hjust = 1),
        axis.ticks = element_line(colour = "grey50", size = 0.7))
p3b



# Throughput Peserta Per Kelompok Buku
p3c <- ggplot(dat_C_buk, aes(x = bulan, y = nominal, fill = fct_rev(zona))) +
  geom_area(position = position_fill()) +
  annotate("segment", y = 0.3, yend = 0.3,
           x = MIN_DATE, xend = MAX_DATE, 
           colour = "grey30", size = 1, alpha = .7) +
  annotate("segment", y = 0.6, yend = 0.6,
           x = MIN_DATE, xend = MAX_DATE, 
           colour = "grey30", size = 1, alpha = .7) +
  annotate("text", label = "30%", fontface = "bold", 
           x = ANNOTATION_DATE, y = 0.35) +
  annotate("text", label = "60%", fontface = "bold", 
           x = ANNOTATION_DATE, y = 0.65) +
  facet_wrap(kelompok~.) +
  scale_x_yearmon(breaks = as.yearmon(seq(as.Date(MIN_DATE), 
                                          as.Date(MAX_DATE), 
                                          "3 month")),
                  expand = c(0, 0)) +
  scale_y_continuous(
    breaks = seq(0,1,0.2),
    labels = scales::percent_format(),
    expand = c(0, 0)
  ) +
  scale_fill_brewer(palette = "Purples") +
  labs(title = "Pola Transaksi Peserta BI-RTGS (Jan 2016 - Apr 2021)", 
       fill = "Zona Throughput") +
  theme_tufte(base_size = 12) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 60, hjust = 1),
        axis.ticks = element_line(colour = "grey50", size = 0.7))
p3c



# Pola Transaksi Harian Peserta
dat_C_jam_scaleY <- 2.5e15  
dat_C_jam_max_nom <- ceiling(max(dat_C_jam$nominal) / 
                               dat_C_jam_scaleY) * dat_C_jam_scaleY
dat_C_jam <- dat_C_jam %>% 
  mutate(kumulatif = cumsum(nominal),
         # nilai kumulatif yang disesuaikan utk visualisasi
         kumulatif = kumulatif * dat_C_jam_max_nom / max(kumulatif)) 

dat_C_jam_nom <- dat_C_jam %>% 
  group_by(zona = fct_wkt2zn(waktu)) %>% 
  summarise(nominal = sum(nominal)) %>% 
  mutate(nominal = nominal/sum(nominal)) %>% 
  pull(nominal) %>% 
  scales::percent()

dat_C_jam_maxY <- dat_C_jam_max_nom
dat_C_jam_breaksY <- seq(0, dat_C_jam_maxY, length = 11)
dat_C_jam_annotationY <- dat_C_jam_maxY 
dat_C_jam_annotationX <- c(
  0.5 + 7.5/2,
  7.5 + (15.5-7.5)/2,
  15.5 + (23.5-15.5)/2
)

p3d <- ggplot(dat_C_jam, aes(x = waktu)) +
  annotate(geom = "segment", linetype = 2, size = 1, colour = "grey50",
           x = 7.5, xend = 7.5, y = 0, yend = dat_C_jam_maxY) +
  annotate(geom = "segment", linetype = 2, size = 1, colour = "grey50",
           x = 15.5, xend = 15.5, y = 0, yend = dat_C_jam_maxY) +
  annotate("text", label = paste0("Zona 1\n", dat_C_jam_nom[1]),
           fontface = "bold", x = dat_C_jam_annotationX[1],
           y = dat_C_jam_annotationY) +
  annotate("text", label = paste0("Zona 2\n", dat_C_jam_nom[2]),
           fontface = "bold", x = dat_C_jam_annotationX[2],
           y = dat_C_jam_annotationY) +
  annotate("text", label = paste0("Zona 3\n", dat_C_jam_nom[3]),
           fontface = "bold", x = dat_C_jam_annotationX[3],
           y = dat_C_jam_annotationY) +
  geom_col(aes(y = nominal), fill = "grey30") +
  geom_line(aes(y = kumulatif, group = 1), size = 1.5, alpha = 0.7, 
            colour = "red") +
  scale_x_discrete(breaks = WAKTU_LEVELS) +
  scale_y_continuous(
    limits = c(0, dat_C_jam_maxY + dat_C_jam_scaleY),
    breaks = dat_C_jam_breaksY,
    labels = scales::unit_format(unit = "", scale = 1e-12),
    expand = c(0,0),
    sec.axis = sec_axis(~.,
                        name = "Persentase Nominal Transaksi Kumulatif",
                        breaks = dat_C_jam_breaksY,
                        labels = scales::percent(0:10/10))) +
  labs(
    title = "Throughput Peserta RTGS (Periode Jan 2016 - Apr 2021)",
    subtitle = paste("Jenis Transaksi:", 
                     paste(dat_C_type_trx, collapse = ", ")),
    y = "Nominal Transaksi (dalam Triliun)",
    x = "Waktu Transaksi"
  ) +
  theme_tufte(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )
p3d



# Save grafik
ggsave(here("figures/result/kum_3.png"), p3a, width = 12, height = 6)
ggsave(here("figures/result/kel_3.png"), p3b, width = 12, height = 9)
ggsave(here("figures/result/buk_3.png"), p3c, width = 12, height = 7.5)
ggsave(here("figures/result/jam_3.png"), p3d, width = 12, height = 6)



# Save Output Tabel
dat_C_kel %>% 
  group_by(kelompok, zona) %>% 
  summarise(nominal = sum(nominal)) %>%
  mutate(prop = nominal / sum(nominal),
         prop = scales::percent(prop, accuracy = 0.01)) %>%
  select(kelompok, zona, prop) %>% 
  write_csv(here("data/result/kel_3.csv"))
dat_C_buk %>% 
  group_by(kelompok, zona) %>% 
  summarise(nominal = sum(nominal)) %>%
  mutate(prop = nominal / sum(nominal),
         prop = scales::percent(prop, accuracy = 0.01)) %>%
  select(kelompok, zona, prop) %>% 
  write_csv(here("data/result/buk_3.csv"))
dat_C_kum %>% 
  group_by(zona) %>% 
  summarise(nominal = sum(nominal)) %>% 
  mutate(prop = nominal / sum(nominal),
         prop = scales::percent(prop, accuracy = 0.01)) %>%
  select(zona, prop) %>% 
  write_csv(here("data/result/kum_zn_3.csv"))
dat_C_jam %>% 
  mutate(cum_nom = cumsum(nominal),
         prop = cum_nom / sum(nominal),
         prop = scales::percent(prop, accuracy = 0.01)) %>%
  select(waktu, prop) %>% 
  write_csv(here("data/result/kum_jam_3.csv"))





# f. Analisis Throughput Skenario 4 (Request Mas Afin) --------------------

# Jenis transaksi skenario 4
dat_D_type_trx <- c("Nasabah", "PUAB", "Valas", "Pasar Modal")



# Persiapan Data
dat_D_jam <- df_kel %>% 
  filter(bulan <= MAX_DATE,
         fct_wkt2zn(waktu) %in% ZONA,
         kelompok %in% KELOMPOK,
         transaksi %in% dat_D_type_trx) %>% 
  mutate(waktu = fct_drop(waktu)) %>% 
  group_by(waktu) %>% 
  summarise(nominal = sum(nominal)) 



# Pola Transaksi Harian Peserta
dat_D_jam_scaleY <- 2.5e15  
dat_D_jam_max_nom <- ceiling(max(dat_D_jam$nominal) / 
                               dat_D_jam_scaleY) * dat_D_jam_scaleY
dat_D_jam <- dat_D_jam %>% 
  mutate(kumulatif = cumsum(nominal),
         # nilai kumulatif yang disesuaikan utk visualisasi
         kumulatif = kumulatif * dat_D_jam_max_nom / max(kumulatif)) 

dat_D_jam_nom <- dat_D_jam %>% 
  group_by(zona = fct_wkt2zn(waktu)) %>% 
  summarise(nominal = sum(nominal)) %>% 
  mutate(nominal = nominal/sum(nominal)) %>% 
  pull(nominal) %>% 
  scales::percent()

dat_D_jam_maxY <- dat_D_jam_max_nom
dat_D_jam_breaksY <- seq(0, dat_D_jam_maxY, length = 11)
dat_D_jam_annotationY <- dat_D_jam_maxY 
dat_D_jam_annotationX <- c(
  0.5 + 7.5/2,
  7.5 + (15.5-7.5)/2,
  15.5 + (23.5-15.5)/2
)

p4d <- ggplot(dat_D_jam, aes(x = waktu)) +
  annotate(geom = "segment", linetype = 2, size = 1, colour = "grey50",
           x = 7.5, xend = 7.5, y = 0, yend = dat_D_jam_maxY) +
  annotate(geom = "segment", linetype = 2, size = 1, colour = "grey50",
           x = 15.5, xend = 15.5, y = 0, yend = dat_D_jam_maxY) +
  annotate("text", label = paste0("Zona 1\n", dat_D_jam_nom[1]),
           fontface = "bold", x = dat_D_jam_annotationX[1],
           y = dat_D_jam_annotationY) +
  annotate("text", label = paste0("Zona 2\n", dat_D_jam_nom[2]),
           fontface = "bold", x = dat_D_jam_annotationX[2],
           y = dat_D_jam_annotationY) +
  annotate("text", label = paste0("Zona 3\n", dat_D_jam_nom[3]),
           fontface = "bold", x = dat_D_jam_annotationX[3],
           y = dat_D_jam_annotationY) +
  geom_col(aes(y = nominal), fill = "grey30") +
  geom_line(aes(y = kumulatif, group = 1), size = 1.5, alpha = 0.7, 
            colour = "red") +
  scale_x_discrete(breaks = WAKTU_LEVELS) +
  scale_y_continuous(
    limits = c(0, dat_D_jam_maxY + dat_D_jam_scaleY),
    breaks = dat_D_jam_breaksY,
    labels = scales::unit_format(unit = "", scale = 1e-12),
    expand = c(0,0),
    sec.axis = sec_axis(~.,
                        name = "Persentase Nominal Transaksi Kumulatif",
                        breaks = dat_D_jam_breaksY,
                        labels = scales::percent(0:10/10))) +
  labs(
    title = "Throughput Peserta RTGS (Periode Jan 2016 - Apr 2021)",
    subtitle = paste("Jenis Transaksi:", 
                     paste(dat_D_type_trx, collapse = ", ")),
    y = "Nominal Transaksi (dalam Triliun)",
    x = "Waktu Transaksi"
  ) +
  theme_tufte(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )
p4d



# Save Output Grafik
ggsave(here("figures/result/jam_4.png"), p4d, width = 12, height = 6)



# Save Output Tabel
dat_D_kum %>% 
  group_by(zona) %>% 
  summarise(nominal = sum(nominal)) %>% 
  mutate(prop = nominal / sum(nominal),
         prop = scales::percent(prop, accuracy = 0.01)) %>%
  select(zona, prop) %>% 
  write_csv(here("data/result/kum_zn_4.csv"))
dat_D_jam %>% 
  mutate(cum_nom = cumsum(nominal),
         prop = cum_nom / sum(nominal),
         prop = scales::percent(prop, accuracy = 0.01)) %>%
  select(waktu, prop) %>% 
  write_csv(here("data/result/kum_jam_4.csv"))





# g. Analisis Throughput Skenario 5 (Request Mas Afin) --------------------

# Jenis transaksi skenario 4
dat_E_type_trx <- c("PUAB", "Valas", "Pasar Modal")



# Persiapan Data
dat_E_jam <- df_kel %>% 
  filter(bulan <= MAX_DATE,
         fct_wkt2zn(waktu) %in% ZONA,
         kelompok %in% KELOMPOK,
         transaksi %in% dat_E_type_trx) %>% 
  mutate(waktu = fct_drop(waktu)) %>% 
  group_by(waktu) %>% 
  summarise(nominal = sum(nominal)) 



# Pola Transaksi Harian Peserta
dat_E_jam_scaleY <- 1e15  
# nilai maksimal yang dibulatkan sesuai skala
dat_E_jam_max_nom <- ceiling(max(dat_E_jam$nominal) / 
                               dat_E_jam_scaleY) * dat_E_jam_scaleY
dat_E_jam <- dat_E_jam %>% 
  mutate(kumulatif = cumsum(nominal),
         # nilai kumulatif yang disesuaikan utk visualisasi
         kumulatif = kumulatif * dat_E_jam_max_nom / max(kumulatif)) 

dat_E_jam_nom <- dat_E_jam %>% 
  group_by(zona = fct_wkt2zn(waktu)) %>% 
  summarise(nominal = sum(nominal)) %>% 
  mutate(nominal = nominal/sum(nominal)) %>% 
  pull(nominal) %>% 
  scales::percent()

dat_E_jam_maxY <- dat_E_jam_max_nom
dat_E_jam_breaksY <- seq(0, dat_E_jam_maxY, length = 11)
dat_E_jam_annotationY <- dat_E_jam_maxY 
dat_E_jam_annotationX <- c(
  0.5 + 7.5/2,
  7.5 + (15.5-7.5)/2,
  15.5 + (23.5-15.5)/2
)

p5d <- ggplot(dat_E_jam, aes(x = waktu)) +
  annotate(geom = "segment", linetype = 2, size = 1, colour = "grey50",
           x = 7.5, xend = 7.5, y = 0, yend = dat_E_jam_maxY) +
  annotate(geom = "segment", linetype = 2, size = 1, colour = "grey50",
           x = 15.5, xend = 15.5, y = 0, yend = dat_E_jam_maxY) +
  annotate("text", label = paste0("Zona 1\n", dat_E_jam_nom[1]),
           fontface = "bold", x = dat_E_jam_annotationX[1],
           y = dat_E_jam_annotationY) +
  annotate("text", label = paste0("Zona 2\n", dat_E_jam_nom[2]),
           fontface = "bold", x = dat_E_jam_annotationX[2],
           y = dat_E_jam_annotationY) +
  annotate("text", label = paste0("Zona 3\n", dat_E_jam_nom[3]),
           fontface = "bold", x = dat_E_jam_annotationX[3],
           y = dat_E_jam_annotationY) +
  geom_col(aes(y = nominal), fill = "grey30") +
  geom_line(aes(y = kumulatif, group = 1), size = 1.5, alpha = 0.7, 
            colour = "red") +
  scale_x_discrete(breaks = WAKTU_LEVELS) +
  scale_y_continuous(
    limits = c(0, dat_E_jam_maxY + dat_E_jam_scaleY),
    breaks = dat_E_jam_breaksY,
    labels = scales::unit_format(unit = "", scale = 1e-12),
    expand = c(0,0),
    sec.axis = sec_axis(~.,
                        name = "Persentase Nominal Transaksi Kumulatif",
                        breaks = dat_E_jam_breaksY,
                        labels = scales::percent(0:10/10))) +
  labs(
    title = "Throughput Peserta RTGS (Periode Jan 2016 - Apr 2021)",
    subtitle = paste("Jenis Transaksi:", 
                     paste(dat_E_type_trx, collapse = ", ")),
    y = "Nominal Transaksi (dalam Triliun)",
    x = "Waktu Transaksi"
  ) +
  theme_tufte(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )
p5d



# Save Output Grafik
ggsave(here("figures/result/jam_5.png"), p5d, width = 12, height = 6)



# Save Output Tabel
dat_E_jam %>% 
  mutate(cum_nom = cumsum(nominal),
         prop = cum_nom / sum(nominal),
         prop = scales::percent(prop, accuracy = 0.01)) %>%
  select(waktu, prop) %>% 
  write_csv(here("data/result/kum_jam_5.csv"))