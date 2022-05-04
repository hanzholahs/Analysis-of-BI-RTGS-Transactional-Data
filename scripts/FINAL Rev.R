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





# Import and Tidy Data ----------------------------------------------------

# a. Import Data ----------------------------------------------------------

# Import raw data
.path <- here("data/raw_update/",
              "Data RTGS TTC Per Jam(1)_Tanpa Bank Sentral dan Others.xlsx")

excel_sheets(.path)

.raw <- read_excel(.path, sheet = "Page1_1", skip = 6)



# b. Tidy Data By Buku ----------------------------------------------------

# Membuat df data transaksi berdasarkan kelompok buku
df_rev <- .raw %>%
  rename(transaksi = ...1, waktu = ...2) %>%
  select(1:7) %>% 
  tidyr::fill(transaksi) %>%
  slice(-nrow(.)) %>% 
  filter(transaksi != "All",
         waktu != "0000", 
         waktu != "All")



# Pivot data berdasarkan nominal dan volume
df_rev <- df_rev %>%
  pivot_longer(-(1:2), names_to = "tahun", values_to = "nominal") %>% 
  mutate(
    tahun = str_replace(tahun, "(\\.){1,3}\\d*", ""),
    transaksi = factor(transaksi),
    waktu = factor(waktu) %>%
      fct_relevel(">= 19:00", after = Inf),
    nominal = ifelse(!is.na(nominal), nominal, 0)
  ) %>% 
  select(transaksi, tahun, waktu, everything()) %>%
  arrange(transaksi, tahun, waktu)



# c. Export Data ----------------------------------------------------------

write_csv(df_rev, here("data/tidy/tidy_rev.csv"))




# Analisis ----------------------------------------------------------------


# a. Set up ---------------------------------------------------------------

ZONA <- c("Zona 1", "Zona 2", "Zona 3")
WAKTU_LEVELS <- filter(df_rev, fct_wkt2zn(waktu) %in% ZONA) %>% 
  pull(waktu) %>% unique() %>% fct_drop()

TRX_1 <- c(
  "KEWAJIBAN BANK KPD PERUSAHAAN SWITCHING", 
  "KEWAJIBAN PERUSAHAAN SWITCHING KPD BANK",
  "TRANSAKSI ANTAR PESERTA",
  "TRANSAKSI ANTAR PESERTA - PENARIKAN KAS",
  "TRANSAKSI ANTAR PESERTA - PENGEMBALIAN",
  "TRANSAKSI ANTAR PESERTA - PUAB",
  "TRANSAKSI ANTAR PESERTA - PUAB JATUH TEMPO",
  "SSS - TRANSAKSI SURAT BERHARGA - PASAR SEKUNDER",
  "SSS - TRANSAKSI SURAT BERHARGA - PASAR SEKUNDER 2ND LEG",
  "TRANSAKSI ANTAR PESERTA - SURAT BERHARGA PASAR MODAL",
  "TRANSAKSI ANTAR PESERTA UNTUK NASABAH - SURAT BERHARGA PASAR MODAL",
  "TRANSAKSI ANTAR PESERTA - JUAL BELI VALAS",
  "TRANSAKSI ANTAR PESERTA - JUAL BELI VALAS - PVP",
  "TRANSAKSI ANTAR PESERTA - UNTUK NASABAH",
  "TRANSAKSI ANTAR PESERTA - UNTUK NASABAH TANPA REKENING"
)

TRX_2 <- c(
  "KEWAJIBAN BANK KPD PERUSAHAAN SWITCHING", 
  "KEWAJIBAN PERUSAHAAN SWITCHING KPD BANK",
  "TRANSAKSI ANTAR PESERTA",
  "TRANSAKSI ANTAR PESERTA - PENARIKAN KAS",
  "TRANSAKSI ANTAR PESERTA - PENGEMBALIAN",
  "TRANSAKSI ANTAR PESERTA - PUAB",
  "TRANSAKSI ANTAR PESERTA - PUAB JATUH TEMPO",
  "SSS - TRANSAKSI SURAT BERHARGA - PASAR SEKUNDER",
  "SSS - TRANSAKSI SURAT BERHARGA - PASAR SEKUNDER 2ND LEG",
  "TRANSAKSI ANTAR PESERTA - SURAT BERHARGA PASAR MODAL",
  "TRANSAKSI ANTAR PESERTA UNTUK NASABAH - SURAT BERHARGA PASAR MODAL",
  "TRANSAKSI ANTAR PESERTA - JUAL BELI VALAS",
  "TRANSAKSI ANTAR PESERTA - JUAL BELI VALAS - PVP"
)






# b. Skenario 1 -----------------------------------------------------------
dat_A_jam <- df_rev %>% 
  filter(fct_wkt2zn(waktu) %in% ZONA,
         transaksi %in% TRX_1) %>% 
  mutate(waktu = fct_drop(waktu)) %>% 
  group_by(waktu) %>% 
  summarise(nominal = sum(nominal)) 

dat_A_jam_scaleY <- 2.5e15  
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

p_A <- ggplot(dat_A_jam, aes(x = waktu)) +
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
    title = "Throughput Peserta RTGS (Periode Jan 2016 - Des 2020)",
    subtitle = paste("Include Transaksi Nasabah"),
    y = "Nominal Transaksi (dalam Triliun)",
    x = "Waktu Transaksi"
  ) +
  theme_tufte(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )
p_A


# c. Skenario 2 -----------------------------------------------------------
dat_B_jam <- df_rev %>% 
  filter(fct_wkt2zn(waktu) %in% ZONA,
         transaksi %in% TRX_2) %>% 
  mutate(waktu = fct_drop(waktu)) %>% 
  group_by(waktu) %>% 
  summarise(nominal = sum(nominal)) 

dat_B_jam_scaleY <- 1e15  
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

p_B <- ggplot(dat_B_jam, aes(x = waktu)) +
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
    title = "Throughput Peserta RTGS (Periode Jan 2016 - Des 2020)",
    subtitle = paste("Exclude Transaksi Nasabah"),
    y = "Nominal Transaksi (dalam Triliun)",
    x = "Waktu Transaksi"
  ) +
  theme_tufte(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )
p_B



# d. Save Grafik ----------------------------------------------------------

ggsave(here("figures/result/rev_jam_1.png"), p_A, width = 12, height = 6)
ggsave(here("figures/result/rev_jam_2.png"), p_B, width = 12, height = 6)
