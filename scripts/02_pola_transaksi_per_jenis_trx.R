# desc  : Throughput zones analysis
# author: Hanzholah Shobri
# creation date : 2021-06-02




# A. Preparation ----------------------------------------------------------

library(here)
library(tidyverse)
library(scales)
library(lubridate)
library(cowplot)
library(zoo)
library(patchwork)
library(ggthemes)


source(here("scripts/data_import.R"))


waktu_levels <- levels(df_total$waktu) 
waktu_labels<- c(
  str_extract(waktu_levels[1:28], ".\\d{2}:\\d{2}$"), 
  "19:00<="
)

# Skala Y untuk data nominal
scale_y_data_rtgs <- function(max, by = 2.5e16, scale = 1e-12) {
  scale_y_continuous(
    breaks = seq(0, max, by),
    limits = c(0, max),
    labels = unit_format(unit = "", scale = scale)
  )
}

# Scale X untuk waktu
scale_x_waktu <- function(by = 1, waktu = 1:29) {
  scale_x_discrete(
    breaks = waktu_levels[seq(min(waktu), max(waktu), by)],
    labels = waktu_labels[seq(min(waktu), max(waktu), by)]
  )
}

# Fungsi transformasi waktu menjadi zona
fct_waktu2zona <- function(f) {
  stopifnot(mean(levels(f) == waktu_levels) == 1)
  
  fct_collapse(
    f,
    "Zona 1" = c(
      #"<05:30", "05:30 - <06:00", 
      "06:00 - <06:30", "06:30 - <07:00",
      "07:00 - <07:30", "07:30 - <08:00", "08:00 - <08:30",
      "08:30 - <09:00", "09:00 - <09:30", "09:30 - <10:00"
    ),
    "Zona 2" = c(
      "10:00 - <10:30", "10:30 - <11:00", "11:00 - <11:30",
      "11:30 - <12:00", "12:00 - <12:30", "12:30 - <13:00",
      "13:00 - <13:30", "13:30 - <14:00"
    ),
    "Zona 3" = c(
      "14:00 - <14:30", "14:30 - <15:00", "15:00 - <15:30",
      "15:30 - <16:00", "16:00 - <16:30", "16:30 - <17:00",
      "17:00 - <17:30", "17:30 - <18:00", "18:00 - <18:30"
      #"18:30 - <19:00", ">= 19:00" 
    )
  )
}

normalize <- function(x) {(x-min(x))/(max(x)-min(x))}




# Analisa Awal ------------------------------------------------------------

df_by_trx %>% 
  group_by(kelompok) %>% 
  summarise(nominal = sum(nominal),
            volume = sum(volume))

df_by_subtrx %>% 
  group_by(kelompok) %>% 
  summarise(nominal = sum(nominal),
            volume = sum(volume)) %>% 
  print(n = Inf)


df_by_trx %>%
  group_by(kelompok) %>% 
  summarise(nominal = sum(nominal)) %>% 
  ggplot(aes(x=kelompok, y=nominal, fill=kelompok)) +
  geom_col(width=1) +
  #coord_polar("y", start=0) +
  theme_tufte()

p1 <- df_by_trx %>% 
  filter(!(kelompok %in% c("FPJP", "N/A"))) %>% 
  ggplot(aes(x = waktu, y = nominal)) +
  geom_col(fill = "royalblue3") +
  scale_y_continuous(labels = unit_format(unit = "", scale = 1e-12)) +
  scale_x_waktu(2) +
  facet_wrap(~fct_relevel(kelompok, "Lainnya", after = Inf), 
             scales = "free_y", nrow = 4) +
  labs(title = "Pola Transaksi Peserta RTGS (Jan 2016 - Apr 2021", 
       x = "Waktu Penyelesaian", y = "Nominal (dalam Triliun)") +
  theme_minimal(base_size = 20) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
p1

ggsave(here("figures/Grafik per Jenis Trx.png"), 
       plot = p1, width = 20, height = 15)


fct_relevel(df_by_trx$waktu, "Lainnya", after = Inf)



# Simulasi Kumulatif ------------------------------------------------------

jenis_trx <- c("FLI", "Nasabah", "Pasar Modal", "PUAB", "Valas",
               "Pem-IFTSA", "Pem-xIFTSA", "Lainnya")
zona_trx <- c("Zona 1", "Zona 2", "Zona 3")
min_date <- as.yearmon("2016-01")
max_date <- as.yearmon("2021-04")

dat_trx1 <- df_by_trx %>% 
  filter(
    bulan <= max_date,
    fct_waktu2zona(waktu) %in% zona_trx,
    kelompok %in% jenis_trx
  ) %>% 
  group_by(waktu) %>% 
  summarise(nom = sum(nominal)) %>% 
  # normalisasi berdasarkan nilai max nominal trx utk visualisasi
  mutate(cum = cumsum(nom)*max(nom)/sum(nom)) 

write_csv(dat1, here("data/transaksi_per_jenis.csv"))

dat_trx1_nom <- dat_trx1 %>% 
  group_by(zona = fct_waktu2zona(waktu)) %>% 
  summarise(nom = sum(nom)) %>% 
  mutate(nom = nom/sum(nom)) %>% 
  pull(nom) %>% 
  scales::percent()

p2_y1_dat_max <- max(dat_trx1$nom)
p2_y1_scale <- 2.5e15  # kurang lebih = nominal max dibagi 10
p2_y1_breaks <- seq(0, p2_y1_dat_max, p2_y1_scale)
p2_y_max <- ceiling(p2_y1_dat_max/p2_x1_scale)*p2_x1_scale
p2_ann_txt_x <- c(
  0.5 + 8.5/2,
  8.5 + (16.5-8.5)/2,
  16.5 + (25.5-16.5)/2
)


dat_trx1 %>% 
  ggplot(aes(x = waktu)) +
  annotate(geom = "rect", fill = "lightcyan3",
           xmin = 0.5, xmax = 8.5, ymin = 0, ymax = Inf) +
  annotate(geom = "rect", fill = "lightcyan2",
           xmin = 8.5, xmax = 16.5, ymin = 0, ymax = Inf) +
  annotate(geom = "rect", fill = "lightcyan1", 
           xmin = 16.5, xmax = 25.5, ymin = 0, ymax = Inf) +
  annotate("text", label = paste0("Zona 1\n", dat_trx1_nom[1]), 
           fontface = "bold", x = p2_ann_txt_x[1], y = p2_y_max, vjust = 1.5) +
  annotate("text", label = paste0("Zona 2\n", dat_trx1_nom[2]), 
           fontface = "bold", x = p2_ann_txt_x[2], y = p2_y_max, vjust = 1.5) +
  annotate("text", label = paste0("Zona 3\n", dat_trx1_nom[3]), 
           fontface = "bold", x = p2_ann_txt_x[3], y = p2_y_max, vjust = 1.5) +
  geom_col(aes(y = nom), fill = "grey30") +
  geom_line(aes(y = cum, group = 1), size = 1.5, alpha = 0.7, colour = "red") +
  scale_x_waktu() +
  scale_y_continuous(
    breaks = p2_y1_breaks,
    labels = scales::unit_format(unit = "", scale = 1e-12),
    expand = c(0,0),
    sec.axis = sec_axis(~.,
                        name = "Persentase Nominal Transaksi Kumulatif",
                        breaks = p2_y1_breaks,
                        labels = scales::percent(0:10/10))) +
  labs(
    title = "Throughput Peserta RTGS (Periode Jan 2016 - Apr 2021)",
    subtitle = paste("Jenis Transaksi:", paste(jenis_trx, collapse = ", ")),
    y = "Nominal Transaksi (dalam Triliun)",
    x = "Waktu Transaksi"
  ) +
  theme_tufte(base_size = 17) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )




# Analisa per jenis transaksi ---------------------------------------------

df_by_subtrx %>% distinct(kelompok) %>% pull
df_by_trx %>% distinct(kelompok) %>% pull
df_by_subtrx %>% 
  group_by(kelompok) %>% 
  summarise(nom = sum(nominal)) %>% 
  arrange(desc(nom)) %>% 
  write_csv(here("data/data_subtrx.csv"))

jenis_subtrx <- c(
  "TRANSAKSI ANTAR PESERTA - UNTUK NASABAH",
  "TRANSAKSI ANTAR PESERTA - JUAL BELI VALAS",
  "TRANSAKSI ANTAR PESERTA UNTUK NASABAH - SURAT BERHARGA PASAR MODAL",
  "TRANSAKSI ANTAR PESERTA - PUAB",
  "TRANSAKSI ANTAR PESERTA - PUAB JATUH TEMPO",
  "TRANSAKSI PELIMPAHAN PENERIMAAN NEGARA - UJICOBA MPN",
  "TRANSAKSI PEMERINTAH - PENERBITAN SBN",
  "TRANSAKSI ANTAR PESERTA - SURAT BERHARGA PASAR MODAL",
  "TRANSAKSI ANTAR PESERTA",
  "TRANSAKSI KAS - PENYETORAN",
  "TRANSAKSI ANTAR PESERTA - UNTUK NASABAH TANPA REKENING",
  "KEWAJIBAN PERUSAHAAN SWITCHING KPD BANK",
  "TRANSAKSI ANTAR PESERTA - PENARIKAN KAS",
  "SSS - TRANSAKSI SURAT BERHARGA - PASAR SEKUNDER 2ND LEG",
  "KEWAJIBAN BANK KPD PERUSAHAAN SWITCHING",
  "SSS - TRANSAKSI SURAT BERHARGA USD - PASAR SEKUNDER",
  "TRANSAKSI ANTAR PESERTA - PENGEMBALIAN",
  "TRANSAKSI PESERTA KE BI",
  "SSS - FASILITAS LIKUIDITAS INTRAHARI (REPO)",
  "SSS - FASILITAS LIKUIDITAS INTRAHARI (PELUNASAN)",
  "TRANSAKSI ANTAR PESERTA - JUAL BELI VALAS - PVP",
  "TRANSAKSI PELIMPAHAN PENERIMAAN NEGARA (WIT)",
  "BILLING CHARGES",
  "TRANSAKSI KLBI",
  "TRANSAKSI PEMBAYARAN KOMPENSASI",
  "TRANSAKSI KAS - SELISIH LEBIH/KURANG  PEMBATALAN DAN TITIPAN"
)



zona_subtrx <- c("Zona 1", "Zona 2", "Zona 3")

dat_subtrx1 <- df_by_subtrx %>% 
  filter(
    bulan <= max_date,
    fct_waktu2zona(waktu) %in% zona_subtrx,
    kelompok %in% jenis_subtrx
  ) %>% 
  group_by(waktu) %>% 
  summarise(nom = sum(nominal)) %>% 
  # normalisasi berdasarkan nilai max nominal trx utk visualisasi
  mutate(cum = cumsum(nom)*max(nom)/sum(nom)) 

dat_subtrx1 %>% 
  ggplot(aes(x = waktu)) +
  geom_col(aes(y = nom), fill = "orange") +
  geom_line(aes(y = cum, group = 1), size = 1.5, alpha = 0.6) +
  scale_x_waktu() +
  scale_y_continuous(sec.axis = sec_axis(~normalize(.))) +
  theme_tufte() +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1)
  )



jenis_subtrx <- c(
  "OPERASI MONETER",
  "OPERASI MONETER - JATUH TEMPO",
  "TRANSAKSI ANTAR PESERTA - UNTUK NASABAH",
  "TRANSAKSI ANTAR PESERTA - JUAL BELI VALAS",
  "SSS - TRANSAKSI SURAT BERHARGA - PASAR SEKUNDER",
  "TRANSAKSI ANTAR PESERTA UNTUK NASABAH - SURAT BERHARGA PASAR MODAL",
  "TRANSAKSI ANTAR PESERTA - PUAB",
  "TRANSAKSI ANTAR PESERTA - PUAB JATUH TEMPO",
  "SKNBI - PREFUND DEBET",
  "SKNBI - PENGEMBALIAN EKSES PREFUND DAN TOP UP DEBET",
  "TRANSAKSI PEMERINTAH - OLEH BI",
  "SKNBI - PENGEMBALIAN EKSES PREFUND DAN TOP UP KREDIT",
  "SKNBI - TOP UP PREFUND KREDIT",
  "TRANSAKSI PELIMPAHAN PENERIMAAN NEGARA - UJICOBA MPN",
  "TRANSAKSI PEMERINTAH - PENERBITAN SBN",
  "TRANSAKSI ANTAR PESERTA - SURAT BERHARGA PASAR MODAL",
  "TRANSAKSI ANTAR PESERTA",
  "TRANSAKSI ANTAR PESERTA - TSA",
  "SKNBI - PREFUND KREDIT",
  "SKNBI - BSK NASIONAL KLIRING KREDIT",
  "TRANSAKSI BI KE PESERTA",
  "TRANSAKSI KAS - PENYETORAN",
  "TRANSAKSI KAS - PENARIKAN",
  "TRANSAKSI ANTAR PESERTA - UNTUK NASABAH TANPA REKENING",
  "KEWAJIBAN PERUSAHAAN SWITCHING KPD BANK",
  "TRANSAKSI ANTAR PESERTA - PENARIKAN KAS",
  "SKNBI - BSK NASIONAL KLIRING DEBET",
  "TRANSAKSI PEMERINTAH - PELUNASAN SBN",
  "SSS - TRANSAKSI SURAT BERHARGA - PASAR SEKUNDER 2ND LEG",
  "KEWAJIBAN BANK KPD PERUSAHAAN SWITCHING",
  "TRANSAKSI PEMERINTAH - KUPON SBN",
  "SSS - TRANSAKSI SURAT BERHARGA USD - PASAR SEKUNDER",
  "TRANSAKSI PELIMPAHAN PENERIMAAN NEGARA (WIB)",
  "TRANSAKSI ANTAR PESERTA - PENGEMBALIAN",
  "TRANSAKSI BUN OLEH BANK",
  "TRANSAKSI PESERTA KE BI",
  "SSS - FASILITAS LIKUIDITAS INTRAHARI (REPO)",
  "SSS - FASILITAS LIKUIDITAS INTRAHARI (PELUNASAN)",
  "TRANSAKSI PEMERINTAH OLEH BANK",
  "TRANSAKSI PENERIMAAN BI",
  "TRANSAKSI PELIMPAHAN KELEBIHAN RPK-BUN-P",
  "TRANSAKSI PEMERINTAH - KUPON RE-ROUTING",
  "TRANSAKSI PEMERINTAH - OLEH BI VALAS",
  "TRANSAKSI PELIMPAHAN PENERIMAAN NEGARA (WITA)",
  "SKNBI - TOP UP PREFUND DEBET",
  "TRANSAKSI BI KE PESERTA VALAS",
  "OPERASI MONETER - BIAYA",
  "SKNBI - BILYET SALDO KLIRING (BSK) PEMBAYARAN REGULER",
  "TRANSAKSI ANTAR PESERTA - JUAL BELI VALAS - PVP",
  "TRANSAKSI ANTAR PESERTA DLM RANGKA CP OLEH BI",
  "TRANSAKSI PELIMPAHAN PENERIMAAN NEGARA (WIT)",
  "TRANSAKSI PESERTA KE BI DLM RANGKA CP OLEH BI",
  "SKNBI - BILYET SALDO KLIRING (BSK) PENAGIHAN REGULER",
  "BILLING CHARGES",
  "TRANSAKSI KLBI",
  "TRANSAKSI PEMBAYARAN KOMPENSASI",
  "TRANSAKSI PESERTA KE BI VALAS",
  "TRANSAKSI PEMERINTAH LAINNYA- OLEH BANK VALAS",
  "TRANSAKSI KAS - SELISIH LEBIH/KURANG  PEMBATALAN DAN TITIPAN",
  "TRANSAKSI PELIMPAHAN PENERIMAAN NEGARA BUKAN PAJAK",
  "SKNBI - RELEASE PREFUND DEBET",
  "OPERASI MONETER - PENERIMAAN",
  "FTSM FIRST LEG",
  "FTSM SECOND LEG",
  "N/A",
  "SKNBI - RELEASE PREFUND KREDIT",
  "SKNBI - TOP UP PREFUND KREDIT 2",
  "SYSTEM OPERATION",
  "TRANSAKSI ANTAR PESERTA - USD",
  "TRANSAKSI BI KE PESERTA - USD",
  "TRANSAKSI DARI BI - DARI NASABAH NON PEMERINTAH",
  "TRANSAKSI DARI BI - DARI NASABAH NON PEMERINTAH DALAM VALAS",
  "TRANSAKSI FPJP",
  "TRANSAKSI PEMERINTAH - LELANG SBN LAINNYA",
  "TRANSAKSI PEMERINTAH - PENGELUARAN NON TSA",
  "TRANSAKSI PEMERINTAH - PENGELUARAN NON TSA DARI VALAS",
  "TRANSAKSI SURAT BERHARGA BI DALAM VALAS - PELUNASAN",
  "TRANSAKSI SURAT BERHARGA BI DALAM VALAS - PENERBITAN"
)