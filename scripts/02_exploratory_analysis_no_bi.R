# desc  : Exploratory data analysis 2
# author: Hanzholah Shobri
# date  : 2021-01-18




# A. Preparation ----------------------------------------------------------

library(here)
library(tidyverse)
library(scales)
library(lubridate)
library(cowplot)
library(zoo)
library(patchwork)
library(gridExtra)
library(ggthemes)

source(here("scripts/data_import.R"))

df_by_kelompok <- df_by_kelompok %>%
  filter(kelompok != "Bank Sentral")

df_by_kelompok %>% summary


fct_brief_buk <- function(f) {
  fct_recode(f,
             "Buku 1" = "Kelompok Buku 1",
             "Buku 2" = "Kelompok Buku 2",
             "Buku 3" = "Kelompok Buku 3",
             "Buku 4" = "Kelompok Buku 4")
}

fct_brief_kel <- function(f) {
  fct_recode(f,
             "Bank\nAsing" = "Bank Asing",
             "Bank\nCampuran" = "Bank Campuran",
             "Bank\nPemerintah" = "Bank Pemerintah",
             "Bank\nSwasta" = "Bank Swasta",
             "Bank\nSyariah" = "Bank Syariah")
}





# B. Data Visualization ---------------------------------------------------

# B.1. Setting Parameter --------------------------------------------------

# B.1.1. Visualization Settings -------------------------------------------
# Tema Dasar
base_theme <- function(n = 12) {
  # theme_cowplot(n) +
  theme_tufte() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1),
          axis.line = element_line(colour = "grey50"))
}
# Geometri untuk plotting throughput guidelines
geom_throughput_guidelines <- function() {
  # list(
  #   annotate(xmin = 0.55,  xmax = 10.5, ymin = 0.00,  ymax = Inf,
  #            geom = "rect",  fill = "#f0f0f0"),
  #   annotate(xmin = 10.5,  xmax = 18.5, ymin = 0.00,  ymax = Inf,
  #            geom = "rect",  fill = "#f3f3f3"),
  #   annotate(xmin = 18.5,  xmax = 29.5, ymin = 0.00,  ymax = Inf,
  #            geom = "rect",  fill = "#f6f6f6")
  # )
  list(
    annotate(x = 10.5,  xend = 10.5, y = 0.00,  yend = Inf,
             geom = "segment",  colour = "grey60", linetype = "twodash"),
    annotate(x = 18.5,  xend = 18.5, y = 0.00,  yend = Inf,
             geom = "segment",  colour = "grey60", linetype = "twodash")
  )
}
# Skala Y untuk data nominal
scale_y_data_rtgs <- function(max, by = 2.5e16, scale = 1e-12) {
  scale_y_continuous(
    breaks = seq(0, max, by),
    limits = c(0, max),
    labels = unit_format(unit = "", scale = scale)
  )
}
# Scale X untuk waktu
scale_x_waktu <- function(by = 1) {
  scale_x_discrete(
    breaks = waktu_levels[seq(1, 29, by)],
    labels = waktu_labels[seq(1, 29, by)]
  )
}



# B.1.2. Parameter Waktu --------------------------------------------------
waktu_levels <- levels(df_total$waktu) 
waktu_labels<- c(
  str_extract(waktu_levels[1:28], ".\\d{2}:\\d{2}$"), 
  "19:00<="
)





# B.2. Explorasi Data Transaksi -------------------------------------------
# Data Nominal
max_tot_nom_base <- df_by_kelompok %>%
  group_by(waktu) %>% summarise(nominal = sum(nominal)) %>%
  summarise(maks = max(nominal)) %>% pull

max_tot_nom_by_thn <- df_by_kelompok %>%
  group_by(year(bulan), waktu) %>% summarise(nominal = sum(nominal)) %>%
  ungroup() %>% summarise(maks = max(nominal)) %>% pull
max_tot_nom_by_thn_kel <- df_by_kelompok %>%
  group_by(year(bulan), waktu, kelompok) %>% 
  summarise(nominal = sum(nominal)) %>%
  ungroup() %>% summarise(maks = max(nominal)) %>% 
  pull
max_tot_nom_by_thn_buk <- df_by_buku %>%
  group_by(year(bulan), waktu, kelompok) %>% 
  summarise(nominal = sum(nominal)) %>%
  ungroup() %>% summarise(maks = max(nominal)) %>% 
  pull

max_tot_nom_by_kel <- df_by_kelompok %>%
  group_by(kelompok, waktu) %>% summarise(nominal = sum(nominal)) %>%
  ungroup() %>% summarise(maks = max(nominal)) %>% pull
max_tot_nom_by_buk <- df_by_buku %>%
  group_by(kelompok, waktu) %>% summarise(nominal = sum(nominal)) %>%
  ungroup() %>% summarise(maks = max(nominal)) %>% pull


# Data Volume
max_tot_vol_base <- df_total %>%
  group_by(waktu) %>% summarise(volume = sum(volume)) %>%
  summarise(maks = max(volume)) %>% pull

max_tot_vol_by_thn <- df_total %>%
  group_by(year(bulan), waktu) %>% summarise(volume = sum(volume)) %>%
  ungroup() %>% summarise(maks = max(volume)) %>% pull
max_tot_vol_by_thn_kel <- df_by_kelompok %>%
  group_by(year(bulan), waktu, kelompok) %>% 
  summarise(volume = sum(volume)) %>%
  ungroup() %>% summarise(maks = max(volume)) %>% 
  pull
max_tot_vol_by_thn_buk <- df_by_buku %>%
  group_by(year(bulan), waktu, kelompok) %>% 
  summarise(volume = sum(volume)) %>%
  ungroup() %>% summarise(maks = max(volume)) %>% 
  pull

max_tot_vol_by_kel <- df_by_kelompok %>%
  group_by(kelompok, waktu) %>% summarise(volume = sum(volume)) %>%
  ungroup() %>% summarise(maks = max(volume)) %>% pull
max_tot_vol_by_buk <- df_by_buku %>%
  group_by(kelompok, waktu) %>% summarise(volume = sum(volume)) %>%
  ungroup() %>% summarise(maks = max(volume)) %>% pull



# B.2.1. Visualization - Summary ------------------------------------------

.p_summary_kel_1 <- df_by_kelompok %>%
  ggplot(aes(x = waktu, y = nominal, fill = kelompok)) +
  geom_throughput_guidelines() +
  geom_col() +
  scale_y_data_rtgs(max_tot_nom_base) +
  scale_x_waktu() +
  scale_fill_brewer(palette = "Dark2") +
  labs(
    title = "Data Nominal Transaksi (Jan 2016 - Des 2020)",
    x = "Waktu",
    y = "Nominal (dalam triliun)",
    fill = "Kelompok Bank"
  ) 

.p_summary_kel_2 <- df_by_kelompok %>%
  ggplot(aes(x = waktu, y = nominal, fill = kelompok)) +
  geom_col(position = position_fill()) +
  scale_x_waktu() +
  scale_fill_brewer(palette = "Dark2") +
  labs(
    title = "Proporsi Nominal Transaksi (Jan 2016 - Des 2020)",
    x = "Waktu",
    y = "Nominal (dalam triliun)",
    fill = "Kelompok Bank"
  ) 

.p_summary_kel_3 <- df_by_kelompok %>%
  ggplot(aes(x = waktu, y = volume, fill = kelompok)) +
  geom_throughput_guidelines() +
  geom_col() +
  scale_y_data_rtgs(max_tot_vol_base, by = 5e5, scale = 1e-3) +
  scale_x_waktu() +
  scale_fill_brewer(palette = "Dark2") +
  labs(
    title = "Data Volume Transaksi (Jan 2016 - Des 2020)",
    x = "Waktu",
    y = "Volume (dalam ribu)",
    fill = "Kelompok Bank"
  ) 

.p_summary_kel_4 <- df_by_kelompok %>%
  ggplot(aes(x = waktu, y = volume, fill = kelompok)) +
  geom_col(position = position_fill()) +
  scale_x_waktu() +
  scale_fill_brewer(palette = "Dark2") +
  labs(
    title = "Proporsi Volume Transaksi (Jan 2016 - Des 2020)",
    x = "Waktu",
    y = "Proporsi Volume",
    fill = "Kelompok Bank"
  ) 


p_summary_kel <- (.p_summary_kel_1 + .p_summary_kel_3) / 
  (.p_summary_kel_2 + .p_summary_kel_4) & 
  base_theme()

p_summary_kel <- p_summary_kel +
  plot_annotation(title = "Aliran Transaksi Sistem BI-RTGS Sepanjang Hari") +
  plot_layout(guides = 'collect')



.p_summary_buk_1 <- df_by_buku %>%
  ggplot(aes(x = waktu, y = nominal, fill = kelompok)) +
  geom_throughput_guidelines() +
  geom_col() +
  scale_y_data_rtgs(max_tot_nom_base) +
  scale_x_waktu() +
  scale_fill_brewer(palette = "Dark2") +
  labs(
    title = "Data Nominal Transaksi (Jan 2016 - Des 2020)",
    x = "Waktu",
    y = "Nominal (dalam triliun)",
    fill = "Kelompok Bank"
  ) 

.p_summary_buk_2 <- df_by_buku %>%
  ggplot(aes(x = waktu, y = nominal, fill = kelompok)) +
  geom_col(position = position_fill()) +
  scale_x_waktu() +
  scale_fill_brewer(palette = "Dark2") +
  labs(
    title = "Proporsi Nominal Transaksi (Jan 2016 - Des 2020)",
    x = "Waktu",
    y = "Nominal (dalam triliun)",
    fill = "Kelompok Bank"
  ) 

.p_summary_buk_3 <- df_by_buku %>%
  ggplot(aes(x = waktu, y = volume, fill = kelompok)) +
  geom_throughput_guidelines() +
  geom_col() +
  scale_y_data_rtgs(max_tot_vol_base, by = 5e5, scale = 1e-3) +
  scale_x_waktu() +
  scale_fill_brewer(palette = "Dark2") +
  labs(
    title = "Data Volume Transaksi (Jan 2016 - Des 2020)",
    x = "Waktu",
    y = "Volume (dalam ribu)",
    fill = "Kelompok Bank"
  ) 

.p_summary_buk_4 <- df_by_buku %>%
  ggplot(aes(x = waktu, y = volume, fill = kelompok)) +
  geom_col(position = position_fill()) +
  scale_x_waktu() +
  scale_fill_brewer(palette = "Dark2") +
  labs(
    title = "Proporsi Volume Transaksi (Jan 2016 - Des 2020)",
    x = "Waktu",
    y = "Proporsi Volume",
    fill = "Kelompok Bank"
  ) 


p_summary_buk <- (.p_summary_buk_1 + .p_summary_buk_3) / 
  (.p_summary_buk_2 + .p_summary_buk_4) & 
  base_theme()

p_summary_buk <- p_summary_buk +
  plot_annotation(title = "Aliran Transaksi Sistem BI-RTGS Sepanjang Hari") +
  plot_layout(guides = 'collect')



# B.2.2. Visualization - nominal ------------------------------------------

p_nominal_thn <- df_by_kelompok %>%
  ggplot(aes(x = waktu, y = nominal, fill = factor(year(bulan)))) +
  geom_throughput_guidelines() +
  geom_col() +
  scale_y_data_rtgs(max_tot_nom_by_thn, by = 2.5e15) +
  scale_x_waktu(2) +
  scale_fill_brewer(palette = "Dark2") +
  labs(
    title = "Data Nominal Transaksi (Jan 2016 - Des 2020)",
    subtitle = "Pembagian berdasarkan tahun",
    x = "Waktu",
    y = "Nominal (dalam triliun)",
    fill = "Kelompok Bank"
  ) +
  facet_wrap(.~year(bulan)) +
  base_theme()

p_nominal_kel <- df_by_kelompok %>%
  ggplot(aes(x = waktu, y = nominal, fill = kelompok)) +
  geom_throughput_guidelines() +
  geom_col() +
  scale_y_data_rtgs(max_tot_nom_by_kel, by = 2.5e15) +
  scale_x_waktu(2) +
  scale_fill_brewer(palette = "Dark2") +
  labs(
    title = "Data Nominal Transaksi (Jan 2016 - Des 2020)",
    subtitle = "Pembagian berdasarkan kelompok peserta",
    x = "Waktu",
    y = "Nominal (dalam triliun)",
    fill = "Kelompok Bank"
  ) +
  facet_wrap(.~kelompok) +
  base_theme()

p_nominal_thn_kel <- df_by_kelompok %>%
  mutate(kelompok = fct_brief_kel(kelompok)) %>%
  ggplot(aes(x = waktu, y = nominal, fill = kelompok)) +
  geom_throughput_guidelines() +
  geom_col() +
  scale_y_data_rtgs(max_tot_nom_by_thn_kel, by = 1e15) +
  scale_x_waktu(3) +
  scale_fill_brewer(palette = "Dark2") +
  labs(
    title = "Data Nominal Transaksi (Jan 2016 - Des 2020)",
    subtitle = "Pembagian berdasarkan tahun dan kelompok peserta",
    x = "Waktu",
    y = "Nominal (dalam triliun)",
    fill = "Kelompok Bank"
  ) +
  facet_grid(kelompok~year(bulan)) +
  base_theme()

p_nominal_buk <- df_by_buku %>%
  ggplot(aes(x = waktu, y = nominal, fill = kelompok)) +
  geom_throughput_guidelines() +
  geom_col() +
  scale_y_data_rtgs(max_tot_nom_by_buk, by = 2.5e15) +
  scale_x_waktu(2) +
  scale_fill_brewer(palette = "Dark2") +
  labs(
    title = "Data Nominal Transaksi (Jan 2016 - Des 2020)",
    subtitle = "Pembagian berdasarkan kategori buku bank",
    x = "Waktu",
    y = "Nominal (dalam triliun)",
    fill = "Kelompok Bank"
  ) +
  facet_wrap(.~kelompok) +
  base_theme()

p_nominal_thn_buk <- df_by_buku %>%
  mutate(kelompok = fct_brief_buk(kelompok)) %>%
  ggplot(aes(x = waktu, y = nominal, fill = kelompok)) +
  geom_throughput_guidelines() +
  geom_col() +
  scale_y_data_rtgs(max_tot_nom_by_thn_buk, by = 1e15) +
  scale_x_waktu(3) +
  scale_fill_brewer(palette = "Dark2") +
  labs(
    title = "Data Nominal Transaksi (Jan 2016 - Des 2020)",
    subtitle = "Pembagian berdasarkan tahun dan kategori buku bank",
    x = "Waktu",
    y = "Nominal (dalam triliun)",
    fill = "Kelompok Bank"
  ) +
  facet_grid(kelompok~year(bulan)) +
  base_theme()



# B.2.2. Visualization - volume -------------------------------------------

p_volume_thn <- df_by_kelompok %>%
  ggplot(aes(x = waktu, y = volume, fill = factor(year(bulan)))) +
  geom_throughput_guidelines() +
  geom_col() +
  scale_y_data_rtgs(max_tot_vol_by_thn, by = 1e5, scale = 1e-3) +
  scale_x_waktu(2) +
  scale_fill_brewer(palette = "Dark2") +
  labs(
    title = "Data Volume Transaksi (Jan 2016 - Des 2020)",
    subtitle = "Pembagian berdasarkan tahun",
    x = "Waktu",
    y = "Volume (dalam ribu)",
    fill = "Kelompok Bank"
  ) +
  facet_wrap(.~year(bulan)) +
  base_theme()

p_volume_kel <- df_by_kelompok %>%
  ggplot(aes(x = waktu, y = volume, fill = kelompok)) +
  geom_throughput_guidelines() +
  geom_col() +
  scale_y_data_rtgs(max_tot_vol_by_kel, by = 2.5e5, scale = 1e-3) +
  scale_x_waktu(2) +
  scale_fill_brewer(palette = "Dark2") +
  labs(
    title = "Data Volume Transaksi (Jan 2016 - Des 2020)",
    subtitle = "Pembagian berdasarkan kelompok peserta",
    x = "Waktu",
    y = "Volume (dalam ribu)",
    fill = "Kelompok Bank"
  ) +
  facet_wrap(.~kelompok) +
  base_theme()

p_volume_thn_kel <- df_by_kelompok %>%
  mutate(kelompok = fct_brief_kel(kelompok)) %>%
  ggplot(aes(x = waktu, y = volume, fill = kelompok)) +
  geom_throughput_guidelines() +
  geom_col() +
  scale_y_data_rtgs(max_tot_vol_by_thn_kel, by = 1e5, scale = 1e-3) +
  scale_x_waktu(3) +
  scale_fill_brewer(palette = "Dark2") +
  labs(
    title = "Data Volume Transaksi (Jan 2016 - Des 2020)",
    subtitle = "Pembagian berdasarkan tahun dan kelompok peserta",
    x = "Waktu",
    y = "Volume (dalam ribu)",
    fill = "Kelompok Bank"
  ) +
  facet_grid(kelompok~year(bulan)) +
  base_theme()

p_volume_buk <- df_by_buku %>%
  ggplot(aes(x = waktu, y = volume, fill = kelompok)) +
  geom_throughput_guidelines() +
  geom_col() +
  scale_y_data_rtgs(max_tot_vol_by_buk, by = 2.5e5, scale = 1e-3) +
  scale_x_waktu(2) +
  scale_fill_brewer(palette = "Dark2") +
  labs(
    title = "Data Volume Transaksi (Jan 2016 - Des 2020)",
    subtitle = "Pembagian berdasarkan kategori buku bank",
    x = "Waktu",
    y = "Volume (dalam ribu)",
    fill = "Kelompok Bank"
  ) +
  facet_wrap(.~kelompok) +
  base_theme()

p_volume_thn_buk <- df_by_buku %>%
  mutate(kelompok = fct_brief_buk(kelompok)) %>%
  ggplot(aes(x = waktu, y = volume, fill = kelompok)) +
  geom_throughput_guidelines() +
  geom_col() +
  scale_y_data_rtgs(max_tot_vol_by_thn_buk, by = 5e4, scale = 1e-3) +
  scale_x_waktu(3) +
  scale_fill_brewer(palette = "Dark2") +
  labs(
    title = "Data Volume Transaksi (Jan 2016 - Des 2020)",
    subtitle = "Pembagian berdasarkan tahun dan kategori buku bank",
    x = "Waktu",
    y = "Volume (dalam ribu)",
    fill = "Kelompok Bank"
  ) +
  facet_grid(kelompok~year(bulan)) +
  base_theme()




# B.3. Export Plots -------------------------------------------------------
plots <-list(
  p_summary_buk, p_summary_kel, p_nominal_thn, p_nominal_buk,
  p_nominal_thn_buk, p_nominal_kel, p_nominal_thn_kel, p_volume_buk,
  p_volume_kel, p_volume_thn, p_volume_thn_buk, p_volume_thn_kel
)


class(plots) <- c("arrangelist", class(plots))
ggsave(here("figures/multipage.pdf"), plots, 
       width = 297, height = 210, units = "mm")

here(c("ad", "asd"))

plots <- list(
  path = list(
    here("figures/exploratory_no_bi2/fig-summary-by-buk.png"),
    here("figures/exploratory_no_bi2/fig-summary-by-kel.png"),
    here("figures/exploratory_no_bi2/fig-nominal-by-thn.png"),
    here("figures/exploratory_no_bi2/fig-nominal-by-buk.png"),
    here("figures/exploratory_no_bi2/fig-nominal-by-thn-buk.png"),
    here("figures/exploratory_no_bi2/fig-nominal-by-kel.png"),
    here("figures/exploratory_no_bi2/fig-nominal-by-thn-kel.png"),
    here("figures/exploratory_no_bi2/fig-volume-by-thn.png"),
    here("figures/exploratory_no_bi2/fig-volume-by-buk.png"),
    here("figures/exploratory_no_bi2/fig-volume-by-thn-buk.png"),
    here("figures/exploratory_no_bi2/fig-volume-by-kel.png"),
    here("figures/exploratory_no_bi2/fig-volume-by-thn-kel.png")
  ),
  plot = list(
    p_summary_buk, p_summary_kel, p_nominal_thn, p_nominal_buk,
    p_nominal_thn_buk, p_nominal_kel, p_nominal_thn_kel, p_volume_buk,
    p_volume_kel, p_volume_thn, p_volume_thn_buk, p_volume_thn_kel
  )
)

walk2(plots$path, plots$plot, ggsave,
      width = 297, height = 210, units = "mm")
