# desc  : Exploratory data analysis
# author: Hanzholah Shobri
# date  : 2021-01-11




# A. Preparation ----------------------------------------------------------

library(here)
library(tidyverse)
library(scales)
library(lubridate)
library(cowplot)
library(zoo)
library(patchwork)

source(here("scripts/data_import.R"))





# B. Data Visualization ---------------------------------------------------

# B.1. Setting Parameter --------------------------------------------------

# B.1.1. Visualization Settings -------------------------------------------
# Tema Dasar
base_theme <- theme_cowplot() +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1)
  )
# Geometri untuk plotting throughput guidelines
geom_throughput_guidelines <- function() {
  list(
    annotate(xmin = 0.55,  xmax = 10.5, ymin = 0.00,  ymax = Inf,
             geom = "rect",  fill = "#dedede"),
    annotate(xmin = 10.5,  xmax = 18.5, ymin = 0.00,  ymax = Inf,
             geom = "rect",  fill = "#efefef"),
    annotate(xmin = 18.5,  xmax = 29.5, ymin = 0.00,  ymax = Inf,
             geom = "rect",  fill = "#dedede")
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





# B.2. Data total ---------------------------------------------------------
max_tot_nom_base <- df_total %>%
  group_by(waktu) %>% summarise(nominal = sum(nominal)) %>%
  summarise(maks = max(nominal)) %>% pull
max_tot_nom_by_thn <- df_total %>%
  group_by(year(bulan), waktu) %>% summarise(nominal = sum(nominal)) %>%
  ungroup() %>% summarise(maks = max(nominal)) %>% pull

max_tot_vol_base <- df_total %>%
  group_by(waktu) %>% summarise(volume = sum(volume)) %>%
  summarise(maks = max(volume)) %>% pull
max_tot_vol_by_thn <- df_total %>%
  group_by(year(bulan), waktu) %>% summarise(volume = sum(volume)) %>%
  ungroup() %>% summarise(maks = max(volume)) %>% pull

p_tot <- vector("list", 4)



# B.2.1. Visualization - Nominal ------------------------------------------
p_tot[[1]]$path <- here("figures/exploratory/fig_total_nominal.png")
p_tot[[1]]$labels<- labs(
  title = "Aliran Transaksi Sistem BI-RTGS Sepanjang Hari",
  subtitle = "Data Nominal Transaksi (Jan 2016 - Jan 2021)",
  x = "Waktu",
  y = "Nominal (dalam triliun)"
) 
p_tot[[1]]$graph <- df_total %>%
  ggplot(aes(x = waktu, y = nominal)) +
  geom_throughput_guidelines() +
  geom_col(fill = "brown") +
  scale_y_data_rtgs(max_tot_nom_base) +
  scale_x_waktu() +
  p_tot[[1]]$labels +
  base_theme
p_tot[[1]]$graph



# B.2.2. Visualization - Nominal facet tahun ------------------------------
p_tot[[2]]$path  <- here(
  "figures/exploratory/fig_total_nominal_facet_tahun.png")
p_tot[[2]]$labels<- labs(
  title = "Aliran Transaksi Sistem BI-RTGS Sepanjang Hari",
  subtitle = "Data Nominal Transaksi (Jan 2016 - Jan 2021)",
  x = "Waktu", 
  y = "Nominal (dalam triliun)"
) 
p_tot[[2]]$graph <- df_total %>%
  ggplot(aes(x = waktu, y = nominal, fill = factor(year(bulan)))) +
  geom_throughput_guidelines() +
  geom_col() +
  scale_y_data_rtgs(max = max_tot_nom_by_thn, by = 1.25e16) +
  scale_x_waktu(by = 4)+
  scale_fill_brewer(palette = "Set1", guide = F) +
  facet_wrap(.~year(bulan)) +
  p_tot[[2]]$labels + base_theme
p_tot[[2]]$graph



# B.2.3. Visualization - Volume -------------------------------------------
p_tot[[3]]$path  <- here("figures/exploratory/fig_total_volume.png")
p_tot[[3]]$labels<- labs(
  title = "Aliran Transaksi Sistem BI-RTGS Sepanjang Hari",
  subtitle = "Data Volume Transaksi (Jan 2016 - Jan 2021)",
  x = "Waktu", y = "Volume (dalam ribu)"
)
p_tot[[3]]$graph <- df_total %>%
  ggplot(aes(x = waktu, y = volume)) +
  geom_throughput_guidelines() +
  geom_col(fill = "brown") +
  scale_y_data_rtgs(max_tot_vol_base, by = 5e5, scale = 1e-3) +
  scale_x_discrete(labels = waktu_labels) +
  p_tot[[3]]$labels + base_theme
p_tot[[3]]$graph



# B.2.4. Visualization - Volume facet tahun -------------------------------
p_tot[[4]]$path  <- here("figures/exploratory/fig_total_volume_facet_tahun.png")
p_tot[[4]]$labels<- labs(
  title = "Aliran Transaksi Sistem BI-RTGS Sepanjang Hari",
  subtitle = "Data Volume Transaksi (Jan 2016 - Jan 2021)",
  x = "Waktu", y = "Volume (dalam ribu)"
) 
p_tot[[4]]$graph <- df_total %>%
  ggplot(aes(x = waktu, y = volume, fill = factor(year(bulan)))) +
  geom_throughput_guidelines() +
  geom_col() +
  scale_y_data_rtgs(max_tot_vol_by_thn, by = 2.5e5, scale = 1e-3) +
  scale_x_waktu(2) +
  scale_fill_brewer(palette = "Set1", guide = F) +
  facet_wrap(.~year(bulan)) +
  p_tot[[4]]$labels + base_theme
p_tot[[4]]$graph



# B.2.5. Save df_total Plots ----------------------------------------------
walk(p_tot, 
     ~ ggsave(.$path, .$graph, width = 13.333, height = 7.5, units = "in"))





# B.3. Data by kelompok ---------------------------------------------------
kelompok_levels <- df_by_kelompok %>% 
  pull(kelompok) %>% levels

max_kel_nom_base <- df_by_kelompok %>% # data tidak diambil dari df_total
  group_by(waktu) %>% summarise(nominal = sum(nominal)) %>%
  summarise(max(nominal)) %>% pull
max_kel_vol_base <- df_by_kelompok %>% 
  group_by(waktu) %>% summarise(volume = sum(volume)) %>%
  summarise(max(volume)) %>% pull

max_kel_nom_by_kel <- df_by_kelompok %>%
  group_by(kelompok, waktu) %>% summarise(nominal = sum(nominal)) %>%
  ungroup() %>% summarise(max(nominal)) %>% pull
max_kel_nom_by_kel_no_bi <- df_by_kelompok %>%
  filter(kelompok != "Bank Sentral") %>%
  group_by(kelompok, waktu) %>% summarise(nominal = sum(nominal)) %>%
  ungroup() %>% summarise(max(nominal)) %>% pull
max_kel_vol_by_kel <- df_by_kelompok %>%
  group_by(kelompok, waktu) %>% summarise(volume = sum(volume)) %>%
  ungroup() %>% summarise(max(volume)) %>% pull


p_kel <- vector("list", 4)



# B.3.1. Data by kelompok - Summary ---------------------------------------

p_kel[[1]] <- list(
  path = here("figures/exploratory/fig_by_kel_summary.png"),
  labels = vector("list", length = 4),
  graphs = vector("list", length = 4),
  graph  = NA
)

p_kel[[1]]$labels[[1]] <- labs(
  title = "Aliran Transaksi Sistem BI-RTGS Sepanjang Hari",
  subtitle="Data  \"NOMINAL\"  Transaksi " %>%
    str_c("(Jan 2016 - Jan 2021)"),
  x = "Waktu", 
  y = "Nominal (dalam triliun)", 
  fill = "Kelompok Bank"
) 
p_kel[[1]]$graphs[[1]] <- df_by_kelompok %>%
  ggplot(aes(x = waktu, y = nominal, fill = kelompok)) +
  geom_throughput_guidelines() +
  geom_col() +
  scale_y_data_rtgs(max_kel_nom_base, by = 2.5e16, scale = 1e-12) +
  scale_x_waktu(2) +
  scale_fill_brewer(palette = "Set1") +
  p_kel[[1]]$labels[[1]]  + base_theme
p_kel[[1]]$graphs[[1]] 

p_kel[[1]]$labels[[2]] <- labs(
  title = "Proporsi Transaksi Kelompok Bank Sistem BI-RTGS",
  subtitle="Data  \"NOMINAL\"  Transaksi " %>%
    str_c("(Jan 2016 - Jan 2021)"),
  x = "Waktu", 
  y = "Nominal (dalam triliun)", 
  fill = "Kelompok Bank"
) 
p_kel[[1]]$graphs[[2]] <- df_by_kelompok %>%
  ggplot(aes(x = waktu, y = nominal, fill = kelompok)) +
  geom_col(position = position_fill()) +
  scale_x_waktu(2) +
  scale_fill_brewer(palette = "Set1") +
  p_kel[[1]]$labels[[2]] + base_theme
p_kel[[1]]$graphs[[3]] 

p_kel[[1]]$labels[[3]] <- labs(
  title = "Aliran Transaksi Sistem BI-RTGS Sepanjang Hari",
  subtitle="Data  \"VOLUME\"  Transaksi " %>%
    str_c("(Jan 2016 - Jan 2021)"),
  x = "Waktu", 
  y = "Volume (dalam ribu)", 
  fill = "Kelompok Bank"
) 
p_kel[[1]]$graphs[[3]] <- df_by_kelompok %>%
  ggplot(aes(x = waktu, y = volume, fill = kelompok)) +
  geom_throughput_guidelines() +
  geom_col() +
  scale_y_data_rtgs(max_kel_vol_base, by = 5e5, scale = 1e-3) +
  scale_x_waktu(2) +
  scale_fill_brewer(palette = "Set1") +
  p_kel[[1]]$labels[[3]] + base_theme
p_kel[[1]]$graphs[[3]]

p_kel[[1]]$labels[[4]] <- labs(
  title = "Proporsi Transaksi Kelompok Bank Sistem BI-RTGS",
  subtitle="Data  \"VOLUME\"  Transaksi " %>%
    str_c("(Jan 2016 - Jan 2021)"),
  x = "Waktu", 
  y = "Volume (dalam ribu)", 
  fill = "Kelompok Bank"
) 
p_kel[[1]]$graphs[[4]] <- df_by_kelompok %>%
  ggplot(aes(x = waktu, y = volume, fill = kelompok)) +
  geom_col(position = position_fill()) +
  scale_x_waktu(2) +
  scale_fill_brewer(palette = "Set1") +
  p_kel[[1]]$labels[[4]] + base_theme 
p_kel[[1]]$graphs[[4]]

p_kel[[1]]$graph <- (
  p_kel[[1]]$graphs[[1]] + p_kel[[1]]$graphs[[3]] +
    p_kel[[1]]$graphs[[2]] + p_kel[[1]]$graphs[[4]]
)



# B.3.2. Data by kelompok - Nominal facet kelompok ------------------------

p_kel[[2]]$path  <- here(
  "figures/exploratory/fig_by_kel_nominal_facet_kelompok.png")
p_kel[[2]]$labels<- labs(
  title = "Aliran Transaksi Sistem BI-RTGS Sepanjang Hari",
  subtitle="Data  \"NOMINAL\"  Transaksi " %>%
    str_c("(Jan 2016 - Jan 2021)"),
  x = "Waktu", 
  y = "Nominal (dalam triliun)", 
  fill = "Kelompok Bank"
) 
p_kel[[2]]$graph <- df_by_kelompok %>%
  ggplot(aes(x = waktu, y = nominal, fill = kelompok)) +
  geom_throughput_guidelines() +
  geom_col() +
  scale_y_data_rtgs(max_kel_nom_by_kel, by = 5e16, scale = 1e-12) +
  scale_x_waktu(3) +
  scale_fill_brewer(palette = "Set1") +
  facet_wrap(.~kelompok, ncol = 3) +
  p_kel[[2]]$labels + base_theme
p_kel[[2]]$graph


p_kel[[3]]$path  <- here(
  "figures/exploratory/fig_by_kel_nominal_facet_kelompok_no_BI.png")
p_kel[[3]]$labels<- labs(
  title = "Aliran Transaksi BI-RTGS Sepanjang Hari TANPA BANK SENTRAL",
  subtitle="Data  \"NOMINAL\"  Transaksi " %>%
    str_c("(Jan 2016 - Jan 2021)"),
  x = "Waktu", 
  y = "Nominal (dalam triliun)", 
  fill = "Kelompok Bank"
) 
p_kel[[3]]$graph <- df_by_kelompok %>%
  filter(kelompok != "Bank Sentral") %>%
  ggplot(aes(x = waktu, y = nominal, fill = kelompok)) +
  geom_throughput_guidelines() +
  geom_col() +
  scale_y_data_rtgs(max_kel_nom_by_kel_no_bi, by = 5e15, scale = 1e-12) +
  scale_x_waktu(2) +
  scale_fill_brewer(palette = "Set1") +
  facet_wrap(.~kelompok, ncol = 4) +
  p_kel[[3]]$labels + base_theme
p_kel[[3]]$graph



# B.3.1. Data by kelompok - Volume facet kelompok -------------------------

p_kel[[4]]$path  <- here(
  "figures/exploratory/fig_by_kel_volume_facet_kelompok.png")
p_kel[[4]]$labels<- labs(
  title = "Aliran Transaksi Sistem BI-RTGS Sepanjang Hari",
  subtitle="Data  \"VOLUME\"  Transaksi " %>%
    str_c("(Jan 2016 - Jan 2021)"),
  x = "Waktu", 
  y = "Volume (dalam ribu)", 
  fill = "Kelompok Bank"
) 
p_kel[[4]]$graph <- df_by_kelompok %>%
  ggplot(aes(x = waktu, y = volume, fill = kelompok)) +
  geom_throughput_guidelines() +
  geom_col() +
  scale_y_data_rtgs(max_kel_vol_by_kel, by = 2.5e5, scale = 1e-3) +
  scale_x_waktu(2) +
  scale_fill_brewer(palette = "Set1") +
  facet_wrap(.~kelompok, ncol=3, scales = "fixed") +
  p_kel[[4]]$labels + base_theme
p_kel[[4]]$graph



# B.3.5. Save df_by_kelompok Plots ----------------------------------------
walk(p_kel, 
     ~ ggsave(.$path, .$graph, width = 13.333, height = 7.5, units = "in"))





# B.4. Data by buku -------------------------------------------------------
max_buk_nom_base <- df_by_buku %>% # data tidak diambil dari df_total
  group_by(waktu) %>% summarise(nominal = sum(nominal)) %>%
  summarise(max(nominal)) %>% pull
max_buk_nom_by_kel <- df_by_buku %>%
  group_by(kelompok, waktu) %>% summarise(nominal = sum(nominal)) %>%
  ungroup() %>% summarise(max(nominal)) %>% pull

max_buk_vol_base <- df_by_buku %>% 
  group_by(waktu) %>% summarise(volume = sum(volume)) %>%
  summarise(max(volume)) %>% pull
max_buk_vol_by_kel <- df_by_buku %>%
  group_by(kelompok, waktu) %>% summarise(volume = sum(volume)) %>%
  ungroup() %>% summarise(max(volume)) %>% pull

p_buk <- vector("list", 3)



# B.4.1. Data by buku - Summary -------------------------------------------

p_buk[[1]] <- list(
  path = here("figures/exploratory/fig_by_buk_summary.png"),
  labels = vector("list", length = 4),
  graphs = vector("list", length = 4),
  graph  = NA
)

p_buk[[1]]$labels[[1]] <- labs(
  title = "Aliran Transaksi Sistem BI-RTGS Sepanjang Hari",
  subtitle="Data  \"NOMINAL\"  Transaksi " %>%
    str_c("(Jan 2016 - Jan 2021)"),
  x = "Waktu", 
  y = "Nominal (dalam triliun)", 
  fill = "Kelompok Bank"
) 
p_buk[[1]]$graphs[[1]] <- df_by_buku %>%
  ggplot(aes(x = waktu, y = nominal, fill = kelompok)) +
  geom_throughput_guidelines() +
  geom_col() +
  scale_y_data_rtgs(max_buk_nom_base, by = 2.5e16, scale = 1e-12) +
  scale_x_waktu(2) +
  scale_fill_brewer(palette = "Set1") +
  p_buk[[1]]$labels[[1]]  + base_theme
p_buk[[1]]$graphs[[1]] 

p_buk[[1]]$labels[[2]] <- labs(
  title = "Proporsi Transaksi Kelompok Bank Sistem BI-RTGS",
  subtitle="Data  \"NOMINAL\"  Transaksi " %>%
    str_c("(Jan 2016 - Jan 2021)"),
  x = "Waktu", 
  y = "Nominal (dalam triliun)", 
  fill = "Kelompok Bank"
) 
p_buk[[1]]$graphs[[2]] <- df_by_buku %>%
  ggplot(aes(x = waktu, y = nominal, fill = kelompok)) +
  geom_col(position = position_fill()) +
  scale_x_waktu(2) +
  scale_fill_brewer(palette = "Set1") +
  p_buk[[1]]$labels[[2]] + base_theme
p_buk[[1]]$graphs[[3]] 

p_buk[[1]]$labels[[3]] <- labs(
  title = "Aliran Transaksi Sistem BI-RTGS Sepanjang Hari",
  subtitle="Data  \"VOLUME\"  Transaksi " %>%
    str_c("(Jan 2016 - Jan 2021)"),
  x = "Waktu", 
  y = "Volume (dalam ribu)", 
  fill = "Kelompok Bank"
) 
p_buk[[1]]$graphs[[3]] <- df_by_buku %>%
  ggplot(aes(x = waktu, y = volume, fill = kelompok)) +
  geom_throughput_guidelines() +
  geom_col() +
  scale_y_data_rtgs(max_buk_vol_base, by = 5e5, scale = 1e-3) +
  scale_x_waktu(2) +
  scale_fill_brewer(palette = "Set1") +
  p_buk[[1]]$labels[[3]] + base_theme
p_buk[[1]]$graphs[[3]]

p_buk[[1]]$labels[[4]] <- labs(
  title = "Proporsi Transaksi Kelompok Bank Sistem BI-RTGS",
  subtitle="Data  \"VOLUME\"  Transaksi " %>%
    str_c("(Jan 2016 - Jan 2021)"),
  x = "Waktu", 
  y = "Volume (dalam ribu)", 
  fill = "Kelompok Bank"
) 
p_buk[[1]]$graphs[[4]] <- df_by_buku %>%
  ggplot(aes(x = waktu, y = volume, fill = kelompok)) +
  geom_col(position = position_fill()) +
  scale_x_waktu(2) +
  scale_fill_brewer(palette = "Set1") +
  p_buk[[1]]$labels[[4]] + base_theme 
p_buk[[1]]$graphs[[4]]

p_buk[[1]]$graph <- (
  p_buk[[1]]$graphs[[1]] + p_buk[[1]]$graphs[[3]] +
    p_buk[[1]]$graphs[[2]] + p_buk[[1]]$graphs[[4]]
)



# B.4.2. Data by buku - Nominal facet buku --------------------------------

p_buk[[2]]$path  <- here(
  "figures/exploratory/fig_by_buk_nominal_facet_kelompokbuku.png")
p_buk[[2]]$labels<- labs(
  title = "Aliran Transaksi Sistem BI-RTGS Sepanjang Hari",
  subtitle="Data  \"NOMINAL\"  Transaksi " %>%
    str_c("(Jan 2016 - Jan 2021)"),
  x = "Waktu", 
  y = "Nominal (dalam triliun)", 
  fill = "Kelompok Bank"
) 
p_buk[[2]]$graph <- df_by_buku %>%
  ggplot(aes(x = waktu, y = nominal, fill = kelompok)) +
  geom_throughput_guidelines() +
  geom_col() +
  scale_y_data_rtgs(max_buk_nom_by_kel, by = 5e16, scale = 1e-12) +
  scale_x_waktu(3) +
  scale_fill_brewer(palette = "Set1") +
  facet_wrap(.~kelompok) +
  p_buk[[2]]$labels + base_theme
p_buk[[2]]$graph



# B.4.3. Data by buku - Volume facet buku ---------------------------------
p_buk[[3]]$path  <- here(
  "figures/exploratory/fig_by_buk_volume_facet_kelompokbuku.png")
p_buk[[3]]$labels<- labs(
  title = "Aliran Transaksi Sistem BI-RTGS Sepanjang Hari",
  subtitle="Data  \"VOLUME\"  Transaksi " %>%
    str_c("(Jan 2016 - Jan 2021)"),
  x = "Waktu", 
  y = "Volume (dalam ribu)", 
  fill = "Kelompok Bank"
) 
p_buk[[3]]$graph <- df_by_buku %>%
  ggplot(aes(x = waktu, y = volume, fill = kelompok)) +
  geom_throughput_guidelines() +
  geom_col() +
  scale_y_data_rtgs(max_buk_vol_by_kel, by = 2.5e5, scale = 1e-3) +
  scale_x_waktu(2) +
  scale_fill_brewer(palette = "Set1") +
  facet_wrap(.~kelompok, scales = "fixed") +
  p_buk[[3]]$labels + base_theme
p_buk[[3]]$graph



# B.4.4. Save df_by_buku Plots --------------------------------------------
walk(p_buk, 
     ~ ggsave(.$path, .$graph, width = 13.333, height = 7.5, units = "in"))
