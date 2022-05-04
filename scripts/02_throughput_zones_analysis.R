# desc  : Throughput zones analysis
# author: Hanzholah Shobri
# creation date : 2021-01-18
# last update   : 2021-05-10




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

fct_waktu2zona <- function(f) {
  stopifnot(mean(levels(f) == waktu_levels) == 1)
  
  fct_collapse(
    f,
    "Zona 1" = c(#"<05:30", "05:30 - <06:00", 
                 "06:00 - <06:30", "06:30 - <07:00",
                 "07:00 - <07:30", "07:30 - <08:00", "08:00 - <08:30",
                 "08:30 - <09:00", "09:00 - <09:30", "09:30 - <10:00"),
    "Zona 2" = c("10:00 - <10:30", "10:30 - <11:00", "11:00 - <11:30",
                 "11:30 - <12:00", "12:00 - <12:30", "12:30 - <13:00",
                 "13:00 - <13:30", "13:30 - <14:00"
                 ),
    "Zona 3" = c("14:00 - <14:30", "14:30 - <15:00", "15:00 - <15:30",
                 "15:30 - <16:00", "16:00 - <16:30", "16:30 - <17:00",
                 "17:00 - <17:30", "17:30 - <18:00"#, "18:00 - <18:30"
                 #"18:30 - <19:00", ">= 19:00" 
                 )
  )
}



# Prepare Data ------------------------------------------------------------

min_date <- as.yearmon("2016-01")
max_date <- as.yearmon("2021-04")
annotation_date <- as.yearmon(as.Date(max_date)-months(2))

df_zona <- df_by_kelompok %>%
  mutate(zona = fct_waktu2zona(waktu)) %>%
  filter(zona %in% c("Zona 1", "Zona 2", "Zona 3"),
         kelompok != "Bank Sentral",
         kelompok != "Others",
         bulan <= max_date) %>%
  group_by(bulan, zona) %>%
  summarise(volume = sum(volume),
            nominal = sum(nominal)) %>% 
  ungroup()

df_zona_kl <- df_by_kelompok %>%
  mutate(zona = fct_waktu2zona(waktu)) %>%
  filter(zona %in% c("Zona 1", "Zona 2", "Zona 3"),
         kelompok != "Bank Sentral",
         kelompok != "Others",
         bulan <= max_date) %>%
  group_by(bulan, zona, kelompok) %>%
  summarise(volume = sum(volume),
            nominal = sum(nominal)) %>% 
  ungroup()

df_zona_bk <- df_by_buku %>%
  mutate(zona = fct_waktu2zona(waktu)) %>%
  filter(zona %in% c("Zona 1", "Zona 2", "Zona 3"),
         bulan <= max_date) %>%
  group_by(bulan, zona, kelompok) %>%
  summarise(volume = sum(volume),
            nominal = sum(nominal)) %>% 
  ungroup()









# Data Visualization ------------------------------------------------------

p1 <- df_zona %>% 
  ggplot(aes(x = bulan, y = nominal, fill = fct_rev(zona))) +
  geom_area(position = position_fill()) +
  annotate("segment", y = 0.3, yend = 0.3,
           x = min_date, xend = max_date, 
           colour = "grey30", size = 1, alpha = .7) +
  annotate("segment", y = 0.6, yend = 0.6,
           x = min_date, xend = max_date, 
           colour = "grey30", size = 1, alpha = .7) +
  annotate("text", label = "30%", fontface = "bold", 
           x = annotation_date, y = 0.35) +
  annotate("text", label = "60%", fontface = "bold", 
           x = annotation_date, y = 0.65) +
  scale_x_yearmon(breaks = as.yearmon(seq(as.Date(min_date), 
                                          as.Date(max_date), 
                                          "3 month")),
                  expand = c(0, 0)) +
  theme_tufte() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 60, hjust = 1),
        axis.text = element_text(size = 12),
        axis.ticks = element_line(colour = "grey50", size = 0.7),
        title = element_text(size = 14))



p2 <- df_zona_kl %>%
  ggplot(aes(x = (bulan), y = nominal, fill = fct_rev(zona))) +
  geom_area(position = position_fill()) +
  annotate("segment", y = 0.3, yend = 0.3,
           x = min_date, xend = max_date, 
           colour = "grey30", size = 1, alpha = .7) +
  annotate("segment", y = 0.6, yend = 0.6,
           x = min_date, xend = max_date, 
           colour = "grey30", size = 1, alpha = .7) +
  annotate("text", label = "30%", fontface = "bold", 
           x = annotation_date, y = 0.35) +
  annotate("text", label = "60%", fontface = "bold", 
           x = annotation_date, y = 0.65) +
  scale_x_yearmon(breaks = as.yearmon(seq(as.Date(min_date), 
                                          as.Date(max_date), 
                                          "6 month")),
                  expand = c(0, 0)) +
  facet_wrap(kelompok~., ncol = 3)



p3 <- df_zona_bk %>%
  ggplot(aes(x = bulan, y = nominal, fill = fct_rev(zona))) +
  geom_area(position = position_fill()) +
  annotate("segment", y = 0.3, yend = 0.3,
           x = min_date, xend = max_date, 
           colour = "grey30", size = 1, alpha = .7) +
  annotate("segment", y = 0.6, yend = 0.6,
           x = min_date, xend = max_date, 
           colour = "grey30", size = 1, alpha = .7) +
  annotate("text", label = "30%", fontface = "bold", 
           x = annotation_date, y = 0.35) +
  annotate("text", label = "60%", fontface = "bold", 
           x = annotation_date, y = 0.65) +
  scale_x_yearmon(breaks = as.yearmon(seq(as.Date(min_date), 
                                          as.Date(max_date), 
                                          "6 month")),
                  expand = c(0, 0)) +
  facet_wrap(kelompok~.) +
  labs(fill = "Zona Throughput")



p4 <- ((p1 + p3) / p2) +
  plot_layout(guides = "collect", widths = c(4,2)) +
  plot_annotation(
    title = "Pola Transaksi Peserta BI-RTGS (Jan 2016 - Apr 2021)"
  ) &
  scale_y_continuous(
    breaks = seq(0,1,0.2),
    labels = scales::percent_format(),
    expand = c(0, 0)
  ) &
  scale_fill_brewer(palette = "Set3") &
  labs(fill = "Zona Throughput") &
  theme_tufte(base_size = 15) &
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 60, hjust = 1),
        axis.ticks = element_line(colour = "grey50", size = 0.7))
p4
ggsave(here("figures/Grafik Gabungan.png"), 
       plot = p4, width = 15, height = 10.5)


# Grafik untuk Laporan Kajian ---------------------------------------------

p1laporan <- p1 +
  scale_x_yearmon(breaks = as.yearmon(seq(as.Date(min_date), 
                                          as.Date(max_date), 
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

ggsave(here("figures/laporan_kajian/grafik_kumulatif.png"), plot = p1laporan,
         height = 12, width = 30, units = "cm")


p2laporan <- df_zona_kl %>%
  ggplot(aes(x = (bulan), y = nominal, fill = fct_rev(zona))) +
  geom_area(position = position_fill()) +
  annotate("segment", y = 0.3, yend = 0.3,
           x = min_date, xend = max_date, 
           colour = "grey30", size = 1, alpha = .7) +
  annotate("segment", y = 0.6, yend = 0.6,
           x = min_date, xend = max_date, 
           colour = "grey30", size = 1, alpha = .7) +
  annotate("text", label = "30%", fontface = "bold", 
           x = annotation_date, y = 0.35) +
  annotate("text", label = "60%", fontface = "bold", 
           x = annotation_date, y = 0.65) +
  facet_wrap(kelompok~., ncol = 3) +
  scale_x_yearmon(breaks = as.yearmon(seq(as.Date(min_date), 
                                          as.Date(max_date), 
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

ggsave(here("figures/laporan_kajian/grafik_per_jenis.png"), plot = p2laporan,
       height = 18, width = 30, units = "cm")



p3laporan <- df_zona_bk %>%
  ggplot(aes(x = bulan, y = nominal, fill = fct_rev(zona))) +
  geom_area(position = position_fill()) +
  annotate("segment", y = 0.3, yend = 0.3,
           x = min_date, xend = max_date, 
           colour = "grey30", size = 1, alpha = .7) +
  annotate("segment", y = 0.6, yend = 0.6,
           x = min_date, xend = max_date, 
           colour = "grey30", size = 1, alpha = .7) +
  annotate("text", label = "30%", fontface = "bold", 
           x = annotation_date, y = 0.35) +
  annotate("text", label = "60%", fontface = "bold", 
           x = annotation_date, y = 0.65) +
  facet_wrap(kelompok~.) +
  scale_x_yearmon(breaks = as.yearmon(seq(as.Date(min_date), 
                                          as.Date(max_date), 
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

ggsave(here("figures/laporan_kajian/grafik_per_buku.png"), plot = p3laporan,
       height = 15, width = 30, units = "cm")

p1laporan
p2laporan
p3laporan



# Menghitung jumlah nominal transaksi per kelompok ------------------------

df_zona_kl %>% 
  group_by(zona) %>% 
  summarise(total = sum(nominal)) %>% 
  mutate(total = total/sum(total)*100)

df_zona_kl %>% 
  group_by(kelompok) %>% 
  summarise(total = sum(nominal)) %>%
  data.frame

df_zona_bk %>% 
  group_by(kelompok) %>% 
  summarise(total = sum(nominal)) %>% 
  data.frame

  



# Perbandingan dengan Hasil Kerjaan PCPM ----------------------------------

df_by_kelompok %>%
  mutate(zona = fct_waktu2zona(waktu)) %>%
  filter(zona %in% c("Zona 1", "Zona 2", "Zona 3"),
         bulan >= as.yearmon("2019-01"),
         kelompok != "Bank Sentral",
         kelompok != "Others",
         bulan <= as.yearmon("2021-04")
         ) %>%
  group_by(bulan, zona) %>%
  summarise(volume = sum(volume),
            nominal = sum(nominal)) %>%
  group_by(bulan) %>% 
  mutate(volume = volume/sum(volume),
         nominal = nominal/sum(nominal)) %>%
  ggplot(aes(x = bulan, y = nominal, colour = zona)) +
  annotate("rect", ymin = 0.3, ymax = 0.4,
           xmin = as.yearmon("2019-01"), xmax = as.yearmon("2021-04"), 
           fill = "pink", size = 1, alpha = .5) +
  annotate("rect", ymin = 0, ymax = 0.3,
           xmin = as.yearmon("2019-01"), xmax = as.yearmon("2021-04"), 
           fill = "yellow", size = 1, alpha = .5) +
  annotate("segment", x = as.yearmon("2020-03"), xend = as.yearmon("2020-03"),
           y = 0, yend = Inf, size = 1.5, colour = "grey50", alpha = 0.5) +
  # annotate("rect", xmin = as.yearmon("2020-04"), xmax = as.yearmon("2020-10"),
  #          ymin = 0.5, ymax = 0.6, fill = "grey90", colour = "grey90", 
  #          size = 4) +
  # annotate("text", label = "Injeksi Likuiditas Rp 300T\npada Awal Covid-19",
  #          x = as.yearmon("2020-07"), y = 0.55, hjust = 0.5, 
  #          fontface = "bold") +
  geom_line(size = 1.2) + 
  geom_point(size = 2.2) +
  scale_colour_manual(values = c("green3", "orange", "red")) +
  scale_x_yearmon(
    breaks = seq(as.Date("2019-01-01"), as.Date("2021-04-01"), "month") %>% 
      as.yearmon()
  ) +
  scale_y_continuous(limits = c(0, 0.55),
                     breaks = seq(0, 0.6, 0.1),
                     labels = scales::percent_format(accuracy = 1)) +
  labs(x = NULL, y = NULL) +
  theme_tufte() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    panel.grid.major.x = element_line(colour = "grey80", size = 1),
    panel.grid.major.y = element_line(colour = "grey80", size = 1)
  )