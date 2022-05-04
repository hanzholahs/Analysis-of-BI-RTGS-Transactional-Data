# desc  : Throughput zones analysis menggunakan data PCPM
# author: Hanzholah Shobri
# creation date : 2021-06-17


# Set Up ------------------------------------------------------------------

# A. Import Libraries -----------------------------------------------------

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



# B. Set Custom Functions -------------------------------------------------


fct_waktu2zona <- function(f) {
  fct_collapse(
    f,
    "Zona 1" = c(#"<05:30", "05:30 - <06:00", "06:00 - <06:30", 
      "06:30 - <07:00",
      "07:00 - <07:30", "07:30 - <08:00", "08:00 - <08:30",
      "08:30 - <09:00", "09:00 - <09:30", "09:30 - <10:00"),
    "Zona 2" = c("10:00 - <10:30", "10:30 - <11:00", "11:00 - <11:30",
                 "11:30 - <12:00", "12:00 - <12:30", "12:30 - <13:00",
                 "13:00 - <13:30", "13:30 - <14:00"
    ),
    "Zona 3" = c("14:00 - <14:30", "14:30 - <15:00", "15:00 - <15:30",
                 "15:30 - <16:00", "16:00 - <16:30", "16:30 - <17:00",
                 "17:00 - <17:30", "17:30 - <18:00" #, "18:00 - <18:30",
                 #"18:30 - <19:00", ">= 19:00" 
    )
  )
}





# Import dan Tidy Data Per Kelompok ---------------------------------------

# A. Import data ----------------------------------------------------------

.path <- here("data/raw/Data PCPM/",
              "Data Transaksi Per Jam_Waktu Setelmen_Excl OM dan Pem_R3.xlsx")
excel_sheets(.path)

.raw_by_kelompok2 <- read_excel(.path, sheet = "Kelompok Bank", skip = 5)



# B. Tidy data ------------------------------------------------------------

# menghapus row yang tidak relevan
.del_kel <- (which(.raw_by_kelompok2[,1] == "All"):nrow(.raw_by_kelompok2))
df_by_kelompok2 <- .raw_by_kelompok %>%
  rename(kelompok = ...1, waktu = ...2) %>%
  slice(- .del_kel) %>% 
  mutate_at(vars(matches(month.abb)), as.numeric)

# memfilter data yang relevan
df_by_kelompok2 <- df_by_kelompok2 %>%
  tidyr::fill(kelompok) %>% 
  filter(waktu != "0000", waktu != "All")

df_by_kelompok2 %>% names
# Membagi data berdasarkan tipe angka (nominal dan volume)
df_by_kelompok2_nom <- df_by_kelompok2 %>%
  select(1:74) %>% 
  pivot_longer(-(1:2), names_to = "bulan", values_to = "nominal") 
df_by_kelompok2_vol <- df_by_kelompok2 %>%
  select(1:2, 75:146) %>%
  pivot_longer(-(1:2), names_to = "bulan", values_to = "volume")


df_by_kelompok2 <- df_by_kelompok2_nom %>%
  mutate(
    bulan = str_replace(bulan, "(\\.){1,3}\\d*", "") %>% 
      as.yearmon(),
    kelompok = factor(kelompok),
    waktu = factor(waktu) %>%
      fct_relevel(">= 19:00", after = Inf),
    volume = df_by_kelompok2_vol$volume,
    volume = ifelse(!is.na(volume), volume, 0),
    nominal = ifelse(!is.na(nominal), nominal, 0)
  ) %>% 
  select(kelompok, bulan, waktu, everything()) %>%
  arrange(kelompok, bulan, waktu) %>%
  group_by(kelompok, bulan, waktu) %>% 
  summarise(nominal = sum(nominal),  # kalkulasi untuk merger col 'Others'
            volume = sum(volume))    # karena pada data raw, ada dua



# C. Export Data ----------------------------------------------------------

write_csv(df_by_kelompok2, here("data/tidy/Data PCPM/tidy_by_kelompok.csv"))




# Import dan Tidy Data Per Buku -------------------------------------------

# A. Import data ----------------------------------------------------------

excel_sheets(.path)
.raw_by_buku2 <- read_excel(.path, sheet = "BUKU", skip = 5)



# B. Tidy data ------------------------------------------------------------

# menghapus row yang tidak relevan
.del_kel <- (which(.raw_by_buku2[,1] == "Total"):nrow(.raw_by_buku2))
df_by_buku2 <- .raw_by_buku %>%
  rename(kelompok = ...1, waktu = ...2) %>%
  slice(- .del_kel) %>% 
  mutate_at(vars(matches(month.abb)), as.numeric)

# memfilter data yang relevan
df_by_buku2 <- df_by_buku2 %>%
  tidyr::fill(kelompok) %>% 
  filter(waktu != "0000", waktu != "All")

df_by_buku2 %>% names
# Membagi data berdasarkan tipe angka (nominal dan volume)
df_by_buku2_nom <- df_by_buku2 %>%
  select(1:74) %>% 
  pivot_longer(-(1:2), names_to = "bulan", values_to = "nominal") 
df_by_buku2_vol <- df_by_buku2 %>%
  select(1:2, 75:146) %>%
  pivot_longer(-(1:2), names_to = "bulan", values_to = "volume")


df_by_buku2 <- df_by_buku2_nom %>%
  mutate(
    bulan = str_replace(bulan, "(\\.){1,3}\\d*", "") %>% 
      as.yearmon(),
    kelompok = factor(kelompok),
    waktu = factor(waktu) %>%
      fct_relevel(">= 19:00", after = Inf),
    volume = df_by_buku2_vol$volume,
    volume = ifelse(!is.na(volume), volume, 0),
    nominal = ifelse(!is.na(nominal), nominal, 0)
  ) %>% 
  select(kelompok, bulan, waktu, everything()) %>%
  arrange(kelompok, bulan, waktu) %>%
  group_by(kelompok, bulan, waktu) %>% 
  summarise(nominal = sum(nominal),  # kalkulasi untuk merger col 'Others'
            volume = sum(volume))    # karena pada data raw, ada dua



# C. Export Data ----------------------------------------------------------

write_csv(df_by_kelompok2, here("data/tidy/Data PCPM/tidy_by_buku.csv"))






# Analysis ----------------------------------------------------------------

# A. Persentase Vol dan Nom setiap Kelompok -------------------------------

dat_agg1 <- df_by_kelompok2 %>% 
  group_by(kelompok) %>% 
  summarise(nominal = sum(nominal),
            volume = sum(volume)) %>% 
  mutate(nominal = nominal/sum(nominal),
         volume = volume/sum(volume)) 

dat_agg1 %>%  mutate_if(is.numeric, scales::percent) %>% 
  arrange(desc(nominal))



# B. Throughput Gabungan --------------------------------------------------

# Persiapan Data

min_date <- as.yearmon("2016-01")
max_date <- as.yearmon("2021-04")
annotation_date <- (as.Date(max_date) - months(4)) %>% as.yearmon

kel_not_zero <- dat_agg1 %>% 
  filter(nominal != 0, 
         volume != 0) %>% 
  pull(kelompok) %>% 
  fct_drop()

dat_agg2 <- df_by_kelompok2 %>%
  mutate(zona = fct_waktu2zona(waktu)) %>% 
  filter(bulan <= max_date,
         zona %in% c("Zona 1", "Zona 2", "Zona 3"),
         kelompok %in% kel_not_zero,
         kelompok != "Bank Sentral",
         kelompok != "Others") %>% 
  group_by(bulan, zona) %>% 
  summarise(nominal = sum(nominal),
            volume = sum(volume)) %>%
  group_by(bulan) %>% 
  mutate(nominalprop = nominal/sum(nominal))

dat_agg2_slice <- dat_agg2 %>% 
  cbind(index = 1:nrow(dat_agg2)) %>% 
  filter(bulan >= as.yearmon("2019-01")) %>% 
  group_by(zona) %>% 
  mutate(ismin = nominalprop == min(nominalprop),
         ismax = nominalprop == max(nominalprop)) %>% 
  filter(ismin == TRUE | ismax == TRUE) %>% 
  pull(index)
  
dat_agg2_highlight <- dat_agg2[dat_agg2_slice,]


# Grafik pergerakan pola transaksi

ggplot(dat_agg2, aes(x = bulan, y = nominal, fill = fct_rev(zona))) +
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

# Grafik pola transaksi style PCPM

dat_agg2 %>% 
  filter(bulan >= as.yearmon("2019-01")) %>%
  ggplot(aes(x = bulan, y = nominalprop, colour = zona)) +
  annotate("rect", ymin = 0.3, ymax = 0.4,
           xmin = as.yearmon("2019-01"), xmax = as.yearmon("2021-04"), 
           fill = "pink", size = 1, alpha = .5) +
  annotate("rect", ymin = 0, ymax = 0.3,
           xmin = as.yearmon("2019-01"), xmax = as.yearmon("2021-04"), 
           fill = "yellow", size = 1, alpha = .5) +
  annotate("segment", x = as.yearmon("2020-03"), xend = as.yearmon("2020-03"),
           y = 0, yend = 0.615, size = 1.5, colour = "grey20", alpha = 0.5) +
  annotate("text", label = "Injeksi Likuiditas Rp 300T\npada Awal Covid-19",
           x = as.yearmon("2020-03"), y = 0.675, vjust = 1,
           fontface = "bold") +
  geom_line(size = 1.2) + 
  geom_point(size = 2.2) +
  geom_point(data = dat_agg2_highlight, 
             aes(x = bulan, y = nominalprop),
             colour = "black") +
  geom_text_repel(data = dat_agg2_highlight, 
            aes(x = bulan, y = nominalprop, 
                label = scales::percent(nominalprop)),
            colour = "black") +
  scale_colour_manual(values = c("green3", "orange", "red")) +
  scale_x_yearmon(
    breaks = seq(as.Date("2019-01-01"), as.Date("2021-04-01"), "month") %>% 
      as.yearmon()
  ) +
  scale_y_continuous(breaks = seq(0, 0.6, 0.1),
                     labels = scales::percent_format(accuracy = 1),
                     expand = c(0,0)) +
  labs(x = NULL, y = NULL) +
  theme_tufte() +
  theme(
    axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1),
    axis.ticks = element_blank(),
    panel.grid.major.x = element_line(colour = "grey80", size = 1),
    panel.grid.major.y = element_line(colour = "grey80", size = 1)
  )



# C. Throughput Per Kelompok ----------------------------------------------

# Persiapan Data

dat_kel1 <- df_by_kelompok2 %>%
  mutate(zona = fct_waktu2zona(waktu)) %>% 
  filter(bulan <= max_date,
         zona %in% c("Zona 1", "Zona 2", "Zona 3"),
         kelompok %in% kel_not_zero,
         kelompok != "Bank Sentral",
         kelompok != "Others") %>% 
  group_by(kelompok, bulan, zona) %>% 
  summarise(nominal = sum(nominal),
            volume = sum(volume)) %>% 
  ungroup()
dat_kel1


# Grafik pergerakan pola transaksi

dat_kel1 %>%
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
  scale_fill_brewer(palette = "Greens") +
  labs(title = "Pola Transaksi Peserta BI-RTGS (Jan 2016 - Apr 2021)", 
       fill = "Zona Throughput") +
  theme_tufte(base_size = 11) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 60, hjust = 1),
        axis.ticks = element_line(colour = "grey50", size = 0.7))





# C. Throughput Per Kelompok ----------------------------------------------

# Persiapan Data

dat_buk1 <- df_by_buku2 %>%
  mutate(zona = fct_waktu2zona(waktu)) %>% 
  filter(bulan <= max_date,
         zona %in% c("Zona 1", "Zona 2", "Zona 3")) %>% 
  group_by(kelompok, bulan, zona) %>% 
  summarise(nominal = sum(nominal),
            volume = sum(volume)) %>% 
  ungroup()
dat_buk1


# Grafik pergerakan pola transaksi

dat_buk1 %>%
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
  scale_fill_brewer(palette = "Greens") +
  labs(title = "Pola Transaksi Peserta BI-RTGS (Jan 2016 - Apr 2021)", 
       fill = "Zona Throughput") +
  theme_tufte(base_size = 12) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 60, hjust = 1),
        axis.ticks = element_line(colour = "grey50", size = 0.7))
