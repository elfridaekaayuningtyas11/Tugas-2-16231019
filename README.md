---
title: "Tugas 2 16231019"
output: html_document
---

# Tugas 2 PSD

## Exercise 1

Ada tiga jenis pendapatan yang dilaporkan dalam data frame ini: `p25th`, `median`, dan `p75th`. Ketiganya masing-masing merujuk pada persentil ke-25, ke-50, dan ke-75 dari distribusi pendapatan individu yang diambil sampelnya untuk suatu jurusan tertentu. Mengapa kita sering memilih median daripada mean untuk menggambarkan pendapatan tipikal suatu kelompok?

### Jawaban

Median lebih sering digunakan daripada mean untuk menggambarkan rata-rata pendapatan karena tidak terpengaruh oleh angka yang terlalu tinggi atau terlalu rendah. Median lebih mencerminkan kondisi kebanyakan orang, lebih mudah dipahami, dan lebih stabil untuk perbandingan antar kelompok. Dalam data pendapatan yang sering kali tidak merata, median lebih akurat dan adil dibandingkan mean, yang bisa terpengaruh oleh segelintir orang dengan pendapatan sangat tinggi atau sangat rendah.

```
library(tidyverse)
library(fivethirtyeight)

glimpse(college_recent_grads)

mean_income <- mean(college_recent_grads$median, na.rm = TRUE)
median_income <- median(college_recent_grads$median, na.rm = TRUE)

mean_income
median_income

income_long <- college_recent_grads %>%
  select(major_category, p25th, median, p75th) %>%
  pivot_longer(cols = c(p25th, median, p75th),
               names_to = "persentil",
               values_to = "income")

ggplot(income_long, aes(x = income, fill = persentil)) +
  geom_density(alpha = 0.5) +
  scale_x_log10() +  
  geom_vline(xintercept = mean_income, color = "red", linetype = "dashed", size = 1, alpha = 0.8) +
  geom_vline(xintercept = median_income, color = "blue", linetype = "dashed", size = 1, alpha = 0.8) +
  labs(
    title = "Distribusi Pendapatan Berdasarkan Persentil",
    subtitle = "Garis merah = mean, garis biru = median",
    x = "Pendapatan ($, log scale)",
    y = "Density"
  ) +
  theme_minimal()
```

---

## Exercise 2

Buat ulang visualisasi berikut. Catatan: Lebar bin yang digunakan adalah $5.000. Perhatikan dengan cermat teks dan label pada sumbu.


```
library(tidyverse)
library(scales)
library(fivethirtyeight)
library(scales)

glimpse(college_recent_grads)

college_recent_grads_filtered <- college_recent_grads %>%
  filter(employed_fulltime_yearround > 0,
         major_category %in% c("Biology & Life Science",
                               "Computers & Mathematics",
                               "Engineering",
                               "Physical Sciences"))

ggplot(college_recent_grads_filtered, aes(x = median, fill = major_category)) +
  geom_histogram(binwidth = 5000, alpha = 0.7, show.legend = FALSE) +
  facet_wrap(~ major_category, nrow = 4) +  
  scale_x_continuous(labels = label_dollar()) + 
  labs(
    x = "Median earnings",
    y = "Frequency",
    title = "Median earnings of full-time, year-round workers",
    subtitle = "For STEM majors"
  ) +
  theme_minimal()
```

---

## Exercise 3

Buat ulang visualisasi dari latihan sebelumnya, kali ini dengan lebar bin sebesar $1.000. Mana yang lebih baik antara $1.000 atau $5.000 sebagai pilihan lebar bin? Jelaskan alasan Anda dalam satu kalimat.

```
library(tidyverse)
library(scales)
library(fivethirtyeight)

glimpse(college_recent_grads)

ggplot(college_recent_grads_filtered, aes(x = median, fill = major_category)) +
  geom_histogram(binwidth = 1000, alpha = 0.7, show.legend = FALSE) +
  facet_wrap(~ major_category, nrow = 4) +  
  scale_x_continuous(labels = label_dollar()) + 
  labs(
    x = "Median earnings",
    y = "Frequency",
    title = "Median earnings of full-time, year-round workers",
    subtitle = "For STEM majors"
  ) +
  theme_minimal()
```

### Jawaban

Bin $5000 memberikan visualisasi yang lebih ringkas dan mudah diinterpretasikan untuk melihat pola umum dalam distribusi pendapatan median, sementara Bin $1000 menampilkan detail lebih spesifik namun bisa membuat visualisasi lebih kompleks jika data memiliki variasi tinggi, sehingga pemilihan binwidth sebaiknya disesuaikan dengan tujuan analisis.

---

## Exercise 4 

Jurusan STEM mana (yaitu, jurusan dalam kategori "Biology & Life Science", "Computers & Mathematics", "Engineering", dan "Physical Sciences") yang memiliki gaji median yang sama dengan atau lebih rendah dari median untuk seluruh jurusan (semua jurusan, bukan hanya yang termasuk dalam kategori STEM)? Output Anda hanya boleh menampilkan nama jurusan serta pendapatan median, persentil ke-25, dan persentil ke-75 untuk jurusan tersebut, dan harus diurutkan sehingga jurusan dengan pendapatan median tertinggi berada di bagian atas.

```
library(tidyverse)
library(fivethirtyeight)
library(glue)
library(ggplot2)

glimpse(college_recent_grads)

stem_categories <- c("Biology & Life Science", "Computers & Mathematics", "Engineering", "Physical Sciences")

median_all <- median(college_recent_grads$median, na.rm = TRUE)

filtered_stem <- college_recent_grads %>%
  filter(major_category %in% stem_categories, median <= median_all) %>%
  select(major, p25th, median, p75th) %>%
  arrange(desc(median))

ggplot(filtered_stem, aes(x = reorder(major, median), y = median)) +
  geom_col(fill = "pink") +
  coord_flip() +
  labs(title = "Jurusan STEM dengan Pendapatan Median ≤ Median Keseluruhan",
       x = "Jurusan", y = "Pendapatan Median") +
  theme_minimal()
```

---

## Exercise 5

Buatlah sebuah pertanyaan yang menarik bagi Anda yang dapat dijawab menggunakan setidaknya tiga variabel dari dataset, lalu jawab pertanyaan tersebut menggunakan statistik ringkasan dan/atau visualisasi.

### Jawaban

Apakah ada perbedaan besar dalam tingkat pengangguran antar kategori jurusan, dan bagaimana hubungannya dengan pendapatan median?

```
library(tidyverse)
library(fivethirtyeight)
library(scales)

glimpse(college_recent_grads)

ggplot(college_recent_grads, aes(x = unemployment_rate, y = median, color = major_category)) +
  geom_point(alpha = 0.7) +
  scale_x_continuous(labels = percent_format()) +  
  scale_y_continuous(labels = dollar_format()) +   
  labs(title = "Tingkat Pengangguran vs Pendapatan Median Berdasarkan Kategori Jurusan",
       x = "Tingkat Pengangguran",
       y = "Pendapatan Median",
       color = "Kategori Jurusan") +
  theme_minimal()
```

### Kesimpulan

Grafik ini menunjukkan bahwa tidak ada hubungan yang jelas antara tingkat pengangguran dan pendapatan median berdasarkan kategori jurusan. Sebagian besar jurusan memiliki tingkat pengangguran di bawah 10% dengan pendapatan berkisar antara $30,000 hingga $60,000. Beberapa jurusan tetap memiliki pendapatan tinggi meskipun tingkat penganggurannya rendah, menunjukkan bahwa faktor lain, seperti sektor industri, mungkin lebih berpengaruh terhadap pendapatan lulusan.

