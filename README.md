# Tugas-2-16231019
Tugas 2 PSD

#exersice 1
Ada tiga jenis pendapatan yang dilaporkan dalam data frame ini: p25th, median, dan p75th. Ketiganya masing-masing merujuk pada persentil ke-25, ke-50, dan ke-75 dari distribusi pendapatan individu yang diambil sampelnya untuk suatu jurusan tertentu. Mengapa kita sering memilih median daripada mean untuk menggambarkan pendapatan tipikal suatu kelompok?

#Answer 
Median lebih sering digunakan daripada mean untuk menggambarkan rata-rata pendapatan karena tidak terpengaruh oleh angka yang terlalu tinggi atau terlalu rendah, lebih mencerminkan kondisi kebanyakan orang, lebih mudah dipahami, dan lebih stabil untuk perbandingan antar kelompok. Dalam data pendapatan yang sering kali tidak merata, median lebih akurat dan adil dibandingkan mean, yang bisa terpengaruh oleh segelintir orang dengan pendapatan sangat tinggi atau sangat rendah.

```{r}
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

#exercise 2
Membuat ulang visualisasi pada soal menggunakan bin $5000

#answer
Bin 5000 memberikan visualisasi yang lebih ringkas dan mudah diinterpretasikan sehingga sesuai untuk melihat pola umum dalam distribusi pendapatan median Sementara itu bin 1000 menampilkan detail lebih spesifik namun dapat menghasilkan visualisasi yang lebih kompleks jika data memiliki variasi yang tinggi Pemilihan binwidth sebaiknya disesuaikan dengan tujuan analisis Jika ingin mengamati tren keseluruhan gunakan 5000 Jika ingin melihat variasi yang lebih detail gunakan 1000
```{r}
library(tidyverse)
library(scales)
library(fivethirtyeight)

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

#exersice 3 
membuat ulang visualisasi exercise 2 dengan bin $1000 lalu dibandingkan

```{r}
library(tidyverse)
library(scales)
library(fivethirtyeight)

glimpse(college_recent_grads)

college_recent_grads_filtered <- college_recent_grads %>%
  filter(employed_fulltime_yearround > 0,
         major_category %in% c("Biology & Life Science",
                               "Computers & Mathematics",
                               "Engineering",
                               "Physical Sciences"))

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
#exercise 4
Jurusan STEM mana (yaitu, jurusan dalam kategori "Biology & Life Science", "Computers & Mathematics", "Engineering", dan "Physical Sciences") yang memiliki gaji median yang sama dengan atau lebih rendah dari median untuk seluruh jurusan (semua jurusan, bukan hanya yang termasuk dalam kategori STEM)? Output Anda hanya boleh menampilkan nama jurusan serta pendapatan median, persentil ke-25, dan persentil ke-75 untuk jurusan tersebut, dan harus diurutkan sehingga jurusan dengan pendapatan median tertinggi berada di bagian atas.

#answer
```{r}
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

jurusan_stem_text <- glue_collapse(filtered_stem$major, sep = ", ", last = " dan ")
cat("Jurusan STEM dengan pendapatan median ≤ median keseluruhan:", jurusan_stem_text, "\n")

ggplot(filtered_stem, aes(x = reorder(major, median), y = median)) +
  geom_col(fill = "pink") +
  coord_flip() +
  labs(title = "Jurusan STEM dengan Pendapatan Median ≤ Median Keseluruhan",
       x = "Jurusan", y = "Pendapatan Median") +
  theme_minimal()
```

#exercise 5
Buatlah sebuah pertanyaan yang menarik bagi Anda yang dapat dijawab menggunakan setidaknya tiga variabel dari dataset, lalu jawab pertanyaan tersebut menggunakan statistik ringkasan dan/atau visualisasi.

#answer
Apakah ada perbedaan besar dalam tingkat pengangguran antar kategori jurusan, dan bagaimana hubungannya dengan pendapatan median?

```{r}
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
#interpretasi
Grafik ini menunjukkan bahwa tidak ada hubungan yang jelas antara tingkat pengangguran dan pendapatan median berdasarkan kategori jurusan. Sebagian besar jurusan memiliki tingkat pengangguran di bawah 10% dengan pendapatan berkisar antara $30,000 hingga $60,000. Beberapa jurusan tetap memiliki pendapatan tinggi meskipun tingkat penganggurannya rendah, menunjukkan bahwa faktor lain, seperti sektor industri, mungkin lebih berpengaruh terhadap pendapatan lulusan.
