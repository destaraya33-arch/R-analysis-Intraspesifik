Tinggi_SPSS <- data %>%
  group_by(
    perlakuan,
    `Minggu ke`
  ) %>%
  summarise(
    Mean = mean(`Tinggi tanaman`, na.rm = TRUE),
    
    Std_Deviation = sd(
      `Tinggi tanaman`,
      na.rm = TRUE
    ),
    
    N = n(),
    
    .groups = "drop"
  )

View(Tinggi_SPSS)
Grafik_Tabel <- Tinggi_SPSS %>%
  select(
    `Minggu ke`,
    perlakuan,
    Mean
  ) %>%
  pivot_wider(
    names_from = perlakuan,
    values_from = Mean
  )

View(Grafik_Tabel)
Grafik_Tinggi <- ggplot(
  Tinggi_SPSS,
  aes(
    x = `Minggu ke`,
    y = Mean,
    color = perlakuan,
    group = perlakuan
  )
)+
  geom_line(
    linewidth = 1
  )+
  geom_point(
    size = 3
  )+
  labs(
    title = "Tinggi Tanaman",
    x = "Minggu",
    y = "Rata-rata Tinggi"
  )+
  theme_classic()

Grafik_Tinggi