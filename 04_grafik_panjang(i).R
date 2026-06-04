Panjang_SPSS <- data %>%
  group_by(
    perlakuan,
    `Minggu ke`
  ) %>%
  summarise(
    Mean = mean(
      `Panjang daun`,
      na.rm = TRUE
    ),
    
    Std_Deviation = sd(
      `Panjang daun`,
      na.rm = TRUE
    ),
    
    N = n(),
    
    .groups = "drop"
  )

View(Panjang_SPSS)
Grafik_Panjang <- Panjang_SPSS %>%
  select(
    `Minggu ke`,
    perlakuan,
    Mean
  ) %>%
  pivot_wider(
    names_from = perlakuan,
    values_from = Mean
  )

View(Grafik_Panjang)
ggplot(
  Panjang_SPSS,
  aes(
    x=`Minggu ke`,
    y=Mean,
    color=perlakuan,
    group=perlakuan
  )
)+
  geom_line(
    linewidth=1
  )+
  geom_point(
    size=3
  )+
  labs(
    title="Panjang Daun",
    x="Minggu",
    y="Rata-rata Panjang Daun"
  )+
  theme_classic()