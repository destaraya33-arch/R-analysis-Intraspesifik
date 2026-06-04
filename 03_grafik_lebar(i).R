Lebar_SPSS <- data %>%
  group_by(
    perlakuan,
    `Minggu ke`
  ) %>%
  summarise(
    Mean = mean(
      `Lebar daun`,
      na.rm = TRUE
    ),
    
    Std_Deviation = sd(
      `Lebar daun`,
      na.rm = TRUE
    ),
    
    N = n(),
    
    .groups="drop"
  )

View(Lebar_SPSS)
Grafik_Lebar <- Lebar_SPSS %>%
  select(
    `Minggu ke`,
    perlakuan,
    Mean
  ) %>%
  pivot_wider(
    names_from = perlakuan,
    values_from = Mean
  )

View(Grafik_Lebar)
ggplot(
  Lebar_SPSS,
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
    title="Lebar Daun",
    x="Minggu",
    y="Rata-rata Lebar Daun"
  )+
  theme_classic()