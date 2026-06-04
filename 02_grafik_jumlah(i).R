Jumlah_SPSS <- data %>%
  group_by(
    perlakuan,
    `Minggu ke`
  ) %>%
  summarise(
    Mean = mean(
      `Jumlah daun`,
      na.rm = TRUE
    ),
    
    Std_Deviation = sd(
      `Jumlah daun`,
      na.rm = TRUE
    ),
    
    N = n(),
    
    .groups="drop"
  )

View(Jumlah_SPSS)
Grafik_Jumlah <- Jumlah_SPSS %>%
  select(
    `Minggu ke`,
    perlakuan,
    Mean
  ) %>%
  pivot_wider(
    names_from=perlakuan,
    values_from=Mean
  )

View(Grafik_Jumlah)
ggplot(
  Jumlah_SPSS,
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
    title="Jumlah Daun",
    x="Minggu",
    y="Rata-rata Jumlah Daun"
  )+
  theme_classic()