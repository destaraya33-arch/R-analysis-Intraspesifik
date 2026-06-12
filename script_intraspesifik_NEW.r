############################################################
# ANALISIS PERSAINGAN INTRASPESIFIK TANAMAN
# R STUDIO VERSION
############################################################


############################################################
# 1. INSTALL PACKAGE (jalankan sekali saja)
############################################################

# install.packages("readxl")
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("ggplot2")
# install.packages("car")
# install.packages("agricolae")


############################################################
# 2. LOAD PACKAGE
############################################################

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(car)
library(agricolae)



############################################################
# 3. IMPORT DATA EXCEL
############################################################

data <- read_excel(
  file.choose()
)

View(data)

names(data)



############################################################
# 4. FORMAT DATA
############################################################

data$Perlakuan <- factor(data$Perlakuan)

data$Minggu_ke <- as.numeric(data$Minggu_ke)

data$Tanaman_ke <- factor(data$Tanaman_ke)


# Rename supaya aman untuk R
data <- data %>%
  rename(
    Massa = `Massa_(gr)`
  )

data <- data %>%
  mutate(
    Tinggi_tanaman = as.numeric(Tinggi_tanaman),
    Jumlah_daun = as.numeric(Jumlah_daun),
    Panjang_daun = as.numeric(Panjang_daun),
    Lebar_daun = as.numeric(Lebar_daun),
    Massa = as.numeric(Massa)
  )

str(data)



############################################################
# 5. FUNGSI ANALISIS OTOMATIS
############################################################


analisis <- function(parameter, judul){
  
  
  ##########################################################
  # DESCRIPTIVE STATISTICS (SPSS STYLE)
  ##########################################################
  
  
  hasil <- data %>%
    group_by(
      Perlakuan,
      Minggu_ke
    ) %>%
    summarise(
      
      Mean = mean(
        .data[[parameter]],
        na.rm = TRUE
      ),
      
      Std_Deviation = sd(
        .data[[parameter]],
        na.rm = TRUE
      ),
      
      N = n(),
      
      .groups = "drop"
    )
  
  
  View(hasil)
  
  
  
  ##########################################################
  # TABEL UNTUK GRAFIK
  ##########################################################
  
  
  tabel_grafik <- hasil %>%
    select(
      Minggu_ke,
      Perlakuan,
      Mean
    ) %>%
    
    pivot_wider(
      names_from = Perlakuan,
      values_from = Mean
    )
  
  
  View(tabel_grafik)
  
  
  
  ##########################################################
  # GRAFIK
  ##########################################################
  
  
  grafik <- ggplot(
    hasil,
    aes(
      x=Minggu_ke,
      y=Mean,
      color=Perlakuan,
      group=Perlakuan
    )
  )+
    
    geom_line(
      linewidth=1
    )+
    
    geom_point(
      size=3
    )+
    
    labs(
      title=judul,
      x="Minggu",
      y=paste(
        "Rata-rata",
        judul
      )
    )+
    
    theme_classic()
  
  
  print(grafik)
  
  
  
  ##########################################################
  # ANOVA
  ##########################################################
  
  
  model <- aov(
    
    as.formula(
      paste(
        parameter,
        "~ Perlakuan"
      )
    ),
    
    data=data
  )
  
  
  print(
    summary(model)
  )
  
  
  
  ##########################################################
  # NORMALITAS
  ##########################################################
  
  
  print(
    shapiro.test(
      residuals(model)
    )
  )
  
  
  
  ##########################################################
  # HOMOGENITAS
  ##########################################################
  
  
  print(
    
    leveneTest(
      
      as.formula(
        paste(
          parameter,
          "~ Perlakuan"
        )
      ),
      
      data=data
    )
  )
  
  
  
  
  ##########################################################
  # DUNCAN TEST
  ##########################################################
  
  
  print(
    
    duncan.test(
      model,
      "Perlakuan"
    )
  )
  
  
}



############################################################
# 6. ANALISIS SEMUA PARAMETER
############################################################


# Tinggi Tanaman

analisis(
  "Tinggi_tanaman",
  "Tinggi Tanaman"
)



# Jumlah Daun

analisis(
  "Jumlah_daun",
  "Jumlah Daun"
)



# Panjang Daun

analisis(
  "Panjang_daun",
  "Panjang Daun"
)



# Lebar Daun

analisis(
  "Lebar_daun",
  "Lebar Daun"
)



# Massa Tanaman

analisis(
  "Massa",
  "Massa Tanaman"
)





############################################################
# 7. REGRESI BERGANDA
############################################################


regresi <- lm(
  
  Tinggi_tanaman ~
    Jumlah_daun +
    Panjang_daun +
    Lebar_daun +
    Massa,
  
  data=data
  
)


summary(regresi)



############################################################
# 8. NORMALITAS REGRESI
############################################################


shapiro.test(
  residuals(regresi)
)



############################################################
# 9. MULTIKOLINEARITAS
############################################################


vif(
  regresi
)



############################################################
# 10. GRAFIK REGRESI
############################################################


ggplot(
  
  data,
  
  aes(
    x=Jumlah_daun,
    y=Tinggi_tanaman
  )
  
)+
  
  geom_point(
    size=3
  )+
  
  geom_smooth(
    method="lm"
  )+
  
  labs(
    title="Regresi Tinggi Tanaman",
    x="Jumlah Daun",
    y="Tinggi Tanaman"
  )+
  
  theme_classic()


############################################################
# ANALISIS SELESAI
############################################################
