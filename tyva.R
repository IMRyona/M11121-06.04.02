setwd("C:/RS")
getwd()

library(tidyverse)
library(lubridate)
library(rnoaa)

# скачиваем список всех метеостанций мира, процесс может занять до нескольких минут
#station_data = ghcnd_stations()

# сохраняем скачанные список метеостанций в отдельный файл, чтобы больше не обращаться к нему через интернет
#write.csv(station_data,"station_data.csv")
station_data = read.csv("station_data.csv")

# для региона 17 создаем таблицу с коррдинатами столицы Кызыл
Kyzyl = data.frame(id = "Kyzyl",
                         latitude = 51.4252,
                         longitude = 94.2712)

# обращаемся к ближайшим метеостанциям в локации Кызыла,
# лимит подбираем, чтобы года были предшествующими 2020 на 9 [2010-2019 гг]
Kyzyl_around = meteo_nearby_stations(lat_lon_df = Kyzyl,
                                           station_data = station_data,
                                           var = c("PRCP", "TAVG"),
                                           year_min = 2010, year_max = 2019)

# проверяем тип данных (получили list из одной строки с вложеной таблийцей)
class(Kyzyl_around)

# чтобы далее работать с данными надо перевести строку в формат таблицы, для этого просто обращаемся к этой строке с таблицей в текстовом формате
Kyzyl_around = Kyzyl_around[[1]]

# убираем из таблицы данные с метеостанций от 70 до 210 км от Кызыла
Kyzyl_around = Kyzyl_around |> filter(distance >=70 & distance <=210)

# создаем вектор с id номерами выбранных метеостанций 
meteo_id_4 = Kyzyl_around[,1]

# получаем все данные со всех выбранных метеостанций
all_meteo_data = meteo_tidy_ghcnd(stationid = meteo_id_4)

all_meteo_data$prcp # Precipitation - осадки
all_meteo_data$snwd # Snow - высота зимних осадков
all_meteo_data$tavg # Temoerature air averaged - средняя темп воздуха за день
all_meteo_data$tmax # Temoerature air max - максимальная темп воздуха за день
all_meteo_data$tmin # Temoerature air min - минимальная темп воздуха за день

# добавляем в таблицу колонки с разделенной датой (год, месяц, день)
all_meteo_data = all_meteo_data |> mutate(
  year = year(date), month = month(date), doy =yday(date))

# фильтруем по годам так, чтобы выбрать период 9 лет, предшествующий 2020 году
all_meteo_data = all_meteo_data |> filter(year > 2010 & year < 2019)

# переводим размерности значений в привычную для пользователей форму (меторологи не любят точки)
all_meteo_data = all_meteo_data |> mutate(
  prcp = prcp /10, snwd = snwd /10, tavg = tavg /10, tmax =tmax/10, tmin = tmin/10)

# в векторе можно подменить все значения меньше 5 на 0 (делаем это через substitute)
all_meteo_data$tavg[all_meteo_data$tavg < 5] = 0

# группируем по месяцам, годам и id 
# и сводим в таблицу с помесячными суммами активных температур на все станции и годы
sum_monht_tavg = all_meteo_data |> group_by(month, id, year) |> 
  summarise(sum_tavg = sum(tavg, na.rm = TRUE)) |> print(n=500) 

# сбрасываем группировку
sum_monht_tavg = ungroup(sum_monht_tavg)

# группируем по месяцам и сводим в таблицу со средними помесячными активными температурами 
mean_month_tavg = sum_monht_tavg |> group_by(month) |>
  summarise(mean_tavg = mean(sum_tavg, na.rm=T))

# считаем среднюю сумму активных температур (>5) за год
sum_year_tavg = sum(mean_month_tavg$mean_tavg) #получили 1466.344 градуса 

# добавляем в сводную таблицу переменные из табл 1 методички https://ecologymodeling.github.io/Tests.html
i={1;12} 
mean_month_tavg = mean_month_tavg |> mutate(
  afi = c(0,0,0,32.11,26.31,25.64,23.2,18.73,16.3,13.83,0,0),
  bfi = c(0,0,0,11.3,9.26,9.03,8.16,6.59,5.73,4.87,0,0),
  di = c(0,0,0,0.33,1,1,1,0.32,0,0,0,0))

# добавляем в сводную таблицу переменную Fi, расчитанную по формуле из методички
mean_month_tavg = mean_month_tavg |> mutate(Fi = afi+bfi*mean_tavg)

# рассчитываем урожайность пшеницы по формуле из методички с учетом заданных констант
Yj = 10^6 * sum(mean_month_tavg$Fi * mean_month_tavg$di * 300 / (
  1600 * 2.2 * (100 - 25)))
Yj = Yj / 1000 / 100

# Итого, урожайность пшеницы в Кызыле (17) в 2020 году 
# составила 98.10637 ц/га 
# По данным сайта https://www.tuvaonline.ru/2020/09/15/v-tuve-startuet-uborochnaya-kampaniya.html
#в 2020 г урожайность в Тыве составила 13,8 центнеров с гектара