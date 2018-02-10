intensity <- read.csv("data/light_intensity.csv", stringsAsFactors = FALSE)
intensity$time <- as.POSIXct(intensity$time)
sunset <- intensity$time[intensity$comment != ""]
intensity$comment <- NULL
r <- 1/7
intensity$ref_outside <- r * intensity$outside
intensity$trn_room <- (1-r) * intensity$room
intensity$trn_room_lamp <- (1-r) * intensity$room_lamp
colnames(intensity) <- c("Czas", "Pokoj z lampa", "Pokoj", "Dwor", "Odb. dworu", "Tr. z pokoju", "Tr. z pokoju z lampa")
df_plot <- tidyr::gather(intensity,
                         key = "Zrodlo Swiatla",
                         value = "Intensywnosc [lx]",
                         -Czas)

library(ggplot2)
library(viridis)
library(dplyr)
g <- ggplot(data = filter(df_plot, `Zrodlo Swiatla` %in% c('Dwor', 'Pokoj', 'Pokoj z lampa')),
            aes(x = Czas, y = `Intensywnosc [lx]`, color = `Zrodlo Swiatla`)) + 
  # geom_smooth(size = 1.1, se = FALSE) + 
  geom_line(size = 1.1) +
  geom_point(size = 2.3) + 
  scale_color_viridis(discrete = TRUE)
print(g)

g2 <- ggplot(data = filter(df_plot, `Zrodlo Swiatla` %in% c('Odb. dworu', "Tr. z pokoju", "Tr. z pokoju z lampa")),
            aes(x = Czas, y = `Intensywnosc [lx]`, color = `Zrodlo Swiatla`)) + 
  # geom_smooth(size = 1.1, se = FALSE) + 
  geom_line(size = 1.1) +
  geom_point(size = 2.3) + 
  scale_color_viridis(discrete = TRUE)
print(g2)