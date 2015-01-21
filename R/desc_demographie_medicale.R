
library("dplyr")
library("ggplot2")
library("ggthemes")

gdm <- read.csv("data/ecosante_depa.csv", stringsAsFactors = FALSE)

gdm %>% 
  filter(geo_level == "departement", year > 2008) %>% 
  View()

str(gdm)

gdm %>% 
  filter(code_geo == "000" & is.na(n) == FALSE) %>%
  ggplot() + 
    geom_line(aes(x = year, y = n, group = speciality))

