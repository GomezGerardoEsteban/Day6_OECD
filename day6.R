
# Limpiamos el ambiente

rm(list = ls())


# libraries ---------------------------------------------------------------

library(tidyverse)
library(ggtext)
library(ggrepel)
library(readxl)

# base --------------------------------------------------------------------

base <- read_excel(path = "../rmd/resultados/graficos/30DayChartChallenge/base_muertesPorMillon.xlsx")

base <- base %>% 
  gather(key = year, value = muertesPorMillon, 3:length(base))

base$year <- as.numeric(base$year)

vector_filtro <- c("Colombia",
                   "Costa Rica",
                   "Chile",
                   "Mexico",
                   "OECD - Europe",
                   # "OECD - Total",
                   # "OECD America",
                   "World",
                   "Brazil",
                   "Bolivia",
                   "Ecuador",
                   "Peru",
                   "Uruguay",
                   "Latin Amer. and Carib.",
                   "China",
                   "India")


base_filtrada <- base %>% 
  filter(year %in% c(1990, 2019) & Pais %in% vector_filtro) %>% 
  spread(key = year, value = muertesPorMillon) %>% 
  mutate(reduccion = `1990` - `2019`)

grafico <- base_filtrada %>% 
  ggplot() +
  geom_point(mapping = aes(x = `1990`, y = reorder(Pais, `1990`)), color = "#AF601A", shape = 16, size = 2) +
  geom_point(mapping = aes(x = `2019`, y = reorder(Pais, `1990`)), color = "#228B08", shape = 15, size = 2) +
  geom_segment(mapping = aes(x = `1990`, xend = `2019`, y = Pais, yend = Pais)) +
  geom_rect(mapping = aes(xmin = 1350, xmax = 1450,
                          ymin = -Inf, ymax = Inf), fill = "lightgrey") +
  geom_text(mapping = aes(x = 1400, y = Pais, label = str_c("- ", round(reduccion,0))), color = "#04306C") +
  scale_x_continuous(limits = c(0,1450),
                     breaks = c(seq(0,1200,100),1400),
                     labels = c(seq(0,1200,100), "Reducción en\nmuertes por millon\n1990 - 2019")) +
  labs(title = "**Contaminación del aire del hogar por combustibles solidos**",
       y = NULL,
       x = "Muertes por millón de habitantes",
       subtitle = "<span style='color:#4B5253;'>Cocinar o calefaccionarse con combustibles como **madera, carbón o residuos agricolas**, tiene efectos adversos<br>en la calidad del aire del hogar, reduciendo la expectativa de vida y generando **muertes prematuras**. Por ello la<br>importancia del *Objetivo 7* de los ODS 2030, para mantener esta tendencia de</span><span style='color:#AF601A;'>1990</span><span style='color:#4B5253;'> y </span><span style='color:#228B08;'>2019</span>",
       caption = "Fuente: OECD.stat<br>**#30DayChartChallenge #Day6** @GEstebanGomez")+
  theme_bw() +
  theme(plot.title = element_markdown(hjust = 0.5, size = 14),
        plot.subtitle = element_markdown(hjust = 0.0, size = 10),
        plot.caption = element_markdown(size=8, hjust=0.0, face="italic", color="black"),
        axis.text.x = element_text(size = 9, angle = 0),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        axis.title.x = element_text(size = 9, vjust = 3),
        legend.title = element_text(size=7, hjust = 0.5), 
        legend.text = element_text(size=6),
        legend.position = "none")
  

ggsave(plot = grafico, 
       filename = "../rmd/resultados/graficos/30DayChartChallenge/5Day_OECD.png",
       dpi = 300,
       width = 8.36,
       height = 5.33
       )

