setwd("/Users/takayukitamura/Documents/R_Computing/us_unemployment")

library(tidyverse)
library(ggtext)
library(patchwork)
library(glue)
library(ggtext)
library(plotly)
library(fredr)
library(patchwork)

#Set my FRED API key
fredr_set_key("0c5fd2514c7d98427fe3c931e2fcb244")
overall_undata <- fredr(series_id = "UNRATE") %>% 
  select(date, "overall_undata" = value)

yourth_undata <- fredr(series_id = "LNS14024887") %>% 
  select(date, "yourth_undata" = value) 

# Black unemployment
black_unrate <- fredr(series_id = "LNS14000006") %>% 
  select(date, "black_undata" = value) 

df <- overall_undata %>% 
  left_join(yourth_undata, by = "date") %>% 
  left_join(black_unrate, by = "date")
  
df_longer <- df %>% 
  filter(date >= "2021-01-01") %>% 
  pivot_longer(-date, names_to = "category", values_to = "unemployment") 
 
df_longer %>%  
  ggplot(aes(x = date, y = unemployment, colour = category)) +
  geom_line(linewidth = 2) +
  annotate("label",
           x = as.Date("2025-02-01"),
           y = c(4.8, 8.0, 11.0),
           label = c("Overall: 4.4 %", "Black American: 7.5%", "16-24 Yrs: 10.4 %"),
           color = c("steelblue", "black", "firebrick"),
           hjust = c(0.35, 0.55, 0.45)) + 
  scale_color_manual(values = c("overall_undata" = "steelblue","black_undate", "yourth_undata" = "firebrick")
                     ) +
  labs(title = "Unemployment Rates: 16-24 years old & Black American vs Overall",
       caption = "FRED(Federal Reserve Economic Data• by Takayuki Tamura)",
       y = "Unemployment Rate (%)",
       x = NULL,
       color = "none") +
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(face = "bold"),
    plot.title.position = "plot",
    plot.title = element_textbox_simple(size = 20),
    plot.caption = element_text(face = "italic", size = 8),
    legend.position = "none")
 
ggsave("unenployment_young_black.png", width = 6.5, height = 5.6) 
