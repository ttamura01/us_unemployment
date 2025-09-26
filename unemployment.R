library(tidyverse)
library(ggtext)
library(patchwork)
library(glue)
library(ggtext)
library(plotly)
library(fredr)
library(patchwork)
setwd("/Users/takayukitamura/Documents/R_Computing")

# unemploy_white <- read_csv("https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23ebf3fb&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1320&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=LNS14000003&scale=left&cosd=1954-01-01&coed=2025-07-01&line_color=%230073e6&link_values=false&line_style=solid&mark_type=none&mw=3&lw=3&ost=-99999&oet=99999&mma=0&fml=a&fq=Monthly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2025-09-01&revision_date=2025-09-01&nd=1954-01-01") %>% 
#   rename(date = observation_date, white = LNS14000003)
# unemploy_hispanic <- read_csv("https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23ebf3fb&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1320&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=LNS14000009&scale=left&cosd=1973-03-01&coed=2025-07-01&line_color=%230073e6&link_values=false&line_style=solid&mark_type=none&mw=3&lw=3&ost=-99999&oet=99999&mma=0&fml=a&fq=Monthly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2025-09-01&revision_date=2025-09-01&nd=1973-03-01") %>% 
#   rename(date = observation_date, hispanic = LNS14000009)
# unemploy_black <- read_csv("https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23ebf3fb&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1320&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=LNS14000006&scale=left&cosd=1972-01-01&coed=2025-07-01&line_color=%230073e6&link_values=false&line_style=solid&mark_type=none&mw=3&lw=3&ost=-99999&oet=99999&mma=0&fml=a&fq=Monthly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2025-09-01&revision_date=2025-09-01&nd=1972-01-01") %>% 
#   rename(date = observation_date, black = LNS14000006)
# unemploy_asian <- read_csv("https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23ebf3fb&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1320&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=LNS14032183&scale=left&cosd=2003-01-01&coed=2025-07-01&line_color=%230073e6&link_values=false&line_style=solid&mark_type=none&mw=3&lw=3&ost=-99999&oet=99999&mma=0&fml=a&fq=Monthly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2025-09-01&revision_date=2025-09-01&nd=2003-01-01") %>% 
#   rename(date = observation_date, asian = LNS14032183)

#Set my FRED API key
fredr_set_key("0c5fd2514c7d98427fe3c931e2fcb244")

# Dates
start_date <- as.Date("2021-01-01")
end_date <- Sys.Date()

# Overall unemployment
unrate <- fredr(series_id = "UNRATE", observation_start = start_date, observation_end = end_date)

# Black unemployment
black_unrate <- fredr(series_id = "LNS14000006", observation_start = start_date, observation_end = end_date)

black_unrate %>% 
  filter(date>= "2024-01-01")

# Labor force participation
civpart <- fredr(series_id = "CIVPART", observation_start = start_date, observation_end = end_date)

# Put into a single dataframe
df <- unrate %>%
  select(date, overall = value) %>%
  left_join(black_unrate %>% select(date, black = value), by = "date") %>%
  left_join(civpart %>% select(date, lfpr = value), by = "date")

p1 <- df %>%
  pivot_longer(cols = c(overall, black), names_to = "group", values_to = "rate") %>%
  ggplot(aes(x = date, y = rate, color = group)) +
  geom_line(size = 1) +
  annotate("label",
            x = as.Date("2025-03-01"),
            y = c(4.6, 7.75),
            label = c("Overall: 4.3 %", "Black Americans: 7.5 %"),
           color = c("steelblue", "firebrick"),
           hjust = c(0.5, 0.65)) + 
  scale_color_manual(values = c("overall" = "steelblue", "black" = "firebrick"),
                     labels = c("Overall", "Black Americans")) +
  labs(title = "Unemployment Rates: Black Americans vs Overall",
       caption = "FRED(Federal Reserve Economic Data)",
       y = "Unemployment Rate (%)",
       x = NULL,
       color = "") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple(size = 20, face = "bold"),
    legend.position = "none")

p2 <- ggplot(df, aes(x = date, y = lfpr)) +
  geom_line(color = "darkgreen", size = 1) +
  labs(title = "Labor Force Participation Rate",
       y = "LFPR (%)",
       x = NULL) +
  theme_minimal(base_size = 14)

p1/p2
#---Download Data---
# CPI
unemploy_white <- fredr(series_id = "LNS14000003") %>% 
  select(date, white = value)
unemploy_hispanic <- fredr(series_id = "LNS14000009") %>% 
  select(date, hispanic = value )
unemploy_black <- fredr(series_id = "LNS14000006") %>% 
  select(date, black = value )
unemploy_asian <- fredr(series_id = "LNS14032183") %>% 
  select(date, asian = value )

latest_date <-  max(unemployment_long$date)
latest_rate <- aggregate(unemployment_rate ~ race, data = unemployment_long[unemployment_long$date == as.Date(latest_date), ], max)
unemployment_long$race <- factor(unemployment_long$race, levels = latest_rate[order(latest_rate$unemployment_rate, decreasing = TRUE), "race"])

unemployment_long <- unemploy_white %>% 
  # filter(date >= as.Date("2020-01-01")) %>%
  left_join(unemploy_hispanic, by = "date") %>% 
  left_join(unemploy_black, by = "date") %>% 
  left_join(unemploy_asian, by = "date") %>% 
  pivot_longer(-date, names_to = "race", values_to = "unemployment_rate")

latest_date <-  max(unemployment_long$date)
latest_rate <- aggregate(unemployment_rate ~ race, data = unemployment_long[unemployment_long$date == as.Date(latest_date), ], max)
unemployment_long$race <- factor(unemployment_long$race, levels = latest_rate[order(latest_rate$unemployment_rate, decreasing = TRUE), "race"])

unemployment_long%>% 
  ggplot(aes(x = date, y = unemployment_rate, group = race, colour = race)) +
  geom_line() +
  scale_color_manual(breaks = c(FALSE, TRUE),
                     values = c("#0079ae","red")) +
  scale_size_manual(breaks = c(FALSE, TRUE),
                    values = c(0.3,0.8))
  theme(
    legend.text = element_markdown(size = 20)
  )

sapply(unemploy_white, class)

unemployment_long%>% 
  # mutate(is_black = race == "black") %>% 
  ggplot(aes(x = date, y = unemployment_rate, group = race, colour = race)) +
  geom_line() +
  # scale_color_manual(breaks = c(FALSE, TRUE),
  #                    values = c("#0079ae","red")) +
  scale_size_manual(breaks = c(FALSE, TRUE),
                    values = c(0.3,0.8))
theme(
  legend.text = element_markdown(size = 20)
)


