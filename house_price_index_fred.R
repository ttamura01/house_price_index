#all-transaction house index from FRED

library(tidyverse)
library(ggtext)
library(patchwork)
library(glue)
library(ggtext)
library(fredr) 

getwd()

#Set my FRED API key
fredr_set_key("0c5fd2514c7d98427fe3c931e2fcb244")
san_jose <- fredr(series_id = "ATNHPIUS41940Q") %>% 
  select(date, SanJose = value) 

seattle <- fredr(series_id = "ATNHPIUS42644Q") %>% 
  select(date, Seattle = value)

san_francisco <- fredr(series_id = "ATNHPIUS41884Q") %>% 
  select(date, SanFrancisco = value)

austin <- fredr(series_id = "ATNHPIUS12420Q") %>% 
  select(date, Austin = value)

boston <- fredr(series_id = "ATNHPIUS14454Q") %>% 
  select(date, Boston = value)

ny_nj <- fredr(series_id = "ATNHPIUS35614Q") %>% 
  select(date, NY.NJ = value)

dallas <- fredr(series_id = "ATNHPIUS19124Q") %>% 
  select(date, Dallas = value)

philadelphia <- fredr(series_id = "ATNHPIUS37964Q") %>% 
  select(date, Philadelphia = value)

kansas_city <- fredr(series_id = "ATNHPIUS28140Q") %>% 
  select(date, KansasCity = value)

columbus <- fredr(series_id = "ATNHPIUS18140Q") %>% 
  select(date, Columbus = value)

pittsburgh <- fredr(series_id = "ATNHPIUS38300Q") %>% 
  select(date, Pittsburgh = value)

house_index <- list(
  san_jose,
  seattle,
  san_francisco,
  austin,
  boston,
  ny_nj,
  dallas,
  philadelphia,
  kansas_city,
  columbus,
  pittsburgh
) %>% 
  reduce(full_join, by = "date") %>% 
  drop_na()

# Convert 'date' column to Date format if it's not already
house_index$date <- as.Date(house_index$date)

sapply(house_index, class)
head(house_index)
tail(house_index)

house_index <- house_index[-202,]

# house_index[ 198, ]

# Reshape the data frame from wide to long format
house_index_long <- pivot_longer(house_index, cols = -date, names_to = "city", values_to = "price") 

# Get the latest price for each city
latest_date <- max(house_index_long$date)

latest_prices <- aggregate(price ~ city, data = house_index_long[house_index_long$date == as.Date(latest_date), ], max)

# Reorder the levels of 'city' based on the latest price
house_index_long$city <- factor(house_index_long$city, levels = latest_prices[order(latest_prices$price, decreasing = TRUE), "city"])

# Plot the data with ggplot
# ggplot(data = house_index_long, aes(x = date, y = price, color = city)) +
#   geom_line() +
#   labs(title = "Historical House Prices Index in US major cities",
#        subtitle = "(house price of 1980-01-01 = 100)",
#        caption = "Source = FRED(Federal Reserve Bank of St.Louis)", 
#        x = NULL,
#        y = "House Price Index") +
#   theme(
#     legend.title = element_blank(),
#     plot.caption = element_markdown(color = "grey", size = 7)
#   )

p <- house_index_long %>% 
  mutate(is_pittsburgh = city == "Pittsburgh") %>% 
  ggplot(aes(x = date, y = price, color = city,
             size = is_pittsburgh)) +
  geom_line() +
  scale_size_manual(breaks = c(FALSE, TRUE),
                    values = c(0.3, 1),
                    guide = "none") +
  labs(title = "Historical House Prices Index in US Major Inovation-hub Cities",
       subtitle = "(house price of 1995:Q1 = 100)",
       caption = "Source = FRED(Federal Reserve Bank of St.Louis)", 
       x = NULL,
       y = "House Price Index") +
  theme_classic() +
  theme(
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.text = element_markdown(size = 20), 
    plot.caption = element_markdown(color = "grey", size = 7),
    plot.title.position = "plot",
    plot.title = element_textbox_simple(size = 24, face = "bold"),
    axis.text = element_text(size = 14, face = "bold"),
    panel.grid.major = element_line())

p

ggsave("/Users/takayukitamura/Documents/R_Computing/figures/histrical_house_prices_major_cities.png", width = 6, height = 5)

# plot just US average house price index
us_house_price <- read_csv("https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1320&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=USSTHPI&scale=left&cosd=1975-01-01&coed=2024-04-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Quarterly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2024-09-04&revision_date=2024-09-04&nd=1975-01-01") %>% 
  rename(date = DATE, US = USSTHPI)

us_house_price %>% 
  ggplot(aes(x = date, y = US)) +
  geom_line() +
  labs(title = "Historical US Average House Prices Index",
       subtitle = "(house price of 1980-01-01 = 100)",
       caption = "Source = FRED(Federal Reserve Bank of St.Louis)", 
       x = NULL,
       y = "House Price Index") +
  theme(
    legend.title = element_blank(),
    plot.caption = element_markdown(color = "grey", size = 7)
  )

ggsave("/Users/takayukitamura/Documents/R_Computing/figures/histrical_house_prices.png", width = 6, height = 4)

