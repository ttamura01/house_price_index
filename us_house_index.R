library(tidyverse)
library(ggtext)
library(patchwork)
library(glue)
library(ggtext)
library(plotly)
setwd("/Users/takayukitamura/Documents/R_Computing/all_house_price_index")

# upload house_px file
house_index <- read.csv("/Users/takayukitamura/Documents/R_Computing/all_house_price_index/all_house_price_index .csv", sep = ",", 
                     header = TRUE, stringsAsFactors = FALSE ) %>% 
  select(-X)

# georgia <- read_csv("/Users/takayukitamura/Desktop/GASTHPI.csv") %>% 
#   select(date = observation_date, Gorgia = GASTHPI)

# house_index <- house_index %>% 
#   left_join(georgia, by = "date")

tail(house_index)

sapply(house_index, class)

updates <- tribble(~date, ~KansasCity, ~Seattle, ~SanJose, ~SanFrancisco, ~Austin, ~Dallas, ~Pittsburgh, ~Boston, ~NY.NJ, ~Philadelphia, ~Columbus,
                   "2025-04-01", 366.23, 556.82, 568.42, 509.54, 510.45, 426.94, 328.12, 489.94, 432.63, 405.67, 341.62)
# 
house_index <- rbind(house_index, updates)

tail(house_index)

write.csv(house_index, "/Users/takayukitamura/Documents/R_Computing/all_house_price_index/all_house_price_index .csv")

# Convert 'date' column to Date format if it's not already
house_index$date <- as.Date(house_index$date)

sapply(house_index, class)
head(house_index)
tail(house_index)

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

ggplotly(p)

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

us_cpi_raw <- read.csv("/Users/takayukitamura/Documents/R_Computing/all_house_price_index/cpi_aucsl.csv") %>% 
  rename_all(tolower) %>% 
  mutate("cpi_index" = (cpiaucsl/78)*100) 
  
us_cpi_raw$date <- as.Date(us_cpi_raw$date, 
                           format = "%Y-%m-%d")  


us_cpi_raw %>% 
  ggplot(aes(x=date, y = cpi_index)) + 
  geom_line()

cpi_house_index <- merge(us_cpi_raw, us_house_price, by = "date") %>% 
  select(date, cpi_index, US) %>% 
  rename("home_price_average" = US) %>% 
  pivot_longer(cols = -date, names_to = "inflation", 
               values_to = "index")


cpi_house_index %>% 
  ggplot(aes(x = date, y = index, color = inflation)) +
  geom_line()+
  labs(title = "Historical Inflation and US Average House Price Index",
       subtitle = "(1980-01-01 = 100)",
       caption = "Source = FRED(Federal Reserve Bank of St.Louis)", 
       x = NULL,
       y = "inflation & average house price index") +
  theme(
    legend.title = element_blank(),
    plot.title.position = "plot",
    plot.caption = element_markdown(color = "grey", size = 7),
    legend.background = element_blank(),
    legend.key = element_blank()
  )

ggsave("/Users/takayukitamura/Documents/R_Computing/figures/inflation vs home price.png", width = 6, height = 4)

str(us_cpi_raw)
us_cpi_raw[397,]
cpi_house_index[1,]
cpi_house_index[2,]

cpi_house_index[391,]
cpi_house_index[392,]

(average_inflation <- 394/67.1)
(average_home_price_appreciation <-658/59.9 )
(10.98497-1)

67.1*(1.033)^50

str(cpi_house_index)

