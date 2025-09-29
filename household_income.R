#Median Household Income in US major cities
setwd("/Users/takayukitamura/Documents/R_Computing/all_house_price_index")

library(tidyverse)
library(ggtext)
library(patchwork)
library(glue)
library(ggtext)


# upload house_px file

income <- read_csv("/Users/takayukitamura/Documents/R_Computing/all_house_price_index/average_income.csv")


tail(income)

sapply(income, class)

income <- income %>% 
  select(-US)


# Reshape the data frame from wide to long format
income_long <- pivot_longer(income, cols = -year, names_to = "city", values_to = "household_income") 

# Get the latest price for each city
latest_date <- max(income_long$year)

latest_income <- aggregate(household_income ~ city, data = income_long[income_long$year == as.Date(latest_date), ], max)

# Reorder the levels of 'city' based on the latest price
income_long$city <- factor(income_long$city, levels = latest_income[order(latest_income$household_income, decreasing = TRUE), "city"])


income_long %>% 
  mutate(is_pittsburgh = city == "Pittsburgh") %>% 
  ggplot(aes(x = year, y = household_income, colour = city,
             size = is_pittsburgh)) +
  geom_line() +
  # scale_color_manual(breaks = c("Austin", "SanFrancisco", "Pittsburgh", "Dallas", "NewYork", "Seattle", "Boston", "Columbus", "KansasCity", "SanJose", "Philadelphia"),
  #                    values = c("#738626", "#FFBA1C", "#F5270A","#30664A", "#31BCCF","#FF8D82","#7C5989","#BB3630","#374D77","#70DBFF","#929580")) +
  scale_size_manual(breaks = c(FALSE, TRUE),
                    values = c(0.3, 2),
                    guide = "none") +
  labs(title = "Median Household Income in US major cities",
       caption = "Source:U.S.Census Bureau, FRED(Federal Reserve Bank of St.Louis)", 
       x = NULL,
       y = "Median Household Income ($)") +
  scale_y_continuous(limits = c(25000, 160000),
                     breaks = seq(25000, 160000, 20000),
                     label = scales::label_currency()) +
  theme_classic() +
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple(size = 24, face = "bold"),
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(size = 14),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 14, face = "bold"),
    plot.caption = element_markdown(color = "grey", size = 7),
    panel.grid.major = element_line())
    
ggsave("/Users/takayukitamura/Documents/R_Computing/figures/histrical_household_income_major_cities.png", width = 6, height = 5)
