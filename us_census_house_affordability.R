library(tidyverse)
library(glue)
library(lubridate)
library(ggtext)
library(scales)
library(patchwork)

census <- read_csv("/Users/takayukitamura/Documents/R_Computing/house_price_index/census_2023.csv") %>% 
  rename(county = "Fact", population_2023 = "Population estimates, July 1, 2023, (V2023)", population_2010 = "Population, Census, April 1, 2010",
         median_house_value =  "Median value of owner-occupied housing units, 2019-2023", median_income = "Median households income (in 2023 dollars), 2019-2023",
         per_capita_income_2023 = "Per capita income in past 12 months (in 2023 dollars), 2019-2023") %>% 
  select(county, median_income, median_house_value)

read_csv("/Users/takayukitamura/Documents/R_Computing/house_price_index/census_2023.csv")

# Update the dataset with 'is_pittsburgh' flag
census <- census %>%
  mutate(is_allegheny = county == "Allegheny County, Pennsylvania")

# Define custom colors
custom_colors <- c("FALSE" = "gray", "TRUE" = "blue")

# Plot a: Median Household Income
a <- census %>%
  ggplot(aes(x = reorder(county, median_income), y = median_income, fill = is_allegheny)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_fill_manual(values = custom_colors) +
  scale_y_continuous(limits = c(0, 160000),
                     breaks = seq(0, 160000, 20000),
                     label = scales::label_currency()) +
  labs(x = NULL, y = "Median Household Income (2023E)",
       caption = "US Census") +
  theme_classic() +
  theme(
    axis.text.y = element_text(size = 7),
    panel.grid.major.x = element_line()
  )

# Plot b: Median House Value
b <- census %>%
  ggplot(aes(x = reorder(county, median_house_value), y = median_house_value, fill = is_allegheny)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_fill_manual(values = custom_colors) +
  scale_y_continuous(limits = c(0, 1600000),
                     breaks = seq(0, 1600000, 250000),
                     label = scales::label_currency()) +
  labs(x = NULL, y = "Median House Value",
       caption = "US Census") +
  theme_classic() +
  theme(
    axis.text.y = element_text(size = 7),
    panel.grid.major.x = element_line()
  )

# Plot c: Affordability Ratio
c <- census %>%
  mutate(affordability_ratio = median_house_value / median_income) %>%
  ggplot(aes(x = reorder(county, affordability_ratio), y = affordability_ratio, fill = is_allegheny)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_fill_manual(values = custom_colors) +
  labs(x = NULL, y = "Affordability Ratio (House Value / Household Income)",
       caption = "US Census") +
  theme_classic() +
  theme(
    axis.text.y = element_text(size = 7),
    panel.grid.major.x = element_line()
  )

d <- census %>%
  ggplot(aes(x = median_income, y = median_house_value)) +
  geom_point(aes(color = is_allegheny), size = 3) +  # Color Pittsburgh red, others gray
  geom_smooth(method = "lm", color = "black", se = FALSE) +  # Add regression line in blue
  scale_color_manual(
    values = c("FALSE" = "gray", "TRUE" = "blue"),  # Map colors for Pittsburgh and other cities
    guide = "none"  # Remove legend
  ) +
  scale_x_continuous(limits = c(40000, 160000),
                     breaks = seq(40000, 160000, 20000),
                     label = scales::label_currency()) +
  scale_y_continuous(limits = c(150000, 1400000),
                     breaks = seq(150000, 1400000, 250000),
                     label = scales::label_currency()) +
  labs(
    title = "House Value vs. Household Income by City",
    x = "Household Income (USD)",
    y = "House Value (USD)",
    caption = "US Census"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  )

cor.test(census$median_income, census$median_house_value)
cor.test(census$median_income, census$median_house_value, method = "spearman", exact = FALSE)

model <- lm(median_house_value ~ median_income, census)
summary(model)  
coef(model)


# Combine the plots using patchwork

(a + b)/(c + d)

