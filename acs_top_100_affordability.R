library(tidyverse)
library(patchwork)

census <- read_csv("/Users/takayukitamura/Documents/R_Computing/house_price_index/acs_top_100.csv") %>% 
  filter(population >= 1000000) %>% 
  mutate(income = income/1000, home_value = home_value/1000)


# Update the dataset with 'is_pittsburgh' flag
census <- census %>%
  mutate(is_allegheny = county == "Allegheny County, Pennsylvania")

# Define custom colors
custom_colors <- c("FALSE" = "gray70", "TRUE" = "blue")

# Plot a: Median Household Income
a <- census %>%
  ggplot(aes(x = reorder(county, income), y = income, fill = is_allegheny)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_fill_manual(values = custom_colors) +
  scale_y_continuous(limits = c(0, 160),
                     breaks = seq(0, 160, 20),
                     label = scales::label_currency()) +
  labs(x = NULL, y = "Median Household Income (x1,000)",
       caption = "US Census 2023, by Takayuki Tamura") +
  theme_classic() +
  theme(
    axis.text.y = element_text(size = 7),
    axis.text.x = element_text(size = 6),
    panel.grid.major.x = element_line()
  )

# Plot b: Median House Value
b <- census %>%
  ggplot(aes(x = reorder(county, home_value), y = home_value, fill = is_allegheny)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_fill_manual(values = custom_colors) +
  scale_y_continuous(limits = c(0, 1600),
                     breaks = seq(0, 1600, 250),
                     label = scales::label_currency()) +
  labs(x = NULL, y = "Median House Value (x1,000)",
       caption = "US Census 2023, by Takayuki Tamura") +
  theme_classic() +
  theme(
    axis.text.y = element_text(size = 7),
    panel.grid.major.x = element_line()
  )

# Plot c: Affordability Ratio
c <- census %>%
  mutate(affordability_ratio = home_value / income) %>%
  ggplot(aes(x = reorder(county, affordability_ratio), y = affordability_ratio, fill = is_allegheny)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_fill_manual(values = custom_colors) +
  labs(x = NULL, y = "Affordability Ratio (House Value / Household Income)",
       caption = "US Census 2023, by Takayuki Tamura") +
  theme_classic() +
  theme(
    axis.text.y = element_text(size = 7),
    axis.text.x = element_text(size = 6),
    panel.grid.major.x = element_line()
  )

d <- census %>%
  ggplot(aes(x = income, y = home_value)) +
  geom_smooth(method = "lm", color = "black", se = FALSE) +  # Add regression line in blue
  geom_point(aes(color = is_allegheny), size = 3) +  # Color Pittsburgh red, others gray
  scale_color_manual(
    values = c("FALSE" = "gray70", "TRUE" = "blue"),  # Map colors for Pittsburgh and other cities
    guide = "none"  # Remove legend
  ) +
  scale_x_continuous(limits = c(40, 160),
                     breaks = seq(40, 160, 20),
                     label = scales::label_currency()) +
  scale_y_continuous(limits = c(120, 1500),
                     breaks = seq(120, 1500, 250),
                     label = scales::label_currency()) +
  labs(
    title = "House Value vs. Household Income by City",
    x = "Household Income (USD)",
    y = "House Value (x1,000)",
    caption = "US Census 2023, by Takayuki Tamura"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  )

cor.test(census$income, census$home_value)
cor.test(census$income, census$home_value, method = "spearman", exact = FALSE)

model <- lm(home_value ~ income, census)
summary(model)  
coef(model)


# Combine the plots using patchwork

(a + b)/(c + d)

