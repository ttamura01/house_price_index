library(tidyverse)
library(glue)
library(lubridate)
library(ggtext)
library(scales)
library(patchwork)

income_house <- tibble(city = c("Austin", "Boston", "Columbus", "Dallas", "Kansas", 
                                "NewYork", "Philadelphia", "Pittsburgh", "SanFrancisco", "SanJose", 
                                "Seattle"), 
                       household_income =c(97169, 84548, 72584, 74584, 67178, 
                                 104553, 60689, 76393, 141446, 159674, 
                                 122148),
                       house_value = c(487600, 680700, 303400, 277900, 277900, 
                                 1108900, 232400, 216700, 1380500, 1382800, 
                                 811200))

# Update the dataset with 'is_pittsburgh' flag
income_house <- income_house %>%
  mutate(is_pittsburgh = city == "Pittsburgh")

# Define custom colors
custom_colors <- c("FALSE" = "gray", "TRUE" = "blue")

# Plot a: Median Household Income
a <- income_house %>%
  ggplot(aes(x = reorder(city, household_income), y = household_income, fill = is_pittsburgh)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_fill_manual(values = custom_colors) +
  scale_y_continuous(limits = c(0, 160000),
                     breaks = seq(0, 160000, 50000),
                     label = scales::label_currency()) +
  labs(x = NULL, y = "Median Household Income",
       caption = "souce: US Census") +
  theme_classic() +
  theme(
    panel.grid.major.x = element_line(),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12, face = "bold")
  )

# Plot b: Median House Value
b <- income_house %>%
  ggplot(aes(x = reorder(city, house_value), y = house_value, fill = is_pittsburgh)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_fill_manual(values = custom_colors) +
  scale_y_continuous(limits = c(0, 1500000),
                     breaks = seq(0, 1500000, 500000),
                     label = scales::label_currency()) +
  labs(x = NULL, y = "Median House Value",
       caption = "souce: US Census") +
  theme_classic() +
  theme(
    panel.grid.major.x = element_line(),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12, face = "bold")
  )

# Plot c: Affordability Ratio
c <- income_house %>%
  mutate(affordability_ratio = house_value / household_income) %>%
  ggplot(aes(x = reorder(city, affordability_ratio), y = affordability_ratio, fill = is_pittsburgh)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_fill_manual(values = custom_colors) +
  labs(x = NULL, y = "Affordability Index (House Value / Household Income)",
       caption = "souce: US Census") +
  theme_classic() +
  theme(
    panel.grid.major.x = element_line(),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12, face = "bold")
  )


d <- income_house %>%
  ggplot(aes(x = household_income, y = house_value)) +
  geom_point(aes(color = is_pittsburgh), size = 3) +  # Color Pittsburgh red, others gray
  geom_smooth(method = "lm", color = "black", se = FALSE) +  # Add regression line in blue
  annotate("text", x = 80000, y = 200000,
           hjust = 0.1, vjust = 0,
           label = "Pittsburgh",
           size = 7, fontface = "bold") +
  annotate("text", x = 70000, y = 1180000,
           hjust = 0.2, vjust = 0,
           label = "y = 13.12582 * X - 615,095.75,
           R2 = 0.85, pv = 3.696e-05",
           size = 3, fontface = "italic") +
  scale_color_manual(
    values = c("FALSE" = "gray", "TRUE" = "blue"),  # Map colors for Pittsburgh and other cities
    guide = "none"  # Remove legend
  ) +
  scale_x_continuous(limits = c(50000, 160000),
                     breaks = seq(50000, 160000, 50000),
                     label = scales::label_currency()) +
  scale_y_continuous(limits = c(100000, 1600000),
                     breaks = seq(100000, 1600000, 500000),
                     label = scales::label_currency()) +
  labs(
    title = "House Value vs. Household Income by City",
    x = "Household Income (USD)",
    y = "House Value (USD)",
       caption = "souce: US Census"
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12, face = "bold")
  )


d
(a+b)/(c+d)

ggsave("census_major_cities.png", width = 10, height = 8)

cor.test(income_house$household_income, income_house$house_value)
cor.test(income_house$household_income, income_house$house_value, method = "spearman", exact = FALSE)

model <- lm(house_value ~ household_income, income_house)
summary(model)  
coef(model)


