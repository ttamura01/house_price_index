library(tidyverse)
library(scales)
library(slider)

# Data preparation
data <- tibble( 
  date = c("9/1/23", "10/1/23", "11/1/23", "12/1/23", "1/1/24", "2/1/24", "3/1/24", "4/1/24", "5/1/24", "6/1/24",  
           "7/1/24", "8/1/24", "9/1/24", "10/1/24", "11/1/24", "12/1/24"),
  home_sales = c(3980000, 3850000, 3910000, 3880000, 4000000, 4380000, 4220000, 4140000, 4110000, 3900000,
                 3960000, 3880000, 3840000, 3960000, 4150000, 4240000),
  mortgage_rate = c(7.2, 7.8, 7.22, 6.61, 6.69, 6.94, 6.8, 7.2, 7.03, 6.86, 6.78, 6.35, 6.08, 6.72, 6.81, 6.69)
)

data$date <- as.Date(data$date, format = "%m/%d/%y")

# Convert home sales to millions
data <- data %>% mutate(home_sales = home_sales / 1000000)

data <- data %>% 
  mutate(home_sales_2 = home_sales) %>% 
  mutate(one_month_lag = lag(mortgage_rate),
         two_month_lag = lag(mortgage_rate, n = 2)) 

data %>% 
  select(date, home_sales, two_month_lag) %>% 
  pivot_longer(-date) %>% 
  ggplot(aes(x = date, y = value)) +
  geom_col() +
  facet_wrap(~name, ncol = 1, scales = "free_y")

data %>% 
  select(date, home_sales, two_month_lag) %>% 
  ggplot(aes(x = date)) +
  geom_col(aes(y = home_sales), color = "dodgerblue", fill = "dodgerblue", alpha = 0.3) +
  geom_line(aes(y = two_month_lag/2, color = "red")) +
  theme_classic()

data %>% 
  select(date, home_sales, two_month_lag) %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = home_sales), color = "dodgerblue", fill = "dodgerblue") +
  geom_line(aes(y = two_month_lag/2, color = "red")) +
  theme_classic()

data <- tibble( 
  date = c("9/1/23", "10/1/23", "11/1/23", "12/1/23", "1/1/24", "2/1/24", "3/1/24", "4/1/24", "5/1/24", "6/1/24",  
           "7/1/24", "8/1/24", "9/1/24", "10/1/24", "11/1/24"),
  home_sales = c(3980000, 3850000, 3910000, 3880000, 4000000, 4380000, 4220000, 4140000, 4110000, 3900000,
                 3960000, 3880000, 3840000, 3960000, 4150000,),
  mortgage_rate_2m_lag = c(6.8, 7.7, 7.2, 7.8, 7.22, 6.61, 6.69, 6.94, 6.8, 7.2, 7.03, 6.86, 6.78, 6.35, 6.08)
)

data$date <- as.Date(data$date, "%m/%d/%y")

data <- data %>% 
  mutate(home_sales = home_sales/1000000,
         mortgage_rate_2m_lag = mortgage_rate_2m_lag/1.65)

data %>% 
  pivot_longer(cols = -date) %>% 
  ggplot(aes(x = date, y = value, color = name)) +
  geom_line() +
  scale_y_continuous(limits = c(3.6, 4.8))

data %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = home_sales)) +
  geom_line(aes(y = mortgage_rate_2m_lag), color = "red") +
  scale_y_continuous(limits = c(3.6, 4.8),
                     name = "Existing Home Sales(mil)", 
                     sec.axis = sec_axis(~.*1, 
                       breaks = c(3.6, 4.0, 4.5, 4.8),
                       name = "Mortgage Rate (Two Month Lag)"
                     )) +
  labs(title = "Existing Home Sales and 30-Year Mortgage Rate(Two Month Lag)",
       x = NULL) +
  theme_classic() +
  theme(
    axis.title.y.right = element_text(color = "red"),
    axis.text.y.right = element_text(color = "red"),
    plot.title.position = "plot",
    plot.title = element_textbox_simple()
  )
  
data %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = home_sales)) +
  geom_line(aes(y = mortgage_rate_2m_lag), color = "red") +
  scale_y_continuous(limits = c(3.6, 4.8),
                     name = "Existing Home Sales(mil)", 
                     sec.axis = sec_axis(~.*1, 
                                         breaks = c(3.6, 4.0, 4.5, 4.8),
                                         name = "Mortgage Rate (Two Month Lag)")) +
  labs(title = "Existing Home Sales and 30-Year Mortgage Rate(Two Month Lag)",
       x = NULL) +
  theme_classic() +
  theme(
    axis.title.y.right = element_text(color = "red"),
    axis.text.y.right = element_text(color = "red"),
    plot.title.position = "plot",
    plot.title = element_textbox_simple()
  )

