

### libraries
library(readxl)
library(dplyr)
library(ggplot2)

### data
income_quintiles <- read_excel("/Users/jackconnors/Downloads/income_quintiles_5.xlsx",
                               sheet="Sheet1")

### plot 2021 quintiles
library(tidyverse)

### plot quintiles
earned_income  <- income_quintiles %>%
  filter(Year=="2019") %>%
  pivot_longer(cols = -Year, names_to = "Quintile", values_to = "Income") %>%
  slice(1:5) %>%
  ggplot(aes(x = reorder(Quintile, Income), y = Income, fill=1)) +
  geom_col() +
  labs(title="2019 Income Quintiles",
       x="Quintiles",
       caption = "Source: Tax Policy Center") +
  theme_bw() +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$")) +
  theme(legend.position = "none") 

### ratio of top to bottom income inequality
earned_income_inequality <- income_quintiles %>%
  mutate(inequality = Earned_Top / Earned_Lowest) %>%
  ggplot(aes(Year, inequality, fill=1)) +
  geom_line(size=1) +
  theme_bw() +
  labs(title="Income Inequality", 
       subtitle="Percent Difference Between Top Quintile & Bottom Quintile",
       y="Inequality",
       caption = "Source: TaxPolicyCenter.org") +
  theme(legend.position = "none") 

### income type
income_type <- income_quintiles %>%
  pivot_longer(-Year, names_to = "Quintile", values_to = "Income") %>%
  mutate(income_type = ifelse(grepl("Earned", Quintile), "Earned", 
                              ifelse(grepl("Transfers", Quintile), "Transfers", 
                                     ifelse(grepl("take_home", Quintile), "take_home", "Payroll"))))


### Quintile adjustment
income_type <- income_type %>%
  mutate(Quintile = str_extract(Quintile, "Lowest|Second|Middle|Fourth|Top"))

### adjusted income inequality
adjusted_inequality <- income_type %>%
  filter(income_type %in% c("Transfers", "take_home"),
         Quintile %in% c("Top", "Lowest")) %>%
  group_by(Year, Quintile) %>%
  summarise(Income = sum(Income)) %>%
  pivot_wider(names_from = Quintile, values_from = Income) %>% 
  mutate(adjusted_inequality = Top / Lowest)
ggplot(aes(x = Year, y = inequality)) +
  geom_line() +
  labs(title = "Income Ratio: Top to Lowest Quintile",
       x = "Year",
       y = "Income Ratio") +
  theme_minimal()
