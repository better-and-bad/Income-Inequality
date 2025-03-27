

### income inequality
rm(list=ls())

### libraries
library(readxl)
library(dplyr)
library(ggplot2)

### data
income_quintiles <- read_excel("/Users/jackconnors/Downloads/income_quintiles_5.xlsx",
                               sheet="Sheet1")

### plot 2021 quintiles
library(tidyverse)

### set plot theme
theme_set(
  theme_minimal(base_size = 14) +
    theme(
      axis.title = element_text(size=16),
      plot.title = element_text(hjust = 0.5, face="bold", size=18),
      plot.subtitle = element_text(hjust = 0.5, face="bold", size=16),
      plot.caption = element_text(size=12),
      legend.position = "bottom",
      legend.title = element_blank()
    )
)

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
  ### plot the adjusted_inequality
  
  #ggplot(aes(x = Year, y = adjusted_inequality)) +
  #geom_line() +
  #labs(title = "Income Ratio: Top to Lowest Quintile",
  #     x = "Year",
  #     y = "Income Ratio") +
  #theme_minimal()
  
  cb <- income_type %>%
    filter(income_type %in% c("Earned"),
           Quintile %in% c("Top", "Lowest")) %>%
    group_by(Year, Quintile) %>% 
    summarise(Income = sum(Income)) %>% 
    pivot_wider(names_from = Quintile, values_from = Income) %>% 
    mutate(cb = Top / Lowest)

  combined_inequality <- merge(adjusted_inequality, cb, by="Year")
  
### adjusted & earned inequality
  # Convert to long format
  library(tidyr)
  library(dplyr)
  library(ggplot2)
  
  inequality_long <- combined_inequality %>%
    pivot_longer(
      cols = c(adjusted_inequality, cb),
      names_to = "Type",
      values_to = "Inequality"
    ) %>%
    mutate(Type = recode(Type,
                         adjusted_inequality = "Actual Inequality",
                         cb = "Reported Inequality"))
  
  ### plot reported and actual inequality
  combined_inequality %>% 
    mutate(Year = as.Date(paste0(Year, "-01-01"))) %>% 
    ggplot() +
    geom_bar(aes(Year, cb, fill="Reported Inequality"), stat="identity", alpha=0.7) +
    geom_bar(aes(Year, adjusted_inequality, fill="Actual Inequality"), alpha= 0.7, stat="identity")+
      scale_fill_manual(values = c("Actual Inequality" = "blue",
                                   "Reported Inequality" = "red")) +
    labs(title = "Income Inequality Is Over Hyped", y= "Inequality (%)", x="",
         caption= "TaxPolicyCenter.Org")
  
### income type
income_type <- income_quintiles %>%
  pivot_longer(-Year, names_to = "Quintile", values_to = "Income") %>%
  mutate(income_type = ifelse(grepl("Earned", Quintile), "Earned", 
                ifelse(grepl("Transfers", Quintile), "Transfers", 
                  ifelse(grepl("take_home", Quintile), "take_home", "Payroll"))))


### Quintile adjustment
income_type <- income_type %>%
  mutate(Quintile = str_extract(Quintile, "Lowest|Second|Middle|Fourth|Top"))


### transfer adjusted income
transfers <- income_type %>%
  filter(Year == "2019") %>%
  filter(income_type %in% c("Earned", "Transfers")) %>%
  ggplot(aes(reorder(Quintile, Income), Income, fill=income_type)) +
  geom_col() +
  scale_y_continuous(labels = scales::dollar) +
  labs(title="Income Quintiles with Transfers", subtitle="2019",
       x="Quintiles") 
  theme_bw()

### payroll plots
payroll <- income_type %>%
  filter(income_type %in% "Payroll") %>%
  ggplot(aes(reorder(Quintile, Income), Income), fill=1) +
  geom_col() +
  labs(title="Payroll Taxes",
       y="Taxes ($)", x="Quintiles") +
  theme_bw() +
  theme(legend.position = "none")

library(cowplot)
plot_grid(earned, adjusted, ncol = 2)

### final take home + transfers pay
adjusted <- income_type %>% 
  filter(Year=="2019") %>% 
  mutate(
    income_type = case_when(
      income_type == "Transfers" ~ "Transfer Payments",
      income_type == "take_home" ~ "Take Home Pay",
      income_type == "Earned" ~ "Earned Income",
      income_type == "Payroll" ~ "Payroll Taxes",
      TRUE ~  income_type
    )) %>% 
  filter(income_type != "Take Home Pay") %>% 
 # filter(income_type %in% c("Transfer Payments", "Take Home Pay", "Payroll")) %>% 
  ggplot(aes(reorder(Quintile, Income), Income, fill=income_type)) +
    geom_col() +
    scale_y_continuous(breaks = seq(0, 300000, 50000), limits = c(0, 300000),
                       labels = scales::dollar_format()) +
    labs(title="Income Is More Than What You Earn", subtitle = "2019", 
         x="Income Quintiles", y ="",  caption="Source: TaxPolicyCenter.org")

### earned income
earned <- income_type %>%
  filter(Year=="2019") %>% 
  filter(income_type %in% "Earned") %>% 
  ggplot(aes(reorder(Quintile, Income), Income)) +
  geom_col(fill="darkblue") +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0, 300000, 50000), limits = c(0, 300000)) +
    labs(title="Pretax Earned Income", x="Quintiles", subtitle="Data Used by Census Bureau",
       caption="Source: TaxPolicyCenter.org")

### revised income inequality
  ### transfers and take home pay
adjusted_income_inequality <- income_type %>%
  filter(income_type %in% c("Transfers", "take_home")) %>%
  group_by(Year, Quintile) %>%
  summarize(total = sum(Income)) %>%
mutate(percent_diff = ((total[Quintile == "Top"] / total[Quintile == "Lowest"]))) %>%
  ggplot(aes(Year, percent_diff, fill=1)) +
  geom_area() +
  theme_minimal() +
  labs(title="Transfers and Payroll Tax Adjusted Income Inequality",
       subtitle="Ratio of Top to Bottom Income Quintiles",
       y="Inequality Ratio") +
  theme(legend.position = "none")

