

cb <- income_type %>%
  filter(income_type %in% c("Earned"),
         Quintile %in% c("Top", "Lowest")) %>%
  group_by(Year, Quintile) %>% 
  summarise(Income = sum(Income)) %>% 
  pivot_wider(names_from = Quintile, values_from = Income) %>% 
  mutate(cb = Top / Lowest)

combined_inequality <- merge(adjusted_inequality, cb, by="Year")

### adjusted & earned inequality
ggplot(combined_inequality) +
  geom_bar(aes(Year, adjusted_inequality, fill = "Adjusted Inequality"), stat = "identity", alpha = 0.5) +
  geom_bar(aes(Year, cb, fill = "Earned Income Inequality"), stat = "identity", alpha = 0.5) +
  ylab("Inequality (%)") +
  scale_fill_manual(values = c("Adjusted Inequality" = "blue", "Earned Income Inequality" = "black"), 
                    labels = c("Adjusted Inequality", "Earned Income Inequality")) +
  theme_minimal() +
  labs(title="Adjusted and Earned Income Inequality", caption="Source: TaxPolicyCenter.org")


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
       x="Quintiles") +
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
  filter(income_type %in% c("Transfers", "take_home")) %>% 
  ggplot(aes(reorder(Quintile, Income), Income, fill=income_type)) +
  geom_col() +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0, 300000, 50000), limits = c(0, 300000)) +
  labs(title="Adusted Income Quintiles", x="Quintiles", 
       caption="Source: TaxPolicyCenter.org")

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
