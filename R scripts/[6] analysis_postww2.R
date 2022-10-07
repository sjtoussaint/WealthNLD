# install.packages(pacman)

pacman::p_load(tidyverse, readxl, stargazer, fixest, paletteer, writexl, tidyquant)

theme_set(ggthemes::theme_clean())

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

wealthincome <- read_excel("../../Public/WealthNLD.xlsx",
                           sheet = "Savings v Capital Gains") %>%
  rename("Year" = "year")

pop <- read_excel("../../Public/WealthNLD.xlsx",
                  sheet = "Overview")

# clean data

wealthincome %>% inner_join(pop) %>%
  select(Year, `real wealth (eur)`, `real income (eur)`, Population) %>%
  rename("Wealth" = `real wealth (eur)`,
         "Income" = `real income (eur)`) %>%
  pivot_longer(Wealth:Income, names_to = "category",
               values_to = "val") %>%
  mutate(val = val/(Population*1000)) %>%
  filter(Year < 1980,
         Year > 1946) %>%
  ggplot(aes(x = Year, y = val, color = category)) +
  geom_point() +
  geom_ma(n = 3) +
  labs(y = "Constant 2015 EUR Per Capita (Thousands)",
       x = "Year") +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 16),
        legend.title = element_blank(),
        legend.position = "bottom") +
  scale_x_continuous(breaks = seq(1945, 1980, by = 5)) +
  scale_y_continuous(breaks = seq(0, 70, by = 20),
                     limits = c(0, 70),
                     labels = scales::comma)  +
  scale_color_paletteer_d("ggthemes::calc")

ggsave("realwealthincome_1947_1980.pdf", 
       path = "../../Public/Figures",
       width = 11.69,
       height = 8.27,
       units = "in")


wealthincome %>% inner_join(pop) %>%
  select(Year, `real wealth (eur)`, `real income (eur)`, Population) %>%
  rename("Wealth" = `real wealth (eur)`,
         "Income" = `real income (eur)`) %>%
  pivot_longer(Wealth:Income, names_to = "category",
               values_to = "val") %>%
  mutate(val = val/(Population*1000)) %>%
  filter(Year >= 1980) %>%
  ggplot(aes(x = Year, y = val, color = category)) +
  geom_point() +
  geom_ma(n = 3) +
  labs(y = "Constant 2015 EUR Per Capita (Thousands)",
       x = "Year") +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 16),
        legend.title = element_blank(),
        legend.position = "bottom") +
  scale_x_continuous(breaks = seq(1980, 2020, by = 5)) +
  scale_y_continuous(breaks = seq(0, 200, by = 25),
                     limits = c(0, 200),
                     labels = scales::comma)  +
  scale_color_paletteer_d("ggthemes::calc")

ggsave("realwealthincome_1980_2019.pdf", 
       path = "../../Public/Figures",
       width = 11.69,
       height = 8.27,
       units = "in")

