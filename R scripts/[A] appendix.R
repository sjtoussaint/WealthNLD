# install.packages(pacman)

pacman::p_load(tidyverse, readxl, stargazer, fixest, paletteer, tidyquant, ggformula)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))


theme_set(theme_clean())

dd <- read_excel("../../Public/WealthNLD.xlsx",
                 sheet = "Death Duties")

wi <- read_excel("../../Public/WealthNLD.xlsx",
                 sheet = "Overview")

dd %>% inner_join(wi) %>%
  rename("1921 Multiplier" = `Household Wealth (1921 Multiplier)`,
         "Time-Varying Multiplier" = `Household Wealth (Time-Varying Multiplier)`,
         "Average" = `Household Wealth (Average)`) %>%
  pivot_longer(4:6, names_to = "Method", values_to = "Value") %>%
  mutate(value = Value/`National Income`) %>%
  ggplot(aes(x = Year, y = value, color = Method, shape = Method)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = seq(1850, 1990, by = 20),
                     limits = c(1850, 1990)) +
  scale_y_continuous(limits = c(0, 12.5),
                     breaks = seq(0, 12, by = 2),
                     labels = scales::label_percent(accuracy = 1L)) +
  labs(y = "Wealth-Income Ratio") +
  scale_color_paletteer_d("ggthemes::calc") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size=16),
        axis.title = element_text(size=18),
        axis.text = element_text(size=16))

ggsave("dd_methods.pdf", 
       path = "../../Public/Figures",
       width = 11.69,
       height = 8.27,
       units = "in")
