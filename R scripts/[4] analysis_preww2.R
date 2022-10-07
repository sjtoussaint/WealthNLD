# install.packages(pacman)

pacman::p_load(tidyverse, readxl, stargazer, fixest, paletteer, tidyquant, ggformula)

theme_set(ggthemes::theme_clean())

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

wealthincome <- read_excel("../../Public/WealthNLD.xlsx",
                           sheet = "Overview")

return <- read_excel("../../Public/WealthNLD.xlsx",
                     sheet = "National Income")

nfa <- read_excel("../../Public/WealthNLD.xlsx",
                  sheet = "Historical National Accounts")

world <- read_excel("../../Public/WealthNLD.xlsx",
                    sheet = "International - NFA")

income <- read_excel("../../Public/WealthNLD.xlsx",
                     sheet = "International - Foreign Income")

capitalshares <- read_excel("../../Public/WealthNLD.xlsx",
                            sheet = "International - Capital Shares")

mpd <- read_excel("../../Public/WealthNLD.xlsx",
                  sheet = "International - GDP")
  



# capital shares world----

return %>% 
  select(year, `capital share`) %>% 
  full_join(capitalshares) %>%
  select(1:8) %>%
  mutate(`capital share` = `capital share`*100) %>%
  rename("Netherlands" = `capital share`,
         "Sweden" = "csn_se",
         "Spain" = "csn_es",
         "United States" = "csn_us",
         "United Kingdom" = "csn_uk",
         "Germany" = "csn_de",
         "France" = "csn_fr") %>%
  filter(year < 1940) %>%
  pivot_longer(-year, names_to = "Country", values_to = "Value") %>%
  mutate(Value = Value/100) %>%
  ggplot(aes(x = year, y = Value, color = Country)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(name = "Year",
                     breaks = seq(1850,1940, by = 10)) +
  scale_y_continuous(name = "Capital Share",
                     breaks = seq(0, 0.6, by = 0.1),
                     limits = c(0, 0.6)) +
  scale_color_paletteer_d("ggthemes::calc") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size=16),
        axis.title = element_text(size=18),
        axis.text = element_text(size=16))
ggsave("capitalshare_1854_1938.pdf", 
       path = "../../Public/Figures",
       width = 11.69,
       height = 8.27,
       units = "in")


# return to wealth----

return %>%
  filter(year < 1940) %>%
  ggplot(aes(x = year, y = `return to wealth`)) + 
  geom_point() +
  geom_line() +
  scale_y_continuous(limits = c(0, 0.1),
                     breaks = seq(0, 0.1, by = 0.02),
                     labels = scales::label_percent(accuracy = 1L)) +
  scale_x_continuous(breaks = seq(1850, 1940, by = 10)) +
  labs(x = "Year",
       y = "Return to Wealth") +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 16))

ggsave("wealthreturn_1854_1938.pdf", 
       path = "../../Public/Figures",
       width = 11.69,
       height = 8.27,
       units = "in")

# foreign vs domestic wealth----

nfa %>%
  mutate(Financial = Securities + Deposits + `Nonlisted Firms`,
         `Domestic Securities` = (Securities - `Foreign Securities (Net)` + 
                                    `Nonlisted Firms`)/Financial,
         `Foreign Securities` = `Foreign Securities (Net)`/Financial,
         Deposits = Deposits/Financial) %>%
  select(Year, NNI, Financial, `Foreign Securities`, 
         Deposits, `Domestic Securities`) %>%
  filter(Year < 1940) %>%
  pivot_longer(4:6, names_to = "name", values_to = "value_pct") %>%
  ggplot(aes(x = Year, y = value_pct, fill = name)) + 
  geom_bar(stat = "identity") +
  scale_fill_paletteer_d("ggthemes::calc") +
  labs(y = "In \\% of Financial Assets") +
  scale_x_continuous(breaks = seq(1880, 1940, by = 10)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2)) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16))

ggsave("financialassets_1880_1938.pdf", 
       path = "../../Public/Figures",
       width = 11.69,
       height = 8.27,
       units = "in")

nfa %>%
  mutate(Colonial = `Colonial Wealth`/`Net wealth (total)`,
         `Other Foreign` = `Non-Colonial Foreign Securities`/`Net wealth (total)`) %>%
  pivot_longer(Colonial:`Other Foreign`, names_to = "name", values_to = "value") %>%
  filter(Year < 1940) %>%
  ggplot(aes(x = Year, y = value, shape = name, color = name)) + 
  geom_point() +
  geom_line() +
  scale_color_paletteer_d("ggthemes::calc") +
  scale_x_continuous(breaks = seq(1880, 1940, by = 10)) +
  scale_y_continuous(breaks = seq(0, 0.5, by = 0.05),
                     labels = scales::label_percent(accuracy = 1L)) +
  labs(y = "In \\% of Household Wealth") +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 16))

ggsave("foreignwealth_1880_1938.pdf", 
       path = "../../Public/Figures",
       width = 11.69,
       height = 8.27,
       units = "in")

nfa %>%
  mutate(Colonial = `Colonial Wealth`/`NNI`,
         `Other Foreign` = `Non-Colonial Foreign Securities`/`NNI`) %>%
  pivot_longer(Colonial:`Other Foreign`, names_to = "name", values_to = "value") %>%
  filter(Year < 1940) %>%
  ggplot(aes(x = Year, y = value, color = name, shape = name)) + 
  geom_point() +
  geom_line() +
  scale_color_paletteer_d("ggthemes::calc") +
  scale_x_continuous(breaks = seq(1880, 1940, by = 10)) +
  scale_y_continuous(breaks = seq(0, 3, by = 0.25),
                     labels = scales::label_percent(accuracy = 1L)) +
  labs(y = "In \\% of National Income") +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 16))

ggsave("foreignwealth_1880_1938_nni.pdf", 
       path = "../../Public/Figures",
       width = 11.69,
       height = 8.27,
       units = "in")


# NFA World----

world %>%
  pivot_longer(UK:Netherlands, names_to = "Country",
               values_to = "NFA") %>% 
  ggplot(aes(x = Year, y = NFA, color = Country)) +
  geom_point() +
  geom_line() +
  scale_color_paletteer_d("ggthemes::calc") +
  scale_x_continuous(breaks = seq(1850, 1940, by = 10)) +
  scale_y_continuous(breaks = seq(-1, 3, by = 0.5),
                     labels = scales::percent) +
  labs(y = "NFA in \\% of National Income") +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 16),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18))

ggsave("nfa_world.pdf", 
       path = "../../Public/Figures",
       width = 11.69,
       height = 8.27,
       units = "in")


income %>%
  pivot_longer(UK:Netherlands, names_to = "Country", values_to = "value") %>%
  ggplot(aes(x = Year, y = value, color = Country)) +
  geom_point() +
  geom_line() +
  scale_color_paletteer_d("ggthemes::calc") +
  scale_x_continuous(breaks = seq(1850, 1940, by = 10)) +
  scale_y_continuous(name = "Foreign Investment as \\% of National Income",
                     limits = c(0, 0.17),
                     breaks = seq(0, 0.15, by = 0.05),
                     labels = scales::percent) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size=16),
        axis.text = element_text(size=16),
        axis.title = element_text(size=18))

ggsave("foreign_income.pdf", 
       path = "../../Public/Figures",
       width = 11.69,
       height = 8.27,
       units = "in")

# GDP World----

mpd %>%
  filter(year < 1901) %>%
  ggplot(aes(x = year, y = gdppc, color = country)) + geom_line() +
  labs(x = "Year",
       y = "GDP per Capita") +
  scale_x_continuous(breaks = seq(1850, 1940, by = 10)) +
  scale_y_continuous(breaks = seq(2000, 12000, by = 2000),
                     labels = scales::label_dollar()) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16))

ggsave("gdp_percapita_1854_1938.pdf", 
       path = "../../Public/Figures",
       width = 11.69,
       height = 8.27,
       units = "in")
