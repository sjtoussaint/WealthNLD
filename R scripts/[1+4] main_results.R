# install.packages(pacman)

pacman::p_load(tidyverse, readxl, stargazer, fixest, paletteer, tidyquant, ggformula)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))


theme_set(theme_clean())

#### Load data ####
##### Main data #####
gvw <- read_excel("../../Public/WealthNLD.xlsx",
                  sheet = "Overview")

comp_shares <- read_excel("../../Public/WealthNLD.xlsx",
                          sheet = "International - Wealth Shares")

comp_wi <- read_excel("../../Public/WealthNLD.xlsx",
                      sheet = "International - Wealth-Income")

ws <- read_excel("../../Public/WealthNLD.xlsx",
                 sheet = "Wealth Shares")

composition <- read_excel("../../Public/WealthNLD.xlsx",
                          sheet = "Historical National Accounts")

#### Clean data ####

### Without Pensions & Insurance

gvw_no_pens <- gvw %>% 
  pivot_longer(5:7, names_to = "variable", values_to = "values") %>%
  mutate(values = values/`National Income`)

### With Pensions & Insurance

gvw_with_pens <- gvw %>% 
  pivot_longer(c(5,8,9), names_to = "variable", values_to = "values") %>%
  mutate(values = values/`National Income`)


##### NL vs World ##### 

comp_shares_l <- comp_shares %>%
  pivot_longer(!Year, names_to = "variable", values_to = "values")
comp_shares_l[comp_shares_l == 0] <- NA

comp_wi_l <- comp_wi %>%
  pivot_longer(!Year, names_to = "variable", values_to = "values")

comp_wi_l[comp_wi_l == 0] <- NA


#### Composition ####

comp_l <- composition %>%
  mutate(Financial = Securities + Deposits) %>%
  select(Year, Financial, Housing, `Agricultural Land`,
         Liabilities, `Fixed Capital Stock`, `Semiprivate Wealth`,
         `Net wealth (total)`, `Gross Wealth`, NNI) %>%
  pivot_longer(2:7, names_to = "Wealth Component", values_to = "Value")


#### Figures ####

##### Everything ####

gvw %>%
  ggplot(aes(x = Year, y = `Everything over NNI`)) + 
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = seq(1850, 2020, by = 20)) +
  scale_y_continuous(breaks = seq(0, 9, by = 1),
                     limits = c(0, 9.5),
                     labels = scales::percent) +
  labs(y = "Wealth-Income Ratio") +
  theme(axis.title = element_text(size=18),
        axis.text = element_text(size=16))

ggsave("Everything_wi.pdf", 
       path = "../../Public/Figures",
       width = 11.69,
       height = 8.27,
       units = "in")

ws %>%
  filter(Year < 1940 | Year > 1945) %>%
  ggplot(aes(x = Year, y = `Top 1%`)) +
  geom_point() + geom_line() +
  scale_x_continuous(breaks = seq(1890, 2020, by = 10)) +
  scale_y_continuous(limits = c(0, 0.6),
                     breaks = seq(0, 0.6, by = 0.1),
                     labels = scales::label_percent(accuracy = 1L)) +
  theme(axis.title = element_text(size=18),
        axis.text = element_text(size=16))

ggsave("Everything_ws.pdf", 
       path = "../../Public/Figures",
       width = 11.69,
       height = 8.27,
       units = "in")


##### Without Pensions & Insurance ##### 

gvw_no_pens %>% 
  filter(Year >= 1854,
         Year < 1939 | Year > 1945) %>%
  mutate(values = values) %>%
  ggplot(aes(x= Year, y = values, group = variable, color = variable)) + 
  geom_point(size = 1.5) +
  geom_ma(n = 3, na.rm = TRUE, size = 1) +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0, 12.5, by = 1)) +
  ylab("Wealth-Income Ratio") +
  xlab("Year") +
  guides(color=guide_legend("")) +
  scale_x_continuous(breaks = seq(1850,2020, by = 10)) +
  theme_clean() +
  scale_color_paletteer_d("ggthemes::colorblind") +
  theme(axis.title = element_text(size=18), 
        axis.text = element_text(size = 16), 
        legend.position = "bottom",
        legend.text = element_text(size=16))
ggsave("wi_threemethods.pdf", 
       path = "../../Public/Figures",
       width = 11.69,
       height = 8.27,
       units = "in")


##### With Pensions & Insurance ####
gvw_with_pens %>% 
  filter(Year >= 1854,
         Year < 1939 | Year > 1945) %>%
  ggplot(aes(x= Year, y = values, group = variable, color = variable)) + 
  geom_point(size = 1.5) +
  guides(color=guide_legend("")) +
  geom_ma(n = 3, na.rm = TRUE, size = 1) +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0, 12.5, by = 1)) +
  ylab("Wealth-Income Ratio") +
  xlab("Year") +
  #ggtitle("Estimates of aggregate wealth over national net income", subtitle = "Including Pensions") +
  scale_x_continuous(breaks = seq(1850,2020, by = 10)) +
  scale_colour_paletteer_d("ggthemes::colorblind") +
  theme_clean() +
  theme(axis.title = element_text(size=20), 
        axis.text = element_text(size = 16), 
        legend.position = "bottom",
        legend.text = element_text(size = 16))
ggsave("wi_threemethods_withpensions.pdf", 
       path = "../../R Scripts/Pictures",
       width = 11.69,
       height = 8.27,
       units = "in")

##### Wealth Shares #####

ws %>%
  pivot_longer(2:4, names_to = "variable", values_to = "values") %>%
  ggplot(aes(x= Year, y = values, shape = variable, color = variable)) + 
  geom_point(size = 1.5) +
  geom_ma(n = 4, na.rm = TRUE, size = 1) +
  ylab("Wealth Shares") +
  xlab("Year") +
  scale_color_paletteer_d("ggthemes::calc") +
  theme(axis.title = element_text(size=18), 
        axis.text = element_text(size=16),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size=16)) +
  scale_y_continuous(breaks = round(seq(0, 1, by = 0.05),1),
                     limits = c(0, 0.8),
                     labels = scales::percent) + 
  scale_x_continuous(breaks = seq(1890, 2020, by = 10)) 
ggsave("wealthshares.pdf", 
       path = "../../Public/Figures",
       width = 11.69,
       height = 8.27,
       units = "in")


##### Composition #####

comp_l %>%
  mutate(value = Value/`Gross Wealth`) %>%
  ggplot(aes(x= Year, y = value, fill = `Wealth Component`)) + 
  geom_bar(stat = "identity") +
  ylab("In \\% of Total Assets") +
  xlab("Year") +
  scale_fill_paletteer_d("ggthemes::calc") +
  theme(axis.title = element_text(size=18), 
        axis.text = element_text(size = 16))  +
  scale_x_continuous(breaks = seq(1880, 2020, 10)) +
  scale_y_continuous(breaks = seq(-1, 1, 0.25), labels = scales::percent) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 16))

ggsave("wealthcomposition_1880_2019.pdf", 
       path = "../../Public/Figures",
       width = 11.69,
       height = 8.27,
       units = "in")


comp_l %>%
  mutate(value = Value/NNI) %>%
  ggplot(aes(x= Year, y = value, fill = `Wealth Component`)) + 
  geom_bar(stat = "identity") +
  ylab("In \\% of National Income") +
  xlab("Year") +
  scale_fill_paletteer_d("ggthemes::calc") +
  theme(axis.title = element_text(size=18), 
        axis.text = element_text(size = 16))  +
  scale_x_continuous(breaks = seq(1880, 2020, 10)) +
  scale_y_continuous(breaks = seq(-2, 10, 1), labels = scales::percent) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size=16))

ggsave("wealthcomposition_1880_2019_nni.pdf", 
       path = "../../Public/Figures",
       width = 11.69,
       height = 8.27,
       units = "in")


##### NL vs World ####

comp_wi %>%
  select(-`UK (Piketty-Zucman)`,
         -`Germany (Piketty-Zucman)`) %>%
  rename(Germany = `Germany (Albers-Bartels-Schularick)`,
         UK = `UK (Madsen)`) %>%
  pivot_longer(-Year, names_to = "Country", values_to = "values")  %>%
  mutate(Category = ifelse(Country %in% c("Netherlands", "UK", "France"), 
                           "Major Colonial Power", "Other Countries")) %>%
  group_by(Country) %>%
  ggplot(aes(x= Year, y = values, group = Country, color = Country)) + 
  geom_point(size = 1.5, position = position_jitter()) +
  guides(color=guide_legend("")) +
  geom_line() +
  ylab("Wealth-Income Ratio") +
  xlab("Year") +
  scale_color_paletteer_d("ggthemes::few_Dark") +
  theme_clean() +
  theme(axis.title = element_text(size=18), 
        axis.text = element_text(size=16),
        strip.text = element_text(size=16),
        legend.position = "bottom",
        legend.text = element_text(size=16)) +
  scale_y_continuous(breaks = seq(0, 12.5, by = 1),
                     labels = scales::percent) +
  scale_x_continuous(breaks = round(seq(1850, 2020, by = 20),1)) +
  facet_wrap(~Category,
             ncol = 1)
ggsave("international_wi.pdf", 
       path = "../../Public/Figures",
       width = 11.69,
       height = 8.27,
       units = "in")


##### NL vs World (wealth shares) ####
comp_shares_l %>% 
  filter(Year < 1939 | Year > 1945) %>%
  ggplot(aes(x= Year, y = values, group = variable, color = variable)) + 
  geom_point(size = 1.5) +
  guides(color=guide_legend("")) +
  geom_ma(n = 3, na.rm = TRUE, size = 1) +
  ylab("Top 1% Share") +
  xlab("Year") +
  scale_color_paletteer_d("ggthemes::calc") +
  theme_clean() +
  theme(axis.title = element_text(size=18), 
        axis.text = element_text(size=16),
        legend.text = element_text(size=16),
        legend.position = "bottom") +
  scale_y_continuous(breaks = round(seq(0, 0.8, by = 0.1),1),
                     labels = scales::percent) +
  scale_x_continuous(breaks = round(seq(1850, 2020, by = 10),1)) 
ggsave("international_top1.pdf", 
       path = "../../Public/Figures",
       width = 11.69,
       height = 8.27,
       units = "in")
