library(tidyverse)
library(readxl)
library(lubridate)
library(cowplot)
theme_set(theme_cowplot())


first_date <- ymd("2020-04-08")
tsa_df <- readxl::read_xlsx("raw-data/tsa_hospitalizations/Texas COVID-19 Hospitalizations by TSA.xlsx", skip = 2) %>% 
  gather(date, hospitalizations, -`TSA ID`, -`TSA Name`) %>% 
  rename(tsa_id = `TSA ID`,
         tsa = `TSA Name`) %>% 
  mutate(date = first_date + days(as.numeric(date) - min(as.numeric(date))))


tsa_df %>% 
  filter(tsa == "Statewide Total") %>% 
  ggplot(aes(date, hospitalizations)) + 
  geom_line() +
  background_grid(major = "xy", minor = "xy")


tsa_df %>% 
  filter(tsa != "Statewide Total") %>% 
  ggplot(aes(date, hospitalizations)) + 
    geom_line() +
    facet_wrap(~tsa, scales = "free_y") +
    background_grid(major = "xy", minor = "xy") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          legend.position = "none")
  
tsa_df %>% 
  mutate(hospitalizations = hospitalizations + 1) %>% 
  filter(date == ymd("2020-06-01") | date == max(date)) %>% 
  spread(date, hospitalizations) %>% 
  mutate(percent_change = (`2020-06-22` - `2020-06-01`) / `2020-06-01`,
         trend = ifelse(percent_change > 0, "Increasing", "Decreasing")) %>% 
  mutate(tsa_lab = forcats::fct_reorder(tsa, percent_change)) %>% 
  ggplot(aes(tsa_lab, percent_change, fill = trend)) + 
    geom_col() +
    scale_y_continuous(labels = scales::percent) +
    geom_hline(yintercept = 0, lty = 2) +
    labs(x = "TSA", y = "Percent change since June 1st") +
    scale_fill_brewer(type = "qual", palette = 2)  +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          legend.position = "none") +
    background_grid(major = "y", minor = "y")
  
## Trauma areas
## https://www.dshs.texas.gov/emstraumasystems/etrarac.shtm