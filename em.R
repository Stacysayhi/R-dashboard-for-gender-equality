#install.packages("Rilostat")
#if(!require(devtools)){install.packages('devtools')}
library(devtools)
#install_github("ilostat/Rilostat")
library(Rilostat)
#install.packages("countrycode")
library(countrycode)
library(dplyr)

toc <-get_ilostat_toc(search = 'Employment by sex and age')
employ <- get_ilostat("EMP_2EMP_SEX_AGE_NB_A") %>%
  rename(Country = ref_area) %>%
  mutate(sex = case_when(
    sex == "SEX_M" ~ "Male",
    sex == "SEX_F" ~ "Female",
    TRUE ~ sex
  )) %>%
  filter(sex %in% c("Male", "Female")) %>%
  rename(Age = classif1) %>%
  filter(Age == "AGE_YTHADULT_YGE15") %>%
    filter(time >= 2000 & time <= 2022) %>%
  rename(Employment = obs_value) %>%
  select(Country, sex, time, Employment) %>%
  arrange(Country, desc(time)) %>%
  mutate(Country = countrycode(Country, "iso3c", "country.name"))
employ <- employ %>% filter(!is.na(Country))
employ$Region <- countrycode(employ$Country, "country.name", "region")
employ <- employ %>% mutate(Region = ifelse(Country == "Western Sahara", "Middle East & North Africa", Region))
missing_values <- colSums(is.null(employ) | is.na(employ))
employ <- employ %>%
  group_by(Region) %>%
  mutate(nums_country_region = n_distinct(Country))
# Tạo dataframe mới từ dữ liệu "employ" với các cột chỉ định
rate_employ <- employ %>%
  select(Country, time, sex, Employment) %>%
  group_by(Country, time) %>%
  summarise(Employment = sum(ifelse(sex == "Female", Employment, 0)) / sum(ifelse(sex == "Male", Employment, 0)))
colnames(rate_employ)[2] <- "Year"
dfe<-rate_employ

chart_em <- employ %>%
  select(Country, time, sex, Employment,Region) %>%
  group_by(Country, time,Region) %>%
  summarise(Employment = sum(ifelse(sex == "Female", Employment, 0)) / sum(ifelse(sex == "Male", Employment, 0)))
colnames(chart_em)[2] <- "Year"

