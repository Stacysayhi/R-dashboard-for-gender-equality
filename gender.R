library(rvest)

base_url <- "https://data.un.org/Data.aspx?d=POP&f=tableCode%3a1%3brefYear%3a2000%2c2001%2c2002%2c2003%2c2004%2c2005%2c2006%2c2007%2c2008%2c2009%2c2010%2c2011%2c2012%2c2013%2c2014%2c2015%2c2016%2c2017%2c2018%2c2019%2c2020%2c2021%2c2022%3bareaCode%3a0%3bsexCode%3a1%2c2&c=2,3,6,8,10,12,13,14&s=_countryEnglishNameOrderBy:asc,refYear:desc,areaCode:asc&v="

data_part_2 <- data.frame()  # Khởi tạo data frame rỗng

for (i in 1:50) {
  url <- paste0(base_url, i)
  page <- read_html(url)
  
  # Trích xuất dữ liệu từ phần tử HTML (điều chỉnh CSS selector theo yêu cầu)
  dataset_df <- page %>%
    html_nodes("table") %>%
    html_table(fill = TRUE)
  
  # Gộp dữ liệu vào data frame chính
  if (!is.null(dataset_df[[2]])) {
    data_part_2 <- rbind(data_part_2, dataset_df[[2]])
  }
}

for (i in 51:100) {
  url <- paste0(base_url, i)
  page <- read_html(url)
  
  # Trích xuất dữ liệu từ phần tử HTML (điều chỉnh CSS selector theo yêu cầu)
  dataset_df <- page %>%
    html_nodes("table") %>%
    html_table(fill = TRUE)
  
  # Gộp dữ liệu vào data frame chính
  if (!is.null(dataset_df[[2]])) {
    data_part_2 <- rbind(data_part_2, dataset_df[[2]])
  }
}

for (i in 101:153) {
  url <- paste0(base_url, i)
  page <- read_html(url)
  
  # Trích xuất dữ liệu từ phần tử HTML (điều chỉnh CSS selector theo yêu cầu)
  dataset_df <- page %>%
    html_nodes("table") %>%
    html_table(fill = TRUE)
  
  # Gộp dữ liệu vào data frame chính
  if (!is.null(dataset_df[[2]])) {
    data_part_2 <- rbind(data_part_2, dataset_df[[2]])
  }
}
for (i in 154:200) {
  url <- paste0(base_url, i)
  page <- read_html(url)
  
  # Trích xuất dữ liệu từ phần tử HTML (điều chỉnh CSS selector theo yêu cầu)
  dataset_df <- page %>%
    html_nodes("table") %>%
    html_table(fill = TRUE)
  
  # Gộp dữ liệu vào data frame chính
  if (!is.null(dataset_df[[2]])) {
    data_part_2 <- rbind(data_part_2, dataset_df[[2]])
  }
}
for (i in 201:230) {
  url <- paste0(base_url, i)
  page <- read_html(url)
  
  # Trích xuất dữ liệu từ phần tử HTML (điều chỉnh CSS selector theo yêu cầu)
  dataset_df <- page %>%
    html_nodes("table") %>%
    html_table(fill = TRUE)
  
  # Gộp dữ liệu vào data frame chính
  if (!is.null(dataset_df[[2]])) {
    data_part_2 <- rbind(data_part_2, dataset_df[[2]])
  }
}

genderdf <- data_part_2
#Xử lí genderdf
genderdf <- genderdf[, -c(3,5:7,9)]
colnames(genderdf)[colnames(genderdf) == "Value"] <- "Gender"
head(genderdf,5)

#install.packages("countrycode")
library(countrycode)
genderdf1 <- data.frame(genderdf)
#Kiểm tra liệu có giá trị null hay na
column_names <- names(genderdf1)
print(column_names)

for (col_name in column_names) {
  num_null <- sum(is.null(genderdf1[[col_name]]))
  num_na <- sum(is.na(genderdf1[[col_name]]))
  
  print(paste("Column:", col_name))
  print(paste("Number of null:", num_null))
  print(paste("Number of NA:", num_na))
  print("")
}

genderdf$Country <- genderdf$`Country or Area`
# Chuyển đổi mã quốc gia thành khu vực
genderdf11 <- data.frame(genderdf1)
genderdf11$Region <- countrycode(genderdf11$Country, "country.name", "region")

genderdf11$Region <- ifelse(genderdf11$Country %in% c("Mayotte", "Reunion"), "Sub-Saharan Africa", genderdf11$Region)
genderdf11$Region <- ifelse(genderdf11$Country %in% c("Saint Helena ex. dep.", "Saint Helena: Ascension", "Saint Helena: Tristan da Cunha"), "Sub-Saharan Africa", genderdf11$Region)
genderdf11$Region <- ifelse(genderdf11$Country %in% c("Wallis and Futuna Islands"), "East Asia & Pacific", genderdf11$Region)

#install.packages("dplyr")
library(dplyr)
# Nhóm các quốc gia theo khu vực và tính số lượng quốc gia trong mỗi khu vực
grouped_genderdf11 <- genderdf11 %>%
  group_by(Region) %>%
  summarize(Num_Countries = n_distinct(`Country.or.Area`))

print(grouped_genderdf11)
merged_genderdf <- left_join(genderdf11, grouped_genderdf11, by = "Region")
merged_genderdf <- merged_genderdf %>%
  mutate(Gender = as.numeric(gsub(",", "", Gender)))
merged_genderdf <- merged_genderdf %>%
  rename(Country = Country.or.Area)
dfgen2 <- merged_genderdf[, c("Country", "Year","Sex", "Gender")]
dfgen <- merged_genderdf[merged_genderdf$Sex == "Female", c("Country", "Year", "Gender")]

chart_gen <- merged_genderdf[merged_genderdf$Sex == "Female", c("Country", "Year", "Gender","Region")]



