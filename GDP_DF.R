library(rvest)
#install.packages("countrycode")
library(countrycode)
#install.packages("dplyr")
library(dplyr)
base_url<- "https://data.un.org/Data.aspx?d=SNAAMA&f=grID%3a101%3bcurrID%3aUSD%3bpcFlag%3afalse%3byr%3a2000%2c2001%2c2002%2c2003%2c2004%2c2005%2c2006%2c2007%2c2008%2c2009%2c2010%2c2011%2c2012%2c2013%2c2014%2c2015%2c2016%2c2017%2c2018%2c2019%2c2020%2c2021%2c2022%3bitID%3a9&c=2,3,5,6&s=_crEngNameOrderBy:asc,yr:desc&v="

data_part_3 <- data.frame()  # Khởi tạo data frame rỗng

for (i in 1:50) {
  url <- paste0(base_url, i)
  page <- read_html(url)
  
  # Trích xuất dữ liệu từ phần tử HTML (điều chỉnh CSS selector theo yêu cầu)
  dataset_df1 <- page %>%
    html_nodes("table") %>%
    html_table(fill = TRUE)
  
  # Gộp dữ liệu vào data frame chính
  if (!is.null(dataset_df1[[2]])) {
    data_part_3 <- rbind(data_part_3, dataset_df1[[2]])
  }
}

for (i in 51:98) {
  url <- paste0(base_url, i)
  page <- read_html(url)
  
  # Trích xuất dữ liệu từ phần tử HTML (điều chỉnh CSS selector theo yêu cầu)
  dataset_df1 <- page %>%
    html_nodes("table") %>%
    html_table(fill = TRUE)
  
  # Gộp dữ liệu vào data frame chính
  if (!is.null(dataset_df1[[2]])) {
    data_part_3 <- rbind(data_part_3, dataset_df1[[2]])
  }
}
gpddf <- data_part_3
#Xử lí gdpdf
gpddf <- gpddf[, -c(3)]
colnames(gpddf)[colnames(gpddf) == "Value"] <- "GDP"
head(gpddf,5)

gdpdf1 <- data.frame(gpddf)
#Kiểm tra liệu có giá trị null hay na
column_names <- names(gdpdf1)
print(column_names)

for (col_name in column_names) {
  num_null <- sum(is.null(gdpdf1[[col_name]]))
  num_na <- sum(is.na(gdpdf1[[col_name]]))
  
  print(paste("Column:", col_name))
  print(paste("Number of null:", num_null))
  print(paste("Number of NA:", num_na))
  print("")
}

gdpdf1$Country <- gdpdf1$Country
# Chuyển đổi mã quốc gia thành khu vực
gdpdf11 <- data.frame(gdpdf1)
gdpdf11$Region <- countrycode(gdpdf11$Country, "country.name", "region")
gdpdf11$Region <- ifelse(gdpdf11$Country %in% c("Mayotte", "Reunion"), "Sub-Saharan Africa", gdpdf11$Region)
gdpdf11$Region <- ifelse(gdpdf11$Country %in% c("Saint Helena ex. dep.", "United Republic of Tanzania: Zanzibar", "Saint Helena: Tristan da Cunha"), "Sub-Saharan Africa", gdpdf11$Region)
gdpdf11$Region <- ifelse(gdpdf11$Country %in% c("Wallis and Futuna Islands"), "East Asia & Pacific", gdpdf11$Region)
# Nhóm các quốc gia theo khu vực và tính số lượng quốc gia trong mỗi khu vực
grouped_gdpdf11 <- gdpdf11 %>%
  group_by(Region) %>%
  summarize(Num_Countries = n_distinct(Country))

print(grouped_gdpdf11)
merged_gdpdf <- left_join(gdpdf11, grouped_gdpdf11, by = "Region")

# Hiển thị bảng kết quả
merged_gdpdf
merged_gdpdf <- merged_gdpdf %>%
  mutate(GDP = gsub(",", "", GDP)) %>%
  mutate(GDP = as.numeric(GDP))
gdp_df <- merged_gdpdf[, c("Country", "Year", "GDP")]
dfgdp <- gdp_df

chart_gdp <- merged_gdpdf[, c("Country", "Year", "GDP",'Region')]
