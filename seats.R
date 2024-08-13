#install.packages('httr')
#install.packages("jsonlite")
#install.packages("countrycode")
#install.packages("dplyr")
#install.packages("tidyverse")
library(tidyverse)
library(dplyr)
library(countrycode)
library(httr)
library(jsonlite)
library(jsonlite)

library(httr)
library(jsonlite)

response <- GET("http://api.data.ipu.org/v1/chambers?page[size]=1000000&page[number]=1&fields=current_women_percent%2Ccountry_code%2Cparliament%2Cparliament_country&filter=struct_parl_status.term%3Aeq%3Alower_chamber")

status_code <- status_code(response)
if (status_code == 200) {
  # Lấy dữ liệu từ phản hồi
  data <- content(response, as = "text")
  # Xử lý dữ liệu theo nhu cầu của bạn
  parsed_data <- fromJSON(data)
  print(parsed_data)
} else {
  # Xử lý khi yêu cầu không thành công
  print("Yêu cầu không thành công. Mã trạng thái:", status_code)
}

data <- fromJSON(content(response, "text"))

current_women_percent <- data$data$attributes$current_women_percent

list_of_data <- current_women_percent

list_of_data1 <- list_of_data

countries <- c(
  "Afghanistan", "Albania", "Algeria", "Andorra", "Angola", "Antigua and Barbuda",
  "Argentina", "Armenia", "Australia", "Austria", "Azerbaijan", "Bahamas", "Bahrain",
  "Bangladesh", "Barbados", "Belarus", "Belgium", "Belize", "Benin", "Bhutan",
  "Bolivia", "Bosnia and Herzegovina", "Botswana", "Brazil", "Brunei Darussalam",
  "Bulgaria", "Burkina Faso", "Burundi", "Cabo Verde", "Cambodia", "Cameroon",
  "Canada", "Central African Republic", "Chad", "Chile", "China", "Colombia",
  "Comoros", "Congo", "Costa Rica", "Côte d'Ivoire", "Croatia", "Cuba", "Cyprus",
  "Czech Republic", "Democratic People's Republic of Korea",
  "Democratic Republic of the Congo", "Denmark", "Djibouti", "Dominica",
  "Dominican Republic", "Ecuador", "Egypt", "El Salvador", "Equatorial Guinea",
  "Eritrea", "Estonia", "Eswatini", "Ethiopia", "Fiji", "Finland", "France",
  "Gabon", "Gambia", "Georgia", "Germany", "Ghana", "Greece", "Grenada",
  "Guatemala", "Guinea", "Guinea-Bissau", "Guyana", "Haiti", "Honduras",
  "Hungary", "Iceland", "India", "Indonesia", "Iran", "Iraq", "Ireland",
  "Israel", "Italy", "Jamaica", "Japan", "Jordan", "Kazakhstan", "Kenya",
  "Kiribati", "Kuwait", "Kyrgyzstan", "Lao People's Democratic Republic",
  "Latvia", "Lebanon", "Lesotho", "Liberia", "Libya", "Liechtenstein", "Lithuania",
  "Luxembourg", "Madagascar", "Malawi", "Malaysia", "Maldives", "Mali", "Malta",
  "Marshall Islands", "Mauritania", "Mauritius", "Mexico", "Micronesia",
  "Monaco", "Mongolia", "Montenegro", "Morocco", "Mozambique", "Myanmar",
  "Namibia", "Nauru", "Nepal", "Netherlands", "New Zealand", "Nicaragua", "Niger",
  "Nigeria", "North Macedonia", "Norway", "Oman", "Pakistan", "Palau", "Panama",
  "Papua New Guinea", "Paraguay", "Peru", "Philippines", "Poland", "Portugal",
  "Qatar", "Republic of Korea", "Republic of Moldova", "Romania", "Russian Federation",
  "Rwanda", "Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent and the Grenadines",
  "Samoa", "San Marino", "Sao Tome and Principe", "Saudi Arabia", "Senegal", "Serbia",
  "Seychelles", "Sierra Leone", "Singapore", "Slovakia", "Slovenia", "Solomon Islands",
  "Somalia", "South Africa", "South Sudan", "Spain", "Sri Lanka", "Sudan", "Suriname",
  "Sweden", "Switzerland", "Syrian Arab Republic", "Tajikistan", "Thailand",
  "Timor-Leste", "Togo", "Tonga", "Trinidad and Tobago", "Tunisia", "Turkey",
  "Turkmenistan", "Tuvalu", "Uganda", "Ukraine", "United Arab Emirates",
  "United Kingdom", "United Republic of Tanzania", "United States",
  "Uruguay", "Uzbekistan", "Vanuatu", "Venezuela", "Viet Nam",
  "Yemen", "Zambia", "Zimbabwe"
)

list_of_data1 <- lapply(1:length(list_of_data1), function(i) {
  list_of_data1[[i]]$missing_reason <- countries[i]
  return(list_of_data1[[i]])
})

#install.packages("tidyverse")
library(tidyverse)
final_df <- data.frame()  # Khởi tạo dataframe rỗng

for (i in 1:193) {
  df <- list_of_data1[[i]]
  df <- subset(df, date_valid_until > as.Date('2000-01-01'))
  library(tidyr)
  
  df <- fill(df, .direction = "down")
  df <- df[, -c(4, 6)]
  df$date_from <- as.Date(df$date_from)
  df$date_to <- as.Date(df$date_to)
  
  # Chuyển đổi date_from và date_to sang dạng năm
  df1 <- df %>%
    mutate(date_from = as.integer(format(date_from, "%Y")),
           date_to = as.integer(format(date_to, "%Y")))
  filtered_df <- df1 %>%
    filter(date_to == date_from)
  
  filtered_dfa <- filtered_df
  
  filtered_df <- df1 %>%
    filter(date_to == date_from) %>%
    group_by(date_to) %>%
    summarize(avg_value = mean(value))
  
  filtered_df <- filtered_df %>%
    rename(date = date_to)
  
  filtered_df <- filtered_df %>%
    rename(value = avg_value)
  
  filtered_df <- left_join(filtered_df, filtered_dfa %>% select(date_to, missing_reason), by = c("date" = "date_to"))
  filtered_df <- filtered_df %>%
    distinct() %>%
    filter(!duplicated(.))
  
  filtered_df$date <- as.integer(filtered_df$date)
  filtered_df1 <- df1 %>%
    filter(date_from != date_to)
  
  filtered_df2 <- filtered_df1 %>%
    filter(date_to - date_from >= 2)
  new_df <- filtered_df2 %>%
    mutate(year = date_to - date_from + 1) %>%
    group_by(value, year) %>%
    summarise(date = list(seq(from = date_from[1], to = date_to[1], by = 1))) %>%
    tidyr::unnest(cols = c(date)) %>%
    select(-year)
  new_df <- left_join(new_df, filtered_df2 %>% select(date_to, missing_reason), by = c("date" = "date_to"))
  unique_date_to <- unique(filtered_df$date)
  unique_date <- unique(new_df$date)
  
  new_dates <- setdiff(unique_date, unique_date_to)
  
  new_dates_df <- filter(new_df, date %in% new_dates)
  merged_df <- rbind( filtered_df, new_dates_df)
  merged_df <- subset(merged_df, date >= as.integer('2000'))
  merged_df <- merged_df %>% arrange(date)
  merged_df <- merged_df %>% replace_na(list(missing_reason = unique(merged_df$missing_reason[!is.na(merged_df$missing_reason)])))
  final_df <- rbind(final_df, merged_df)
}




final_df <- data.frame(final_df)
final_df <- distinct(final_df, date, missing_reason, .keep_all = TRUE)
final_df

final_df <- rename(final_df, country = missing_reason)

seatdf <- final_df

column_names <- names(seatdf)
print(column_names)

for (col_name in column_names) {
  num_null <- sum(is.null(seatdf[[col_name]]))
  num_na <- sum(is.na(seatdf[[col_name]]))
  
  print(paste("Column:", col_name))
  print(paste("Number of null:", num_null))
  print(paste("Number of NA:", num_na))
  print("")
}
seatdf1 <- data.frame(seatdf)
seatdf1$Region <- countrycode(seatdf1$country, "country.name", "region")
# Handle specific countries
seatdf1$Region <- ifelse(seatdf1$country %in% c("Micronesia"), "East Asia & Pacific", seatdf1$Region)
seatdf1$Region <- ifelse(seatdf1$country %in% c("Réunion"), "Some Region", seatdf1$Region)

# Group countries by region and calculate the number of unique countries in each region
grouped_seatdf1 <- seatdf1 %>%
  group_by(Region) %>%
  summarize(Num_Countries = n_distinct(country))

merged_seatdf <- left_join(seatdf1, grouped_seatdf1, by = "Region")

# Hiển thị bảng kết quả
merged_seatdf


merged_seatdf <- merged_seatdf %>%
  rename(Country = country, Year = date, Seat = value)
summary(merged_seatdf)

filter_2022 <- merged_seatdf %>% filter(Year == 2022)
seat_sum_by_region <- filter_2022 %>% group_by(Region) %>% summarize(Total_Seat = sum(Seat))


ggplot(seat_sum_by_region, aes(x = reorder(Region, -Total_Seat), y = Total_Seat)) +
  geom_bar(stat = "identity", fill = "aquamarine3") +
  geom_text(aes(label = round(Total_Seat, 2)), vjust = -0.5, color = "#FF6600") +
  labs(title = "Tổng số ghế theo châu lục vào năm 2022", x = "Châu lục", y = "Tổng số ghế") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(dplyr)

sum_by_continent <- merged_seatdf %>%
  group_by(Year, Region) %>%
  summarize(Total_Seat = sum(Seat))
  
ggplot(sum_by_continent, aes(x = Year, y = Total_Seat, group = Region)) +
  geom_line(aes(color = Region), size = 1) +
  geom_point(aes(color = Region), size = 2) +
  labs(title = "Tổng số ghế theo châu lục giai đoạn 2000 - 2020 ", x = "Châu lục", y = "Tổng số ghế") +
  theme(legend.position = "top")

 abc <- merged_seatdf %>%
  filter(Region == "Sub-Saharan Africa") %>%
  group_by(Year, Country) %>%
  summarize(Total_Seat = sum(Seat)) %>%
  group_by(Country)




dfs <- merged_seatdf[, c("Country", "Year", "Seat")]

chart_seat <- merged_seatdf[, c("Country", "Year", "Seat",'Region')]

chartdfa <- merge(chart_seat, chart_gen, by = c("Country", "Year", "Region"))
chartdfa <- merge(chartdfa, chart_gdp, by = c("Country", "Year", "Region"))

ratedf <- merge(merge(chart_seat, dfgdp, by = c("Country", "Year"), all = TRUE), 
                dfgen, by = c("Country", "Year"), all = TRUE)

#ratedf <- na.omit(ratedf)
top_country_2000 <- ratedf %>%
  filter(Year == 2000) %>%
  arrange(desc(Seat)) %>%
  slice(1)
top_country_2022 <- ratedf %>%
  filter(Year == 2022) %>%
  arrange(desc(Seat)) %>%
  slice(1)
top_country_gdp_2000 <- ratedf %>%
  filter(Year == 2000) %>%
  arrange(desc(GDP)) %>%
  slice(1)
top_country_gdp_2022 <- ratedf %>%
  filter(Year == 2022) %>%
  arrange(desc(GDP)) %>%
  slice(1)
ratedf %>%
  filter(Year == 2000 | Year == 2022) %>%
  mutate(pop_m = Gender / 1e6) %>%
  ggplot(aes(x = GDP, y = Seat)) +
  geom_point(aes(size = pop_m, color = Region), alpha = 0.5) +
  geom_smooth(method = "auto") +
  scale_x_continuous(labels = scales::comma) +
  labs(title = 'Tỷ lệ việc làm của Nữ và GDP của các quốc gia năm 2000 và 2022') +
  labs(x = "GDP ($/Năm)", y = "Tỷ lệ việc làm của Nữ (%)") +
  labs(color = "Khu vực", size = "Dân số (Triệu người)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_grid(Year~.) +
  geom_text_repel(data = top_country_2000, aes(label = Country), nudge_x = 5000, nudge_y = -5) +
  geom_text_repel(data = top_country_2022, aes(label = Country), nudge_x = 5000, nudge_y = -5) +
  geom_text_repel(data = top_country_gdp_2000, aes(label = Country), nudge_x = 5000, nudge_y = -5) +
  geom_text_repel(data = top_country_gdp_2022, aes(label = Country), nudge_x = 5000, nudge_y = -5)


# Tải dữ liệu bản đồ

world_map <- map_data("world")

# Kết hợp dữ liệu thu nhập và bản đồ thế giới
mapseat <- subset(merged_seatdf, Year =="2020")

library(plotly)

p <- plot_ly(mapseat, z = ~Seat, text = ~paste("Country: ", Country, "<br>Seat: ", Seat),
             locations = ~Country, type = "choropleth", locationmode = "country names") 

p <- add_annotations(p, x = 0.5, y = -0.1, text = "World Map", showarrow = FALSE, 
                     font = list(size = 16), xref = "paper", yref = "paper")

p
