
#-------------------------------
#   Section 1: Basic Mapping
#-------------------------------


rm(list = ls())
library(tidyverse)
library(magrittr)

# Lấy dữ liệu địa lí của Hoa Kì: 
usa <- map_data("usa")

# Xem qua dữ liệu: 
usa %>% head()

# Bản đồ đơn giản: 
ggplot() + 
  geom_polygon(data = usa, aes(x = long, y = lat, group = group)) + 
  labs(title = "Map 1: Default")

ggplot() + 
  geom_polygon(data = usa, aes(long, lat, group = group)) + 
  coord_fixed(1.3) + 
  labs(title = "Map 2: Y unit was 1.3 times longer than an x unit")


#----------------------------
#    Mapping for Vietnam
#----------------------------

# Lấy dữ liệu địa lí cho VN đến cấp tỉnh: 
library(raster)

# Và dữ liệu địa lí của VN đến cấp tỉnh: 
vietnam_province <- getData("GADM", country = "Vietnam", level = 1)

# Gỡ bỏ sử dụng raster: 
detach(package:raster)

# Lưu ý định dạng dữ liệu: 
vietnam_province %>% class()

# Chuyển hóa về data frame quen thuộc: 
vietnam_df <- vietnam_province %>% fortify(region = "NAME_1")

# Xem qua dữ liệu: 
vietnam_df %>% head()

# Các tỉnh thành của Việt Nam: 
vn_prov <- vietnam_df$id %>% unique()
vn_prov

# Bản đồ VN: 
ggplot() + 
  geom_polygon(data = vietnam_df, aes(long, lat, group = group)) -> m1

m1

# Hiệu chỉnh: 
m1 + coord_fixed(1)
m1 + coord_equal()
m1 + coord_map("albers", lat0 = 30, lat1 = 40)
m1 + coord_map("gilbert")

# Hiệu chỉnh: 
ggplot() + 
  geom_polygon(data = vietnam_df, 
               aes(long, lat, group = group), 
               fill = "red", color = "blue") + 
  coord_equal()

# Giả sử chúng ta muốn biểu diễn vị trí của Hà Nội
# (tra thêm từ https://www.google.com): 

ha_noi <- data.frame(lat = 21.040002, long = 105.834388)

m1 + 
  coord_map("gilbert") + 
  geom_point(data = ha_noi, aes(long, lat), color = "red", size = 4)


# Cách vẽ khác: 

ggplot() + 
  geom_polygon(data = vietnam_df, 
               aes(long, lat, group = group), 
               fill = NA, color = "red") + 
  coord_map("albers", lat0 = 30, lat1 = 40) -> m2

m2

# Bản đồ cụ thể đến cấp tỉnh: 
ggplot() + 
  geom_polygon(data = vietnam_df, 
               aes(long, lat, group = group, fill = id), 
               show.legend = FALSE) + 
  coord_equal()

# Muốn có thêm đường biên giới của các tỉnh (cách 1): 
ggplot() + 
  geom_polygon(data = vietnam_df, 
               aes(long, lat, group = group, fill = id), 
               show.legend = FALSE, color = "white") + 
  coord_equal()

# Hoặc cách 2: 

ggplot() + 
  geom_polygon(data = vietnam_df, 
               aes(long, lat, group = group, fill = id), 
               show.legend = FALSE) + 
  geom_path(data = vietnam_df, aes(long, lat, group = group), 
            color = "white", size = 0.1) +  
  coord_equal()

#------------------------------------------
#  Advanced 1: Poverty Rate by Province
#------------------------------------------

# Tạo một bộ dữ liệu về  tỉ lệ hộ nghèo (tính trên 1000)
# hộ dân cho các tỉnh: 

set.seed(1)
poverty <- data.frame(id = vn_prov, 
                      poverty = runif(length(vn_prov), 0, 200) %>% round(0))

# Hợp nhất dữ liệu: 
vietnam_df <- right_join(vietnam_df, poverty, by = "id")

# Hình ảnh hóa tỉ lệ đói nghèo này theo tình: 
ggplot() + 
  geom_polygon(data = vietnam_df, 
               aes(long, lat, group = group, fill = poverty), 
               color = "white", size = 0.01) + 
  coord_equal() -> m3

m3

# Cải tiến: 

m4 <- m3 + 
  scale_fill_gradient(name = "Poverty", limits = c(0, 200), low = "white", high = "red")

m4

# Màu sắc kiểu này không được đẹp và nên thay đổi: 
library(viridis)
m3 + scale_fill_viridis(direction = -1, option = "D", "Poverty")
m3 + scale_fill_viridis(direction = -1, option = "A", "Poverty")
m3 + scale_fill_viridis(direction = -1, option = "B", "Poverty")
m3 + scale_fill_viridis(direction = -1, option = "C", "Poverty")

#----------------------------------------------------
#  Advanced 2: Poverty Rate by Province + District
#----------------------------------------------------

# Lấy dữ liệu địa lí các tỉnh cho đến cấp huyện: 
library(raster)
vietnam_dis <- getData("GADM", country = "Vietnam", level = 2)
detach(package:raster)

vietnam_df_district <- vietnam_dis %>% fortify(region = "NAME_2")

# Các huyện: 
district_name <- vietnam_df_district$id %>% unique()

# Mô phỏng số liệu về hộ nghèo của các huyện: 

set.seed(1)
poverty_dis <- data.frame(id = district_name, 
                          poverty = runif(length(district_name), 0, 200) %>% round(0))

# Hợp nhất các dữ liệu: 

vietnam_df_district <- right_join(vietnam_df_district, poverty_dis, by = "id")

# Vẽ phác thảo: 

ggplot() + 
  geom_polygon(data = vietnam_df_district, 
               aes(long, lat, group = group, fill = poverty)) + 
  coord_equal() -> m5

m5

# Vẽ cho đẹp hơn: 

m5 + scale_fill_viridis(option = "D", direction = -1, "Poverty") -> m6 
m6

# Hiệu chỉnh nữa cho đẹp. Trước hết tạo một theme riêng: 

theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(color = "#22211d", face = "bold"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#f5f5f2", color = NA), 
      panel.background = element_rect(fill = "#f5f5f2", color = NA), 
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.border = element_blank()
    )
}

m7 <- m6 + theme_map()
m7

# Thêm các tiêu đề các loại cho đầy đủ: 

m7 + labs(title = "Poverty Rate in Vietnam by District", 
          subtitle = "Note: Poverty Data Is Not Available for\nVietnam's Paracel and Spratly Islands", 
          caption = "Data Source: General Statistics Office Of Vietnam") -> m8

m8

# Có thể hiệu chỉnh vị trí của legend: 
m8 + 
  theme(legend.position = c(0.2, 0.4)) -> m9

m9

# Thể hiện thêm đường bao cho các tỉnh: 
m9 + 
  geom_path(data = vietnam_df, aes(long, lat, group = group), 
            color = "white", size = 0.1) 

# Hoặc bằng màu đỏ: 
m9 + 
  geom_path(data = vietnam_df, aes(long, lat, group = group), 
            color = "red", size = 0.1, alpha = 0.3) 

#---------------------------------------------------------
#  Advanced 3: Population Density by Commune for Province
#---------------------------------------------------------

# Lấy dữ liệu địa lí của VN đến cấp xã: 
library(raster)
vietnam_com <- getData("GADM", country = "Vietnam", level = 3)

# Không sử dụng package này nữa: 
detach(package:raster)

# Tách riêng tỉnh Nghệ An: 
na <- vietnam_com[vietnam_com$NAME_1 == "Nghệ An", ]

# Chuyển hóa về DF quen thuộc: 
na_df1 <- na %>% fortify(region = "NAME_3")

# Các xã của Nghệ An: 
na_com <- na_df1$id %>% unique()

# Tạo một bộ dữ liệu giả về mật độ dân số
# (nghìn người trên một km2) cho các xã của Nghệ An: 
set.seed(1)
mat_do <- data.frame(id = na_com, 
                     pop_den = rnorm(na_com %>% length(), 30, 3000) %>% abs %>% round(0))

# Joint các dữ liệu: 
na_df1_den <- right_join(na_df1, mat_do, by = "id")

p <- ggplot() +
  geom_polygon(data = na_df1_den, 
               aes(fill = pop_den, x = long, y = lat, group = group), color = "white") +
  coord_equal() +
  theme_map() +
  labs(x = NULL, y = NULL, 
       title = "Nghe An's Regional Demographics", 
       subtitle = "Population Density by Commune, 2016", 
       caption = "Data Source: Nghe An Statistical Authority, 2016")

p

# Kiểu 1: 

p + scale_fill_viridis(direction = -1, option = "D", "Population Density\nby Commune")

# Kiểu 2: 

p + 
  theme_map() + 
  scale_fill_viridis(direction = -1, option = "A", "Population Density\nby Commune") + 
  theme(legend.position = c(0.87, 0.86))






