#Thư viện
install.packages("dplyr")
install.packages("stringr")
library(dplyr)
library(stringr)

#P1
#Đọc dữ dataset
data <- read.csv('googleplaystore.csv', encoding = "UTF-8")

#Số hàng
nrow(data)

#Số cột
ncol(data)

#P2
#Hiển thị cột
names(data)

#P3
#Thay giá trị "" bằng NA
data[data == ""] <- NA

#Xóa các hàng trùng nhau
data <- unique(data)

#Xóa các hàng trùng giá trị ở một số cột
data <-
  distinct(data,
           Name,
           Category,
           Current.Ver,
           Last.Updated,
           Android.Ver,
           .keep_all = T)

#Thay thế các giá trị bị thiếu

#Tìm số giá trị NA trong mỗi cột
colSums(is.na(data))

#Thay các giá trị NA trong cột Rating 
data[is.na(data$Rating), ]$Rating <- mean(data$Rating, na.rm = T)

#Kiểm tra các giá trị trong cột Current.Ver
unique(data$Current.Ver)

#Thay thế giá trị NA trong cột Current.Ver
data[is.na(data$Current.Ver),]$Current.Ver <-
  "Varies with device"

#Kiểm tra và thay đổi kiểu dữ liệu các cột

#Kiểm tra kiểu dữ liệu các cột
sapply(data, class)

#Check các giá trị trong cột Size
unique(data$Size)

data[data$Size == "Varies with device", ]$Size <- "-1"
#mean(data$Size, na.rm = T)

data[endsWith(data$Size, "M"), ]$Size <-
  substring(data[endsWith(data$Size, "M"), ]$Size , 1, nchar(data[endsWith(data$Size, "M"), ]$Size) -
              1)

data[endsWith(data$Size, "k"), ]$Size <-
  as.character(as.numeric(substring(data[endsWith(data$Size, "k"), ]$Size , 1, nchar(data[endsWith(data$Size, "k"), ]$Size) -
              1))/1024)

data$Size <- as.numeric(data$Size)

tmp <- data[data$Size > 0,]

data[data$Size == -1, ]$Size <- mean(tmp$Size)

data$Size <- round(data$Size, digits = 2)

#Check các giá trị trong cột Rating (1-5)
sort(unique(data$Rating))

#Check các giá trị trong cột Installs
unique(data$Installs)
# Loại bỏ dấu + và , trong mỗi hàng của cột Installs
data$Installs <- gsub('[+,]', '', data$Installs)
#Đổi kiểu dữ liệu cột Install sang Integer
data$Installs <- as.integer(data$Installs)

#Lấy các giá trị có Installs cao hơn hoặc bằng Reviews
data <- data[data$Installs >= data$Reviews,]

#Check các giá trị trong cột Price
unique(data$Price)
# Loại bỏ dấu $ trong mỗi hàng của cột Price
data$Price <- gsub('[$]', '', data$Price)
#Đổi kiểu dữ liệu cột Price sang Numeric
data$Price <- as.numeric(data$Price)

#Thay thế các giá trị có Free mà Price > 0 và ngược lại
data[data$Price > 0,]$Type <- "Paid"

data[data$Price == 0,]$Type <- "Free"

















#Không sử dụng tới

#4

#Tạo cột Revenue dựa trên cột Installs và cột Price
data$Revenue <- round(data$Price * data$Installs, digits = 2)

#Tạo cột Best.Sale dựa trên cột Revenue
data$Best.Sale <- ifelse(data$Revenue >= 1000000, T, F)

#Tạo riêng lẻ các cột Ngày, Tháng, Năm cập nhật




#5

#Có bao nhiêu thể loại App, trung bình mỗi thể loại có Rating bao nhiêu và đâu là thể loại có Rating cao nhất?

#Tạo nhóm các Category, mỗi Category tính giá trị trung bình của Rating
group1 <- aggregate(data$Rating, list(data$Category), mean)
#Sắp xếp nhóm theo chiều giảm dần
group1 <- group1[order(group1$x, decreasing = T), ]
#Đổi tên cột của nhóm
colnames(group1) <- c("Category", "Rating")
#Xem nhóm
group1

#Có sự khác biệt đáng kể về Rating của app bị hạn chế và app không bị hạn chế không?

#Tạo một biến khác để xử lý data
data2 <- data
#Thay các giá trị Content.Rating không phải Everyone thành Restricted
data2[data2$Content.Rating != "Everyone",]$Content.Rating <-
  "Restricted"
#Tạo nhóm các Content.Rating, mỗi Content.Rating tính giá trị trung bình của Rating
group2 <- aggregate(data2$Rating, list(data2$Content.Rating), mean)
#Đổi tên cột của nhóm
colnames(group2) <- c("Content", "Rating")
#Xem nhóm
group2

#Có sự khác biệt đáng kể về Rating của app trả phí và app miễn phí không?

#Tạo nhóm các Type, mỗi Type tính giá trị trung bình của Rating
group3 <- aggregate(data$Rating, list(data$Type), mean)
#Đổi tên cột của nhóm
colnames(group3) <- c("Type", "Rating")
#Xem nhóm
group3

#Lưu file clean
write.csv(data, "clean.csv", row.names = FALSE)
