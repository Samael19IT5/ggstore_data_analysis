library(ggplot2)
library(ggExtra)

stats <- read.csv('clean.csv', encoding = "UTF-8")

#img1

s1 <- aggregate(stats$Category, by = list(stats$Category), length)
s1$x <- round(s1$x / nrow(stats) * 100, digits = 1)
s1 <- s1[order(s1$x, decreasing = T), ]
tmp <- s1[s1$x <= 2.3,]
s1 <- s1[s1$x > 2.3,]
s1 <- rbind(s1, data.frame(Group.1 = "OTHER", x = sum(tmp$x)))
cate <- s1$Group.1
catep <- s1$x
cate <- cate[order(catep)]
catep <- sort(catep)
cate.factor <- factor(cate, levels = as.character(cate))
catepy <- cumsum(catep) - 0.5 * catep
catepy <- 100 - catepy

img1 <- ggplot() + theme_bw() +
  geom_bar(aes(x = "",
               y = catep,
               fill = cate.factor),
           stat = "identity",
           color = "white") +
  coord_polar("y", start = 0) +
  ggtitle("Các thể loại của ứng dụng") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
  ) +
  geom_text(aes(
    x = 1.6,
    y = catepy,
    label = paste0(catep, "%")
  ), size = 4) +
  guides(fill = guide_legend(title = "Category", reverse = T))

img1


#img2

img2 <- ggplot(stats, aes(x = Rating)) +
  geom_histogram(fill = "blue") +
  theme_bw() +
  ggtitle("Số lượng đánh giá")
img2


#img3

s3 <- aggregate(stats$Rating, list(stats$Category), mean)
s3

img3 <- ggplot(s3, aes(x = reorder(Group.1, x), y = x), ) +
  geom_bar(stat = "identity", fill = "pink") +
  theme(axis.text.x = element_text(
    angle = 90,
    vjust = 0.5,
    hjust = 1
  )) +
  ggtitle("Trung bình điểm đánh giá giữa các thể loại") +
  labs(x = "Category", y = "Rating")
img3

#img4

img4 <- ggplot(stats, aes(x = Size, y = Rating)) +
  geom_point(col = "#0D6EFD") +
  ggtitle("Đánh giá ứng dụng dựa vào kích thước") +
  theme(aspect.ratio = 1)

img4 <- ggMarginal(img4, type = "histogram", fill = "slateblue")

img4

#img5

img5 <- ggplot(stats, aes(x = Price, y = Rating)) +
  geom_point(col = "#0D6EFD") +
  ggtitle("Đánh giá ứng dụng dựa vào chi phí") +
  theme(aspect.ratio = 1)

img5 <- ggMarginal(img5, type = "histogram", fill = "slateblue")

img5

#img6

freeI <- log10(stats[stats$Type == "Free",]$Installs)

paidI <- log10(stats[stats$Type == "Paid",]$Installs)

s6 <- list(freeI, paidI)

names(s6) <-
  c(paste("Free\n n=" , length(freeI) , sep = ""),
    paste("Paid\n n=" , length(paidI) , sep = ""))

par(mgp = c(3, 2, 0))

boxplot(
  s6 ,
  col = c("#69b3a2", "#EA85C5"),
  title = "Số lượt tải xuống của Ứng dung miễn phí vs trả phi",
  ylab = "Số lượt tải xuống(log10)"
)


#eximg

#Năm cập nhật
stats$Year <-
  as.integer(str_split_fixed(stats$Last.Updated, ", ", 2)[, 2])

#Ngày cập nhật
stats$Day <-
  as.integer(str_split_fixed(str_split_fixed(stats$Last.Updated, " ", 2)[, 2], ", ", 2)[, 1])

#Tháng cập nhật
stats$Month <-
  str_split_fixed(str_split_fixed(stats$Last.Updated, " ", 2)[, 1], ", ", 2)[, 1]

free <- stats[stats$Type == "Free", ]
paid <- stats[stats$Type == "Paid", ]

sfree <- aggregate(free$Year, list(free$Year), length)
sfree <- sfree[nrow(sfree):(nrow(sfree) - 4), ]
sfree$Type <- c("Free","Free","Free","Free","Free")


spaid <- aggregate(paid$Year, list(paid$Year), length)
spaid <- spaid[nrow(spaid):(nrow(spaid) - 4), ]
spaid$Type <- c("Paid","Paid","Paid","Paid","Paid")

exs = rbind(spaid, sfree)
exs

ggplot(exs, aes(x = reorder(Group.1, x), y=x, fill=Type)) +
  geom_bar(stat="identity", position="dodge") +
  theme(axis.text.x = element_text(angle = 90))+
  ggtitle("Số lượng ứng dụng cập nhật qua từng năm") +
  labs(x = "Year", y = "Count")


