#Thư viện
library(reshape2)
library(ggplot2)

#Load data

model <- read.csv('clean.csv', encoding = "UTF-8")

model <- stats[stats$Year == 2018,]

model <- model[, -c(1, 2, 7 , 9, 10, 11, 12, 13, 14, 15)]

quantile(model$Installs, probs = seq(0, 1, 1 / 20))

max_threshold <- quantile(model$Installs, 0.95)

min_threshold <- quantile(model$Installs, 0.1)

model <-
  model[model$Installs > min_threshold &
          model$Installs < max_threshold, ]

model$Reviews <- model$Reviews/1000000
model$Installs <- model$Installs/1000000



corr <- round(cor(model, method = "pearson"), digits = 2)

melt <- melt(corr)

corr

img7 <- ggplot(data = melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "#438295",
    high = "#C3553A",
    mid = "#F2F2F2",
    midpoint = 0.4,
    limit = c(-0.2, 1),
    space = "Lab",
    name = "Tương quan Pearson"
  ) +
  geom_text(aes(Var2, Var1, label = value),
            color = "black",
            size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
  ) +
  guides(fill = guide_colorbar(barwidth = 2, barheight = 40)) +
  ggtitle("Bảng so sánh sự tương quan giữa các giá trị")

img7


#Review ~ Installs


corr1 <- cor.test(model$Reviews, model$Installs, method = "pearson")
corr1

linearModel <- lm(Reviews ~ Installs , data = model)
linearModel$

fitted(linearModel)
resid(linearModel)
datten <- par(mfrow = c(2, 2))
plot(model$Reviews ~ model$Installs, pch = 16, xlim=c(0,15), ylim=c(0,10))
abline(linearModel)
summary(linearModel)
pair(model)



write.csv(model, "model.csv", row.names = FALSE)
 
