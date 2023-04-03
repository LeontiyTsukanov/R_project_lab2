library(openxlsx)
library(ggplot2)
library(randomForest)
library(xlsx)

data <- read.xlsx(xlsxFile = "Вар_54_Полные_данные.xlsx")

x <- names(data)
del_comma <- function(x){
  x <- gsub(',','',x)
}
y <- sapply(x, del_comma)
colnames(data) <- y
colnames(data)[3] <-"Среднегодовой.доход.тыс"
colnames(data)[7] <-"Доля.от.дохода.семьи.которая.тратится.на.продовольствие"
colnames(data)[9] <- "Издержки.сообщества.на.окружающую.среду.млн."
colnames(data)[10] <- "Охват.беспроводной.связи.в.сообществе"

no_data <- subset(data, data$Ощущаемое.счастье == 'Неизвестно')
learn_data <- subset(data, data$Ощущаемое.счастье != 'Неизвестно')

train_data <- learn_data[1:25000,]
test_data <- learn_data[25001:30000,]

train_short_data <- train_data[,3:12]
train_short_data$Ощущаемое.счастье <- train_data$Ощущаемое.счастье
train_short_data$Ощущаемое.счастье = as.factor(train_short_data$Ощущаемое.счастье)
train_short_data <- train_short_data[1:2000,]

data$Ощущаемое.счастье = as.factor(data$Ощущаемое.счастье)

aim <- "Ощущаемое.счастье"
factors <- names(train_data[,3:12])
formula_text <- paste0(aim, " ~ ", paste0(factors, collapse = " + "))
print(formula_text)

model1 <- randomForest(Ощущаемое.счастье ~ . , data = train_short_data, ntrees = 500, proximity = TRUE)


prognoz <- predict(model1, newdata = test_data)
test_data$result = prognoz


ccc <- 0
for (i in 1:nrow(test_data)){
  if (test_data[i,26] == test_data[i,27]){
    ccc <- ccc + 1
  }
}
res <- ccc/nrow(test_data)

which.min(model1$mse)
pred2 <- predict(model1, newdata = no_data)
no_data$Ощущаемое.счастье <- pred2

write.csv(no_data, "itog.xlsx")

