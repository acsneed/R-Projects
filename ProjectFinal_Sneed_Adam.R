#1.)
pmsm_temperature_data <- read.csv("~/Desktop/R/Final Project/pmsm_temperature_data.csv")
pmsm_temperature_data <- na.omit(pmsm_temperature_data)

View(pmsm_temperature_data)

#2.)
pmsm_temperature_data$stator_yoke <- as.factor(pmsm_temperature_data$stator_yoke)

#3.)
summary(pmsm_temperature_data$pm)
sd(pmsm_temperature_data$pm)

#4.)
summary(pmsm_temperature_data$motor_speed)
sd(pmsm_temperature_data$motor_speed)

#5.)
correlation <- cor(pmsm_temperature_data$motor_speed, pmsm_temperature_data$pm)
print(correlation)

#6.)
yoke_table <- table(pmsm_temperature_data$stator_yoke)
yoke_table

sorted_yoke <- sort(yoke_table, decreasing = TRUE)[1]
mode_yoke <- sorted_yoke[1]
mode_yoke

#7.)
library(ggplot2)

ggplot(data = pmsm_temperature_data, aes(x=pm))+ 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="pink") + geom_vline(aes(xintercept=mean(pm)),
            color="green", linetype="dashed", size=1)

#8.)
ggplot(data = pmsm_temperature_data, aes(x = motor_speed, y=pm))+
  geom_point()+geom_smooth()

#9.)
ggplot(data=pmsm_temperature_data, aes(x=stator_yoke, y=pm)) + 
  geom_boxplot(aes(col= stator_yoke ), notch = TRUE)
ggsave("~/Desktop/R/Final Project/pmyoke.jpg", width = 20, height = 15, units = "cm")

#10.)
#a
lm.result1 <- lm(pm ~ ambient + coolant + motor_speed + torque, data= pmsm_temperature_data)
summary(lm.result1)$coefficients

#b
lm.result2 <- lm(pm ~ ambient + coolant + u_d + motor_speed + torque + stator_winding, data= pmsm_temperature_data)
summary(lm.result2)$coefficients

#c
lm.result3 <- lm(pm ~ ambient + coolant + u_d + u_q + motor_speed + torque + stator_yoke + stator_winding, data= pmsm_temperature_data)
summary(lm.result3)$coefficients

#d
summary(lm.result1)$adj.r.squared
summary(lm.result2)$adj.r.squared
summary(lm.result3)$adj.r.squared

#11.)
library(class)
pmsm_temperature_data$stator_yoke <- as.factor(pmsm_temperature_data$stator_yoke)

predictors <- c("pm", "ambient", "coolant", "motor_speed", "torque", "u_d", "u_q")
data.predictors <- pmsm_temperature_data[predictors]
data.target <- pmsm_temperature_data$stator_yoke

#a
sample.size <- floor(0.85*nrow(pmsm_temperature_data))

train <- data.predictors[1:sample.size, ]
test <- data.predictors[-c(1:sample.size), ]

#b
cl <- data.target[1:sample.size]
knn.test.predict1 <-knn(train[1:3], test[1:3], cl, k=5)

test.label1 <- data.target[-c(1:sample.size)]
table(test.label1, knn.test.predict1)

#c
knn.test.predict2 <-knn(train[1:5], test[1:5], cl, k=5)

test.label2 <- data.target[-c(1:sample.size)]
table(test.label2, knn.test.predict2)

#d
knn.test.predict3 <-knn(train, test, cl, k=5)

test.label3 <- data.target[-c(1:sample.size)]
table(test.label3, knn.test.predict3)






