install.packages("rpivotTable")
library(rpivotTable)

#Задание 1
R <- read.csv("https://raw.githubusercontent.com/junaart/ForStudents/refs/heads/master/R/Lesson_4/HairEyeColor.csv",
              sep = ",")
View(R)

#Задание 2
rpivotTable::rpivotTable(R, row = "Sex", aggregatorName = "Count", vals = "Sex")

#Задание 3
rpivotTable::rpivotTable(R, row = "Hair", aggregatorName = "Count", vals = "Sex")

#Задание 4
four <- subset(R, Eye == "Green")
four <- aggregate(Freq ~ Sex, data = four, sum)
View(four)

#Задание 5
length(R$X)

#Задание 6
install.packages("ggplot2")
library(ggplot2)
ggplot(R, aes(x = Hair, fill = Hair)) +
  geom_bar() +  
  theme_minimal() +
  labs(title = "Распределение по цвету волос",
       x = "Цвет волос",
       y = "Количество людей")

#Модальное значение
hair_count_all <- table(R$Hair)
mode_hair_all <- names(hair_count_all)[which.max(hair_count_all)]
mode_hair_all

#Задание 7
ggplot(R[R$Sex == "Male", ], aes(x = Hair, fill = Hair)) +
  geom_bar(stat = "count") +
  theme_minimal() +
  labs(title = "Распределение мужчин по цвету волос",
       x = "Цвет волос",
       y = "Количество мужчин")

#Модальное значение
hair_count_male <- table(R[R$Sex == "Male", ]$Hair)
mode_hair <- names(hair_count_male)[which.max(hair_count_male)]
mode_hair

#Задание 8
ggplot(R[R$Sex == "Female", ], aes(x = Hair, fill = Hair)) +
  geom_bar(stat = "count") +
  theme_minimal() +
  labs(title = "Распределение женщин по цвету волос",
       x = "Цвет волос",
       y = "Количество женщин")

#Модальное значение 
hair_count_female <- table(R[R$Sex == "Female", ]$Hair)
mode_hair_female <- names(hair_count_female)[which.max(hair_count_female)]
mode_hair_female


#Задание 9
ggplot(R, aes(x = Eye, fill = Eye)) +
  geom_bar(stat = "count") +
  theme_minimal() +
  labs(title = "Распределение людей по цвету глаз",
       x = "Цвет глаз",
       y = "Количество людей")

#Модальное значение 
eye_count <- table(R$Eye)
mode_eye <- names(eye_count)[which.max(eye_count)]
mode_eye

#Задание 10
ggplot(R[R$Sex == "Male", ], aes(x = Eye, fill = Eye)) +
  geom_bar(stat = "count") +
  theme_minimal() +
  labs(title = "Распределение мужчин по цвету глаз",
       x = "Цвет глаз",
       y = "Количество женщин")

#Модальное значение 
eye_count_male <- table(R[R$Sex == "Male", ]$Eye)
mode_eye_male <- names(eye_count_male)[which.max(eye_count_male)]
mode_eye_male

#Задание 11
ggplot(R[R$Sex == "Female", ], aes(x = Eye, fill = Eye)) +
  geom_bar(stat = "count") +
  theme_minimal() +
  labs(title = "Распределение женщин по цвету глаз",
       x = "Цвет глаз",
       y = "Количество женщин")

eye_count_female <- table(R[R$Sex == "Female", ]$Eye)
mode_eye_female <- names(eye_count_female)[which.max(eye_count_female)]
mode_eye_female

#Задание 12
Canada <- read.csv("https://raw.githubusercontent.com/junaart/ForStudents/refs/heads/master/R/Lesson_4/CanPop.csv",
              sep = ",")
View(Canada)

#Среднее арифметическое.
arif <- sum(Canada$population)/length(Canada$population)
arif

#Среднее гармоническое.
harm <- length(Canada$population)/sum(1/Canada$population)
harm

#Среднее геометрическое.
heometr <- (prod(Canada$population))^(1/length(Canada$population))
heometr

#Медианное значение.
median(Canada$population)

#Задание 13
ggplot(Canada, aes(x = year, y = population)) +
  geom_bar(stat = "identity", fill = "blue") +  
  theme_minimal() +
  labs(title = "Численность населения Канады по годам",
       x = "Год",
       y = "Численность населения") +
  scale_y_continuous(labels = scales::comma) 

#Задание 14
q1 <- quantile(Canada$population, 0.25)
q3 <- quantile(Canada$population, 0.75)
year_between <- subset(Canada, population >= q1 & population <= q3)
View(year_between)

#Задание 15
install.packages("zoo")
library(zoo)

skol <- rollmean(Canada$population, k = 3, fill = NA)
ggplot(Canada, aes(x = year)) +
  geom_line(aes(y = population), color = "blue") +
  geom_line(aes(y = skol), color = "red") +
  ggtitle("Скользящее среднее населения")