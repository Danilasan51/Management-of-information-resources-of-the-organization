install.packages("rpivotTable")
install.packages("readxl")
library(rpivotTable)
library(readxl)

R <- read_excel("problems_1.xlsx")

R <- data.frame(R$ID, R$AdmArea, R$Month, as.numeric(R$Year), as.numeric(R$TotalAmount))
colnames(R)<- c("ID", "AdmArea", "Month", "Year", "TotalAmount")

View(R)

first <- data.frame(R$AdmArea[(R$Year == 2016) | (R$Year == 2017) | (R$Year == 2018) | (R$Year == 2019)], 
                    R$Year[(R$Year == 2016) | (R$Year == 2017) | (R$Year == 2018) | (R$Year == 2019)], 
                    R$TotalAmount[(R$Year == 2016) | (R$Year == 2017) | (R$Year == 2018) | (R$Year == 2019)])

colnames(first) <- c("AdmArea", "Year", "TotalAmount")

View(first)

rpivotTable::rpivotTable(first, rows = "Year", cols = "AdmArea", vals = "TotalAmount", aggregatorName = "Average")

first_second <- data.frame(R$Month[(R$Year == 2021)], 
                    R$TotalAmount[(R$Year == 2021)])

colnames(first_second) <- c("Month", "TotalAmount")

rpivotTable::rpivotTable(first_second, rows = "Month", vals = "TotalAmount", aggregatorName = "Sum")

first_third <- data.frame(R$AdmArea, 
                    R$Year,
                    R$TotalAmount)

colnames(first_third) <- c("AdmArea", "Year", "TotalAmount")

rpivotTable::rpivotTable(first_third, rows = "AdmArea", col = "Year", vals = "TotalAmount", aggregatorName = "Average")

boxplot(TotalAmount ~ Year * AdmArea, data = R, 
        main = "Средние ежегодные начисления по округам",
        xlab = "Год", ylab = "Средняя сумма начислений",
        names.arg = Year, col = rainbow(length(areas)))

second <- data.frame(R$ID[R$AdmArea == "Центральный административный округ"],
                    R$AdmArea[R$AdmArea == "Центральный административный округ"],
                    R$Month[R$AdmArea == "Центральный административный округ"],
                    R$Year[R$AdmArea == "Центральный административный округ"], 
                    R$TotalAmount[R$AdmArea == "Центральный административный округ"])

colnames(second) <- c("ID", "AdmArea", "Month", "Year", "TotalAmount")

View(second)

third_meaning = mean(second$TotalAmount)

View(third_meaning)

third <- data.frame(second$Month[second$Year == 2016 | second$TotalAmount >= mean(Total)],
                    second$Year[R$AdmArea == "Центральный административный округ"])

save(second, file = "Second_dataframe.RData")

yearly_mean <- aggregate(TotalAmount ~ Year, data = second, FUN = mean)

second <- merge(second, yearly_mean, by = "Year", suffixes = c("", "_Mean"))

third <- second[second$TotalAmount > second$TotalAmount_Mean, ]

third <- third[, c("ID", "AdmArea", "Month", "Year", "TotalAmount")]

View(third)
save(third, file = "Third_dataframe.RData")
