#1 Part 1
#Question 1
stock_prices <- c(23, 27, 23, 21, 34, 24, 25, 30, 37, 30)
day_of_week <- c(rep(c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), 2))
#Question 2
day_of_week_factor <- as.factor(day_of_week)
named_stock_prices <- stock_prices
names(named_stock_prices) <- day_of_week
named_stock_prices
#Question 3
stock_data <- data.frame("day of the week" = day_of_week, "stock prices" = stock_prices)
stock_data
View(stock_data)
#Question 4
stock_list <- list("stock prices" = stock_prices, "day of the week" = day_of_week, "dataframe" = stock_data)
stock_list

#2 Part 2
#Question 1
stock_prices[4:6]
stock_prices[stock_prices > 30]
#Question 2
stock_data$day.of.the.week
subset(stock_data, stock_prices < 25)
stock_data$day.of.the.week
stock_data[3,]
stock_data[1, 3]
#Question 4
stock_list[[3]]
