library(zoo)
library(tidyverse)
library(tidyquant)
library(lubridate)
library(fitdistrplus) # For MLE and other statistics
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Input the ticker to be analyzed, it's starting date ("YYYY-MM-DD"), timeframe, and other settings
symbol <- "SPY"                   # Equity or ETF ticker to pull from YahooFinance
startDate <- "2003-01-01"         # Specify start date as a string "YYYY-MM-DD"
endDate <- today()                # Specify end date: today(), otherwise a string "YYYY-MM-DD"
timeframe <- "daily"              # Specify timeframe: "daily", "weekly", "monthly"

MAperiods <- 50                   # Simple Moving Average periods

# Get the data
tickerData <- tq_get(x = symbol,
                  get = "stock.prices",
                  from = "1800-01-01",
                  to = today(),
                  periodicity = timeframe) %>% 
  dplyr::select(-"symbol")

names(tickerData) <- c("Date", "Open", "High", "Low", "Close", "Volume", "AdjClose")

# Calculate the Simple Moving Average (SMA)
tickerData <- tickerData %>% 
  mutate(SMA = rollapplyr(data = Close, width = MAperiods, FUN = mean, fill = NA))

# Filter out the NA rows and rows before the desired start date
tickerData <- filter(tickerData, Date >= startDate & Date <= endDate) %>% 
  drop_na()

# Flag buy/sell signals (1 = Buy to Open, 2 = Sell to Close)
tickerData$Entry <- 0
i <- 1

while(i <= nrow(tickerData)) {
  if(tickerData$Close[i] > tickerData$SMA[i]) {
    tickerData$Entry[i] <- 1
    i = i + 1
    repeat {
      if(tickerData$Close[i] < tickerData$SMA[i]) {
        tickerData$Entry[i] <- 2
        break
      }
      else {
        i = i + 1
      }
    }
  }
  i = i + 1
}

# Calculate profits
i <- 1
buyTemp <- 0
sellTemp <- 0
profitVec <- c()

while(i <= nrow(tickerData)) {
  if(tickerData$Entry[i] == 1) {
    buyTemp = tickerData$Close[i]
    i = i + 1
    repeat {
      if(tickerData$Entry[i] == 2) {
        sellTemp = tickerData$Close[i]
        profitVec <- append(profitVec, ((sellTemp-buyTemp)/buyTemp)*100)
        break
      }
      else {
        i = i + 1
      }
    }
  }
  i = i + 1
}

# Print results
descdist(profitVec)

hist(profitVec, breaks = 20,
     main = "Returns Distribution",
     xlab = "Return (%)")

ggplot(data = data.frame(x = profitVec)) +
  geom_histogram(mapping = aes(x = data.frame(x = profitVec)$x, y = ..density..), binwidth = 1) +
  labs(title = "Returns Distribution", x = "Return (%)")

cat("\n", "SMA Crossing Test:",
    "\n",
    "\n Sum:\t\t\t", round(sum(profitVec), digits = 4), "%",
    "\n UL P/L:\t\t", round((((profitVec[length(profitVec)] - profitVec[1])/profitVec[1])*100), digits = 4), "%",
    "\n Mean:\t\t\t", round(mean(profitVec), digits = 4), "%",
    "\n StdDev:\t\t", round(sd(profitVec), digits = 4), "%",
    "\n Median:\t\t", round(median(profitVec), digits = 4), "%",
    "\n Min:\t\t\t", round(min(profitVec), digits = 4), "%",
    "\n Max:\t\t\t", round(max(profitVec), digits = 4), "%",
    "\n Kurtosis:\t\t", round(kurtosis(profitVec), digits = 4),
    "\n",
    "\n Win Freq:\t\t", round(length(profitVec[profitVec > 0])/length(profitVec)*100, digits = 4), "%",
    "\n Lose Freq:\t\t", round(length(profitVec[profitVec < 0])/length(profitVec)*100, digits = 4), "%",
    "\n Sample Size:\t\t", length(profitVec),
    "\n\n")
