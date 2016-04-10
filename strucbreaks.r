# Real Time Structural Breaks
data <- read.csv("/media/ANIRUDH/China's Economic Woes/CURRFX-CNYUSD.csv")
View(data)
data <- tbl_df(data)
data <- arrange(data,Date)
View(data)
