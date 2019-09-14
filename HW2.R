rm(list = ls())
df = read.csv("/Users/shelleyshi/Desktop/Anderson/19 Spring/MGMTMSA413 - CARO/Forcast Assignment 2/Metro_Zhvi_Condominum.csv", header = TRUE)
df = df[2:31,] 

# train-set period is from January 2010 to December 2017
train = df[,1:264]

# test-set period is from January 2018 to date
test = df[,c(1:3,265:280)]
actual = as.numeric(test[1,4:19])

price = as.numeric(train[1,169:264])
plot(diff(price),type = 'l')
price = log(price)
price = diff(price)

arima = arima(price,order = c(1,0,0))
arima
forcast = forecast(arima,15)

# predicted next 15 month diff(log(price))
forcast$mean 
# actual next 15 month diff(log(price))
diff(log(actual)) 

# additional predictors (X)
MedianPrice = read.csv("/Users/shelleyshi/Desktop/Anderson/19 Spring/MGMTMSA413 - CARO/Forcast Assignment 2/Metro_MedianListingPrice_AllHomes.csv")
MedianPrice = MedianPrice[2:31,]
d = c()
for (i in 1:30){
  x = log(as.numeric(MedianPrice[i,3:98]))
  price = diff(log(as.numeric(train[i,169:264])))
  model = arima(price,order = c(1,0,0), xreg = x[-1])
  d[i] = coeftest(model4, type = "HC1")[12]
}
mean(d)
min(d)
max(d)
median(d)