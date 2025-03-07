rm(list = ls())

install.packages("fpp")
install.packages("forecast")

library(fpp)
library(forecast)
library(haven)
hodata <- read_sav("C:\\Users\\orteg\\Desktop\\ZAMANSERİSİ\\odev_zaman_serisi\\london_house.sav")
View(hodata)
summary(hodata$houses_sold)

#zaman serisi grafigi cizelim.
hodata_ts <- ts(hodata$houses_sold, start = c(1995, 1), frequency = 12)
ts.plot(hodata_ts, gpars = list(xlab = "Zaman", ylab = "Satılan Evler"))


#ACF ve PACF grafiklerini cizdirelim:

library(forecast)

Acf(hodata_ts,lag.max = 42,  ylim=c(-1,1), lwd=3)
Pacf(hodata_ts,lag.max = 42, ylim=c(-1,1), lwd=3)

Acf(diff(hodata_ts),lag.max = 42,  ylim=c(-1,1), lwd=3)
Pacf(diff(hodata_ts),lag.max = 42,  ylim=c(-1,1), lwd=3)


#periyot=12.

# Trend bileşenini regresyon analizi ile oluşturalım
hodata_trend <- tslm(hodata_ts ~ trend)

# Fitted values = orijinal serinin trend bileşeni
periyot <- hodata_ts - hodata_trend$fitted.values

Acf(periyot, lag.max = 42, ylim = c(-1, 1), lwd = 3)

hodata_ma <- ma(hodata_ts, order = 12, centre = TRUE)

Mevsim <- hodata_ts - hodata_ma

donemort <- t(matrix(data = Mevsim, nrow = 12, ncol = floor(length(hodata_ts) / 12)))
colMeans(donemort, na.rm = TRUE)


endeks <- colMeans(donemort, na.rm = TRUE) - mean(colMeans(donemort, na.rm = TRUE))
indeks <- rep(endeks, length.out = length(hodata_ts))


trenthata <- hodata_ts - indeks


zaman <- seq_along(trenthata)  
trend <- tslm(trenthata ~ zaman)


tahmin <- indeks + trend$fitted.values

# Hata serisini bulalım
hata <- hodata_ts - indeks - trend$fitted.values

# Orijinal seri ile tahmin serisinin uyumu
plot(window(hodata_ts), xlab = "Zaman", ylab = "", lty = 1, col = 4, lwd = 2)
lines(window(tahmin), lty = 3, col = 2, lwd = 3)
legend("topleft", c(expression(paste("house sold")), expression(paste("Tahmin"))), 
       lwd = c(2, 2), lty = c(1, 3), cex = 0.6, col = c(4, 2))

# Hataların akgürültü olup olmadığını kontrol edelim
Acf(hata, main = "Hata", lag.max = 42, ylim = c(-1, 1), lwd = 3)
Pacf(hata, main = "Hata", lag.max = 42, ylim = c(-1,1),lwd=3)
Box.test(hata, lag = 20, type = "Ljung-Box")


#####Carpimsal Ayristirma Yontemi

Mevsim1 <- hodata_ts/hodata_ma


donemort1<-t(matrix(data=Mevsim1, nrow = 12, ncol=5))
colMeans(donemort1, na.rm = T)
sum(colMeans(donemort1, na.rm = T))
mean(colMeans(donemort1, na.rm = T))

endeks1<- colMeans(donemort1, na.rm = T)/mean(colMeans(donemort1, na.rm = T))

indeks1<-  matrix(data = endeks1, nrow = 192)


trenthata1<- hodata_ts/indeks1


trenthata1 <- ts(trenthata1, start = c(1), frequency = 12)  
trent1 <- tslm(trenthata1 ~ trend)
trent1<- tslm(trenthata1~trend)
tahmin1<- indeks1*trent1[["fitted.values"]]
tahmin1 <- ts(tahmin1, start = start(hodata_ts), frequency = frequency(hodata_ts))

hata1<- hodata_ts - tahmin1

plot(hodata_ts, 
     xlab = "Zaman", ylab = "", lty = 1, col = 4, lwd = 2)
lines(tahmin1, lty = 3, col = 2, lwd = 3)
legend("topleft", 
       c(expression(paste(hodata)), expression(paste(Tahmin))),
       lwd = c(2, 2), lty = c(1, 3), cex = 0.6, col = c(4, 2))

#hatalar akgurultu mu?
Acf(hata1,main="Hata", lag.max = 42,  ylim=c(-1,1), lwd=3)
Pacf(hata1,main="Hata",lag.max = 42, ylim=c(-1,1), lwd=3)

Box.test(hata1, lag = 20, type = "Ljung-Box")

# Orijinal seri ile tahmin serisinin uyumu
plot(window(hodata_ts), 
     xlab = "Zaman", ylab = "", lty = 1, col = 4, lwd = 2)
lines(window(tahmin1), lty = 3, col = 2, lwd = 3)
legend("topleft", c(expression(paste("hodata_sales")),
                    expression(paste("Tahmin"))),
       lwd = c(2, 2), lty = c(1, 3), cex = 0.6, col = c(4, 2))
######################################

########## REGRESYON ANALİZİ ##########

library(fpp)
library(forecast)
library(haven)
library(lmtest)

n_obs <- 192
period <- 12

# Hedef değişkenin seçimi
y <- hodata$houses_sold[1:n_obs]
t <- 1:n_obs


########## TOPLAMSAL MODEL ##########
sin1 <- sin(2 * 3.1416 * t / 12)
cos1 <- cos(2 * 3.1416 * t / 12)
data_j_1 <- as.data.frame(cbind(hodata, t, sin1, cos1))
names(data_j_1) <- c("y", "t", "sin1", "cos1")
attach(data_j_1)

regresyon.model1 <- lm(y ~ t + sin1 + cos1)
summary(regresyon.model1)

########## ÇARPIMSAL MODEL ##########
sin1 <- t * sin(2 * 3.1416 * t / 12)
cos1 <- t * cos(2 * 3.1416 * t / 12)

data_t_j_1 <- as.data.frame(cbind(hodata, t, sin1, cos1))
names(data_t_j_1) <- c("y", "t", "sin1", "cos1")
attach(data_t_j_1)

regresyon.model2 <- lm(y ~ t + sin1 + cos1)
summary(regresyon.model2)

######################################################

###############################
## Toplamsal Winters Yöntemi ##
###############################

Winters1 <- ets(hodata_ts, model = "AAA")

# Modelin özeti
summary(Winters1)


# Winters1 modeli için tahminler
tahmin1 <- Winters1[["fitted"]]

# Orijinal seri ve tahminlerin karşılaştırılması
plot(window(hodata_ts), 
     xlab = "Zaman", ylab = "", lty = 1, col = 4, lwd = 2)
lines(window(tahmin1), lty = 3, col = 2, lwd = 3)
legend("topleft", c(expression(paste("Orijinal Seri")),
                    expression(paste("Winters1 Tahmin"))),
       lwd = c(2, 2), lty = c(1, 3), cex = 0.7, col = c(4, 2))

# Winters1 modelinin hata terimleri
hata1 <- Winters1[["residuals"]]

# Ljung-Box testi
Box.test(hata1, lag = 42, type = "Ljung")

# Hataların ACF ve PACF analizleri
Acf(hata1, main = "Hata", lag.max = 42, ylim = c(-1, 1), lwd = 3)
Pacf(hata1, main = "Hata", lag.max = 42, ylim = c(-1, 1), lwd = 3)


# 5 dönemlik öngörüler
ongoru <- forecast(Winters1, h = 5)

# Öngörülen değerlerin ortalaması
ongoru[["mean"]]

################################################

#CARPIMSAL
Winters2 <- ets(abs(hodata_ts), model = "MAM")
summary(Winters2)
tahmin2 <- Winters2[["fitted"]]

plot(window(hodata_ts), 
     xlab = "Zaman", ylab = "", lty = 1, col = 4, lwd = 2)
lines(window(tahmin2), lty = 3, col = 2, lwd = 3)
legend("topleft", c(expression(paste("Orijinal Seri")),
                    expression(paste("Winters2 Tahmin"))),
       lwd = c(2, 2), lty = c(1, 3), cex = 0.7, col = c(4, 2))


hata2 <- Winters2[["residuals"]]

# Ljung-Box testi
Box.test(hata2, lag = 42, type = "Ljung")

Acf(hata2, main = "Hata", lag.max = 42, ylim = c(-1, 1), lwd = 3)
Pacf(hata2, main = "Hata", lag.max = 42, ylim = c(-1, 1), lwd = 3)

ongoru <- forecast(Winters2, h = 5)
ongoru[["mean"]]


#################################################
#ARIMA


Acf(hodata_ts,lag.max = 36,  ylim=c(-1,1), lwd=3)
Pacf(hodata_ts,lag.max = 36,  ylim=c(-1,1), lwd=3)

Acf(diff(hodata_ts),lag.max = 36, ylim=c(-1,1), lwd=3)
Pacf(diff(hodata_ts),lag.max = 36, ylim=c(-1,1), lwd=3)

Acf(diff(diff(hodata_ts,12)),lag.max = 36, ylim=c(-1,1), lwd=3)
Pacf(diff(diff(hodata_ts,12)),lag.max = 36, ylim=c(-1,1), lwd=3)


#MODEL1
hodata_arima1 <- Arima(hodata_ts, order = c(0,1,1), seasonal= c(0,1,1), include.constant=TRUE)
coeftest(hodata_arima1)
summary(hodata_arima1)

#ANLAMLI ÇIKTI 

residuals <- residuals(hodata_arima1)  # Modelin artıklarını al
acf(residuals, main = "Artıkların ACF Grafiği")  # Artıkların ACF'sini çizdir
Box.test(residuals, lag = 20, type = "Ljung-Box")

#MODEL2
hodata_arima2 <- Arima(hodata_ts, order = c(0,1,1), seasonal= c(0,1,2), include.constant=TRUE)
coeftest(hodata_arima2)
summary(hodata_arima2)
# anlamlı çıkmıyor
residuals2 <- residuals(hodata_arima2)  # Modelin artıklarını al
acf(residuals2, main = "Artıkların ACF Grafiği model 2")  # Artıkların ACF'sini çizdir
Box.test(residuals2, lag = 20, type = "Ljung-Box")






#MODEL3
hodata_arima3 <- Arima(hodata_ts, order = c(3,1,0), seasonal= c(1,1,0), include.constant=TRUE)
coeftest(hodata_arima3)
summary(hodata_arima3)
#anlamlı çıkıyor hatalar sıkıntı

residuals <- residuals(hodata_arima3)  # Modelin artıklarını al
acf(residuals, main = "Artıkların ACF Grafiği")  # Artıkların ACF'sini çizdir
Box.test(residuals, lag = 20, type = "Ljung-Box")

#MODEL4
hodata_arima4 <- Arima(hodata_ts, order = c(3,1,0), seasonal= c(2,1,0), include.constant=TRUE)
coeftest(hodata_arima4)
summary(hodata_arima4)
#katsayılar anlamlı hatalar akgürültü değil 

residuals <- residuals(hodata_arima4)  # Modelin artıklarını al
acf(residuals, main = "Artıkların ACF Grafiği")  # Artıkların ACF'sini çizdir
Box.test(residuals, lag = 20, type = "Ljung-Box")
 

#MODEL5
hodata_arima5 <- Arima(hodata_ts, order = c(3,1,0), seasonal= c(3,1,0), include.constant=TRUE)
coeftest(hodata_arima5)
summary(hodata_arima5)
# hatalar akgürültü değil 

residuals <- residuals(hodata_arima5)  # Modelin artıklarını al
acf(residuals, main = "Artıkların ACF Grafiği")  # Artıkların ACF'sini çizdir
Box.test(residuals, lag = 20, type = "Ljung-Box")

#ikisinin aynı anda kontrol edilmesi 
#buradan sonra bak hızlı hızlı 
#MODEL6
hodata_arima6 <- Arima(hodata_ts, order = c(3,1,1), seasonal= c(1,1,1), include.constant=TRUE)
coeftest(hodata_arima6)
summary(hodata_arima6)

#MODEL7
hodata_arima7 <- Arima(hodata_ts, order = c(3,1,1), seasonal= c(1,1,0), include.constant=TRUE)
coeftest(hodata_arima7)
summary(hodata_arima7)
#ilk baştan yanlış 

#MODEL8
hodata_arima8 <- Arima(hodata_ts, order = c(3,1,1), seasonal= c(0,1,1), include.constant=TRUE)
coeftest(hodata_arima8)
summary(hodata_arima8)

#model9

hodata_arima9 <- Arima(hodata_ts, order = c(3,1,1), seasonal= c(0,1,0), include.constant=TRUE)
coeftest(hodata_arima9)
summary(hodata_arima9)
#ilk baştnan yanlış 




# arima2 modekim anlamlı çıktı bu kullanılarak öngörü yapılacak 

tahminarima<- hodata_arima1[["fitted"]]
hataarima<- hodata_arima1[["residuals"]]

plot( window(hodata_ts), 
      xlab="year", ylab="",lty=1, col=4, lwd=2)
lines( window(tahminarima) ,lty=3,col=2,lwd=3)
legend("topleft",c(expression(paste(house_sold)),
                   expression(paste(Tahmin))),
       lwd=c(2,2),lty=c(1,3), cex=0.7, col=c(4,2))

ongoru<- forecast(hodata_arima1, h=5 )

Box.test (hataarima, lag = 20, type = "Ljung")
ongoru


plot(window(hodata_ts), 
     xlab="year", ylab="", lty=1, col=4, lwd=2)
lines(window(tahminarima), lty=3, col=2, lwd=3)

# ARIMA tahminlerini ekle
lines(ongoru$mean, col="green", lty=2, lwd=2)  # Tahmin çizgisi


# Legend ekle
legend("topleft", c(expression(paste(house_sold)),
                    expression(paste(Tahmin)),
                    expression(paste(Ongoru))),
       lwd=c(2,2,2), lty=c(1,3,2), cex=0.7, col=c(4,2,"green"))
ongoru

