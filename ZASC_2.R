# Ewa Rudzińska (432501)


#BIBLIOTEKI
library("readr")
library("xts")
library("dygraphs")
library("car")
library("FinTS")
library("fBasics")
library("fGarch")
library("rugarch")
library("tseries")
source("functions.R")
Sys.setlocale("LC_TIME", "en_US")

"C:\Users\ewaru\OneDrive\Pulpit\szeregi_projekt\ena.csv"
#DANE
el <- read_csv("elysia-data.csv", sep=";", header = TRUE) # Elysia
rndr = read_csv("render-data.csv", sep=";", header = TRUE) # Render
vet = read_csv("vechain-data.csv", sep=";", header = TRUE) # VeChain
voxel = read_csv("voxies-data.csv", sep=";", header = TRUE) # Voxies

# Sprawdzenie pierwszych wierszy
head(el)
head(rndr)
head(vet)
head(voxel)

# Przycięcie danych do wspólnej liczby wierszy - wszystkie mają po 400 obserwacji

# Zmiana formatu daty
el$timestamp <- as.Date(el$timestamp, format = "%Y-%m-%d")
rndr$timestamp <- as.Date(rndr$timestamp, format = "%Y-%m-%d")
vet$timestamp <- as.Date(vet$timestamp, format = "%Y-%m-%d")
voxel$timestamp <- as.Date(voxel$timestamp, format = "%Y-%m-%d")

# Formatowanie kolumn (upewnij się, że indeksy kolumn są poprawne)
el <- el[c(9,11,12)]
rndr <- rndr[c(9,11,12)]
vet <- vet[c(9,11,12)]
voxel <- voxel[c(9,11,12)]

# Zmiana nazw kolumn - 
colnames(el) <- c("el_close", "el_market_cap", "Date")
colnames(rndr) <- c("rndr_close", "rndr_market_cap", "Date")
colnames(vet) <- c("vet_close", "vet_market_cap", "Date")
colnames(voxel) <- c("voxel_close", "voxel_market_cap", "Date")

# Sprawdzenie zakresów dat po konwersji
print(range(el$Date))
print(range(rndr$Date))
print(range(vet$Date))
print(range(voxel$Date))

# Upewniłyśmy się że wszyskie kruptowaluty mają dokładnie ten sam zakres dat czyli dzienne notowania od
# 28.05.2023 do 30.06.2024 - po 400 obserwacji

# Połączenie wartości market_cap dla wszystkich kryptowalut w jednej tabeli z datą
dane<-el[,c(1,2)]
dane$rndr_close<-rndr$rndr_close
dane$rndr_market_cap<-rndr$rndr_market_cap
dane$vet_close<-vet$vet_close
dane$vet_market_cap<-vet$vet_market_cap
dane$voxel_close<-voxel$voxel_close
dane$voxel_market_cap<-voxel$voxel_market_cap
dane$Date<-voxel$Date

# Wyświetlenie wyników
View(dane)

#####
#BUDOWA PORTFELA 
dane$total_market_cap = dane$el_market_cap+dane$rndr_market_cap+dane$vet_market_cap+dane$voxel_market_cap
dane$el_p = dane$el_market_cap/dane$total_market_cap
dane$rndr_p = dane$rndr_market_cap/dane$total_market_cap
dane$vet_p = dane$vet_market_cap/dane$total_market_cap
dane$voxel_p = dane$voxel_market_cap/dane$total_market_cap

#STOPY ZWROTU 
dane$el_r = diff.xts(log(dane$el_close))
dane$rndr_r = diff.xts(log(dane$rndr_close))
dane$vet_r = diff.xts(log(dane$vet_close))
dane$voxel_r = diff.xts(log(dane$voxel_close))

#STOPA ZWROTU Z PORTFELA 
dane$total_r = (dane$el_p*dane$el_r+dane$rndr_p*dane$rndr_r
                +dane$vet_p*dane$vet_r+dane$voxel_p*dane$voxel_r)

# UDZIAŁ W CZASIE 
share_xts = xts(dane[, c("el_p", "rndr_p", "vet_p", "voxel_p")], order.by = dane$Date)
tot_ret_xts = xts(dane[, c("total_r")], order.by = as.Date(dane$Date))
close_xts = xts(dane[, c("el_close", "rndr_close", "vet_close", "voxel_close")], order.by = dane$Date)

####CENY ZAMKNIECIA
dygraph(close_xts, main = "Ceny zamknięcia") %>%
  dyRangeSelector(height = 50)

###PROFIL PORTFELOWY 
dygraph(share_xts, main = "Profil portfelowy") %>%
  dyRangeSelector(height = 40)

###STOPY ZWROTÓW
dygraph(tot_ret_xts, main="Stopa zwrotu") %>% 
  dyRangeSelector(height = 50)

# Wykres ACF dla zwrotow z portfela
acf(dane$total_r, lag.max = 36, na.action = na.pass,
    ylim = c(-0.4, 0.4), 
    col = "darkblue", lwd = 7,
    main = "Wykres ACF zwrotów z portfela")

####BADANIE AUTOKORELACJI RESZT
durbinWatsonTest(lm(dane$total_r ~ 1), max.lag = 5)

# Wykresy ACF^2 dla zwrotów z portfela (efekty ARCH)
acf(dane$total_r^2, lag.max = 100, na.action = na.pass,
    ylim = c(-0.3, 0.3),
    col = "darkblue", lwd = 7,
    main = "Wykres ACF kwadratów zwrotów z portfela")

# test ARCH: p-value bliskie zera - odrzucamy H0 
ArchTest(dane$total_r, lags = 5)

# Test na autokorelację kwadratów reszt - autokorelacja dla 1% poziomu istotności dla 2, 3, 4 i 5 opóźnienia
durbinWatsonTest(lm(dane$total_r^2 ~ 1), max.lag = 5) 

# Porównanie rozkładu z rozkładem normalnym
hist(dane$total_r, prob = T, breaks = 50, main = "Histogram rozkładu stóp zwrotu portfela", xlab="returns", col="skyblue1")
curve(dnorm(x, mean = mean(dane$total_r, na.rm = T),
            sd  = sd(dane$total_r, na.rm = T)),
      col = "darkblue", lwd = 2, add = TRUE,
)

#STATYSTYKI OPISOWE
empstats = basicStats(dane$total_r)
knitr::kable(as.matrix(empstats), digits = 2)

#TEST NA NORMALNOŚĆ
(jbtest = jarque.bera.test(na.omit(dane$total_r)))

#####
#MODELOWANIE
(n_outs = round(0.3 * length(dane$Date)))
# in sample
ins = dane[which(dane$Date <= dane$Date[length(dane$Date)-n_outs]),]
# out of sample
outs = dane[-which(dane$Date <= dane$Date[length(dane$Date)-n_outs]),]
# wykresy zwrotów in sample i out of sample
plot(ins$Date, ins$total_r, type = "l", col = "black", lwd = 2, main = "Zwroty z portfela in sample",
     xlab = "Szereg", ylab = "Zwroty z portfela")
plot(outs$Date, outs$total_r, type = "l", col = "blue", lwd = 2, main = "Zwroty z portfela out of sample",
     xlab = "Szereg", ylab = "Zwroty z portfela")

#####
#PRÓBA ZNALEZIENIA NAJLEPSZEGO MODELU 

# GARCH(1,1) 
spec = ugarchspec(variance.model = list(model = "sGARCH",
                                        garchOrder = c(1, 1)),
                  mean.model = list(armaOrder = c(0, 0),
                                    include.mean = T),
                  distribution.model = "norm")

ins.garch11 = ugarchfit(spec = spec,
                        data = na.omit(ins$total_r))
ins.garch11
# stała w równaniu średniej jest nieistotna, więc możemy ją wykluczyć z modelu

# GARCH(1,1) mu = 0 
spec = ugarchspec(variance.model = list(model = "sGARCH",
                                        garchOrder = c(1, 1)),
                  mean.model = list(armaOrder = c(0,0),
                                    include.mean = F),
                  distribution.model = "norm")

ins.garch11mu0 = ugarchfit(spec = spec,
                           data = na.omit(ins$total_r))
ins.garch11mu0
# alfa na granicy istotności można spróbować bez niej

# GARCH(0,1) mu = 0 
spec = ugarchspec(variance.model = list(model = "sGARCH",
                                        garchOrder = c(0, 1)),
                  mean.model = list(armaOrder = c(0,0),
                                    include.mean = F),
                  distribution.model = "norm")

ins.garch01mu0 = ugarchfit(spec = spec,
                           data = na.omit(ins$total_r))
ins.garch01mu0

# we wszystkich 3 przechodzą testy Ljunga-Boxa dla reszt, dla kwadratow reszt i test ARCH
# ale spróbujemy tez innych modeli, żeby może znaleźć taki z najmiejszymi kryteriami informacyjnymi

# MA(1)-GARCH(1,1) mu = 0  #
spec = ugarchspec(variance.model = list(model = "sGARCH",
                                        garchOrder = c(1, 1)),
                  mean.model = list(armaOrder = c(0, 1),
                                    include.mean = F),
                  distribution.model = "norm")

ins.garch11ma1mu0 = ugarchfit(spec = spec,
                              data = na.omit(ins$total_r))
ins.garch11ma1mu0

# MA(1)-GARCH(0,1) mu = 0  #
spec = ugarchspec(variance.model = list(model = "sGARCH",
                                        garchOrder = c(0, 1)),
                  mean.model = list(armaOrder = c(0, 1),
                                    include.mean = F),
                  distribution.model = "norm")

ins.garch01ma1mu0 = ugarchfit(spec = spec,
                              data = na.omit(ins$total_r))
ins.garch01ma1mu0

# AR(1)-GARCH(1,1) mu = 0  #
spec = ugarchspec(variance.model = list(model = "sGARCH",
                                        garchOrder = c(1, 1)),
                  mean.model = list(armaOrder = c(1, 0),
                                    include.mean = F),
                  distribution.model = "norm")

ins.garch11ar1mu0 = ugarchfit(spec = spec,
                              data = na.omit(ins$total_r))
ins.garch11ar1mu0

# AR(1)-GARCH(0,1) mu = 0  #
spec = ugarchspec(variance.model = list(model = "sGARCH",
                                        garchOrder = c(0, 1)),
                  mean.model = list(armaOrder = c(1, 0),
                                    include.mean = F),
                  distribution.model = "norm")

ins.garch01ar1mu0 = ugarchfit(spec = spec,
                              data = na.omit(ins$total_r))
ins.garch01ar1mu0

# ARMA(1,1)-GARCH(1,1) mu = 0 #
spec = ugarchspec(variance.model = list(model = "sGARCH",
                                        garchOrder = c(1, 1)),
                  mean.model = list(armaOrder = c(1, 1),
                                    include.mean = F),
                  distribution.model = "norm")

ins.garch11arma11mu0 = ugarchfit(spec = spec,
                                 data = na.omit(ins$total_r))
ins.garch11arma11mu0

# ARMA(1,1)-GARCH(0,1) mu = 0 #
spec = ugarchspec(variance.model = list(model = "sGARCH",
                                        garchOrder = c(0, 1)),
                  mean.model = list(armaOrder = c(1, 1),
                                    include.mean = F),
                  distribution.model = "norm")

ins.garch01arma11mu0 = ugarchfit(spec = spec,
                                 data = na.omit(ins$total_r))
ins.garch01arma11mu0

infocriteria(ins.garch11)
infocriteria(ins.garch11mu0)
infocriteria(ins.garch01mu0)
infocriteria(ins.garch11ma1mu0)
infocriteria(ins.garch01ma1mu0)
infocriteria(ins.garch11ar1mu0)
infocriteria(ins.garch01ar1mu0)
infocriteria(ins.garch11arma11mu0)
infocriteria(ins.garch01arma11mu0)


#### NAJLEPSZY MODEL: ARMA(1,1)-GARCH(1,1) mu = 0 

plot(ins.garch11arma11mu0, which = 11)
plot(ins.garch11arma11mu0, which = 10)
ins.garch11arma11mu0

#### HISTOGRAM RESZT NAJLEPSZEGO MODELU
hist(ins.garch11arma11mu0@fit$residuals, prob = T, breaks = 50,
     main = "Histogram reszt modelu", xlab="residuals", col="skyblue1")
curve(dnorm(x, mean = mean(ins.garch11arma11mu0@fit$residuals, na.rm = T),
            sd  = sd(ins.garch11arma11mu0@fit$residuals, na.rm = T)),
      col = "darkblue", lwd = 2, add = TRUE,
)

### STATYSTYKI RESZT MODELU 
empstats = basicStats(ins.garch11arma11mu0@fit$residuals)
knitr::kable(as.matrix(empstats), digits = 2)

##RESZTY - BADANIE ROZKŁADU 
(jbtest_residuals = jarque.bera.test(ins.garch11arma11mu0@fit$residuals))

#WYKRES RESZT
plot(ins.garch11arma11mu0@fit$residuals, type ="l", lwd = 2, main = "Reszty modelu", ylab = "Reszty")

#####
#PROGNOZY WARUNKOWEJ WARIANCJI

# oceny parametrów
ins.garch11arma11mu0@fit$coef

# bezwarunkowa wariancja#ipars bezwarunkowa wariancja
var_uncond = ins.garch11arma11mu0@fit$coef[3] / (1 - ins.garch11arma11mu0@fit$coef[4]
                                                 - ins.garch11arma11mu0@fit$coef[5])
names(var_uncond) = "wariancja bezwarunkowa"
var_uncond

# prognozy warunkowej wariancji na 100 okresóW
fore100 = ugarchforecast(ins.garch11arma11mu0, n.ahead = 100)
sigma(fore100)

# wykres 
plot(sigma(fore100)^2, type = "l")
abline(h = var_uncond, col = "red", lty = 2)
title(main = "Warunkowa i bezwarunkowa wariancja zwrotów")

# analogicznie dla 300 okresóW
fore300 = ugarchforecast(ins.garch11arma11mu0, n.ahead = 300)
plot(sigma(fore300)^2, type = "l")
abline(h = var_uncond, col = "red", lty = 2)
title(main = "Warunkowa i bezwarunkowa wariancja zwrotów")

# Prognozy warunkowej wariancji zbiegają w długim okresie do poziomu wariancji bezwarunkowej

########################
#VAULE-AT-RISK

# standaryzacja zwrotów i pierwszy kwantyl empiryczny in sample
ins$total_r_std = (ins$total_r - mean(ins$total_r, na.rm=T)) /
  sd(ins$total_r, na.rm = T)

(total_r_std_q01 = quantile(ins$total_r_std, 0.01, na.rm = T))

#### VaR w okresie in sample
ins.garch11arma11mu0@fit$sigma

# obliczanie wartości narażonej na ryzyko 
ins$VaR = total_r_std_q01 * ins.garch11arma11mu0@fit$sigma

# wyres zwrotów i VaR
plot(ins$Date, ins$total_r, col = "red", lwd = 1, type = "l", ylim=c(-0.15, 0.15),
     ylab = "zwroty vs. VaR", xlab = "Szereg w okresie IN-SAMPLE", main = "Oszacowania 1% Value at Risk")
abline(h = 0, lty = 2)
lines(ins$Date, ins$VaR, type = "l", col = "green")

# w ilu przypadkach straty przekroczyły zakładany poziom VaR?
sum(ins$total_r < ins$VaR) / length(ins$VaR)

#### VaR w okresie OUT-OF-SAMPLE

# 1-dniowa prognoza warunkowego odchylenia standardowego
# plot(ugarchforecast(ins.garch11ma1mu0, n.ahead = 1))
sigma.forecast = ugarchforecast(ins.garch11arma11mu0, n.ahead = 1)
sigma.forecast2 = sigma.forecast@forecast$sigmaFor[1, 1]

# Szacowanie 1-dniowej VaR dla całego okresu OUT-OF-SAMPLE
dane$obs = 1:length(dane$total_r)
start  = dane$obs[dane$Date == as.Date(dane$Date[length(dane$Date)-n_outs+1])]
finish = dane$obs[dane$Date == as.Date(dane$Date[length(dane$Date)])]
data2 = dane[start:finish, ]
VaR = rep(NA, times = finish - start)

for (k in start:finish) {
  tmp.data = dane[dane$obs <= k, ]
  tmp.data$rstd = (tmp.data$total_r - mean(tmp.data$total_r, na.rm = T)) /
    sd(tmp.data$total_r, na.rm = T)
  q01 = quantile(tmp.data$rstd, 0.01, na.rm = T)
  spec = ugarchspec(variance.model = list(model = "sGARCH",
                                          garchOrder = c(1, 1)),
                    mean.model = list(armaOrder = c(1, 1),
                                      include.mean = F),
                    distribution.model = "norm")
  tmp.garch11arma11mu0 = ugarchfit(spec = spec, data = na.omit(tmp.data$total_r))
  sigma.forecast  = ugarchforecast(tmp.garch11arma11mu0, n.ahead = 1)
  sigma.forecast2 = sigma.forecast@forecast$sigmaFor[1, 1]
  VaR[k - start + 1] = q01 * sigma.forecast2
}

data2$VaR = VaR

# wyres zwrotów i VaR w okresie OUT-OF-SAMPLE
plot(data2$Date, data2$total_r, col = "red", lwd = 1, type = "l", ylim=c(-0.15, 0.15),
     ylab = "zwroty vs VaR", xlab = "Szereg w okresie OUT-OF-SAMPLE", main = "Oszacowania 1% Value at Risk")
abline(h = 0, lty = 2)
lines(data2$Date, data2$VaR, type = "l", col = "green")

# w ilu przypadkach straty przekroczyły zakładany poziom VaR?
sum(data2$total_r < data2$VaR) / length(data2$VaR)

################################################################################
# analogiczna analiza dla modelu GARCH-t
# testy w poprzednim przykładzie pokazały, że rozkład reszt nie jest normalny, 
# więc być może reszty lepiej opisze rozkład t-studenta
################################################################################

#####
#PROBA ZNALEZIENIA NAJLEPSZEGO MODELU GARCH-t

# GARCH(1,1) #
spec = ugarchspec(
  variance.model = list(model = "sGARCH",
                        garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0),
                    include.mean = T),
  distribution.model = "std")

ins.garch11 = ugarchfit(spec = spec,
                        data = na.omit(ins$total_r))
ins.garch11
# mu nieistotne

# GARCH(1,1) mu = 0
spec = ugarchspec(
  variance.model = list(model = "sGARCH",
                        garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0),
                    include.mean = F),
  distribution.model = "std")

ins.garch11mu0 = ugarchfit(spec = spec,
                        data = na.omit(ins$total_r))
ins.garch11mu0
#alfa silnie nieistotne

# GARCH(0,1) mu = 0
spec = ugarchspec(
  variance.model = list(model = "sGARCH",
                        garchOrder = c(0, 1)),
  mean.model = list(armaOrder = c(0, 0),
                    include.mean = F),
  distribution.model = "std")

ins.garch01mu0 = ugarchfit(spec = spec,
                           data = na.omit(ins$total_r))
ins.garch01mu0

# MA(1)-GARCH(1,1) mu = 0  #
spec = ugarchspec(
  variance.model = list(model = "sGARCH",
                        garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 1),
                    include.mean = F),
  distribution.model = "std")

ins.garch11ma1mu0 = ugarchfit(spec = spec,
                              data = na.omit(ins$total_r))

ins.garch11ma1mu0

# MA(1)-GARCH(0,1) mu = 0  #
spec = ugarchspec(
  variance.model = list(model = "sGARCH",
                        garchOrder = c(0, 1)),
  mean.model = list(armaOrder = c(0, 1),
                    include.mean = F),
  distribution.model = "std")

ins.garch01ma1mu0 = ugarchfit(spec = spec,
                              data = na.omit(ins$total_r))
ins.garch01ma1mu0

# AR(1)-GARCH(1,1) mu = 0  #
spec = ugarchspec(
  variance.model = list(model = "sGARCH",
                        garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(1, 0),
                    include.mean = F),
  distribution.model = "std")

ins.garch11ar1mu0 = ugarchfit(spec = spec,
                              data = na.omit(ins$total_r))
ins.garch11ar1mu0

# AR(1)-GARCH(0,1) mu = 0  #
spec = ugarchspec(
  variance.model = list(model = "sGARCH",
                        garchOrder = c(0, 1)),
  mean.model = list(armaOrder = c(1, 0),
                    include.mean = F),
  distribution.model = "std")

ins.garch01ar1mu0 = ugarchfit(spec = spec,
                              data = na.omit(ins$total_r))
ins.garch01ar1mu0

# ARMA(1,1)-GARCH(1,1) mu = 0 #
spec = ugarchspec(
  variance.model = list(model = "sGARCH",
                        garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(1, 1),
                    include.mean = F),
  distribution.model = "std")

ins.garch11arma11mu0 = ugarchfit(spec = spec,
                                 data = na.omit(ins$total_r))
ins.garch11arma11mu0

# ARMA(1,1)-GARCH(0,1) mu = 0 #
spec = ugarchspec(
  variance.model = list(model = "sGARCH",
                        garchOrder = c(0, 1)),
  mean.model = list(armaOrder = c(1, 1),
                    include.mean = F),
  distribution.model = "std")

ins.garch01arma11mu0 = ugarchfit(spec = spec,
                                 data = na.omit(ins$total_r))
ins.garch01arma11mu0

infocriteria(ins.garch11)
infocriteria(ins.garch11mu0)
infocriteria(ins.garch01mu0)
infocriteria(ins.garch11ma1mu0)
infocriteria(ins.garch01ma1mu0)
infocriteria(ins.garch11ar1mu0)
infocriteria(ins.garch01ar1mu0)
infocriteria(ins.garch11arma11mu0)
infocriteria(ins.garch01arma11mu0)

#### NAJLEPSZY MODEL: ARMA(1,1)-GARCH(0,1) mu = 0

plot(ins.garch01arma11mu0, which = 11)
plot(ins.garch01arma11mu0, which = 10)
ins.garch01arma11mu0

#### HISTOGRAM RESZT NAJLEPSZEGO MODELU
hist(ins.garch01arma11mu0@fit$residuals, prob = T, breaks = 50,
     main = "Histogram reszt modelu", xlab="residuals", col="skyblue1")
curve(dnorm(x, mean = mean(ins.garch01arma11mu0@fit$residuals, na.rm = T),
            sd  = sd(ins.garch01arma11mu0@fit$residuals, na.rm = T)),
      col = "darkblue", lwd = 2, add = TRUE,
)

### STATYSTYKI RESZT MODELU 
empstats = basicStats(ins.garch01arma11mu0@fit$residuals)
knitr::kable(as.matrix(empstats), digits = 2)

#WYKRES RESZT
plot(ins.garch01arma11mu0@fit$residuals, type ="l", lwd = 2, main = "Reszty modelu", ylab = "Reszty")
jarque.bera.test(ins.garch01arma11mu0@fit$residuals)

#####
#PROGNOZY WARUNKOWEJ WARIANCJI

# oceny parametrów
ins.garch01arma11mu0@fit$coef

# bezwarunkowa wariancja
var_uncond = ins.garch01arma11mu0@fit$coef[3] / (1 - ins.garch01arma11mu0@fit$coef[4])
names(var_uncond) = "wariancja bezwarunkowa"
var_uncond

# prognozy warunkowej wariancji na 100 okresów
fore100 = ugarchforecast(ins.garch01arma11mu0, n.ahead = 100)
sigma(fore100)

# wykres 
plot(sigma(fore100)^2, type = "l", ylim=c(0.002,0.014))
abline(h = var_uncond, col = "red", lty = 2)
title(main = "Warunkowa i bezwarunkowa wariancja zwrotów")

# analogicznie dla 300 okresóW
fore300 = ugarchforecast(ins.garch01arma11mu0, n.ahead = 300)
plot(sigma(fore300)^2, type = "l", ylim=c(0.002,0.014))
abline(h = var_uncond, col = "red", lty = 2)
title(main = "Warunkowa i bezwarunkowa wariancja zwrotów")

# analogicznie dla 600 okresóW
fore600 = ugarchforecast(ins.garch01arma11mu0, n.ahead = 600)
plot(sigma(fore600)^2, type = "l", ylim=c(0.002,0.014))
abline(h = var_uncond, col = "red", lty = 2)
title(main = "Warunkowa i bezwarunkowa wariancja zwrotów")

# analogicznie dla 1000 okresóW
fore1000 = ugarchforecast(ins.garch01arma11mu0, n.ahead = 1000)
plot(sigma(fore1000)^2, type = "l", ylim=c(0.002,0.014))
abline(h = var_uncond, col = "red", lty = 2)
title(main = "Warunkowa i bezwarunkowa wariancja zwrotów")

# analogicznie dla 10000 okresóW
fore10000 = ugarchforecast(ins.garch01arma11mu0, n.ahead = 10000)
plot(sigma(fore10000)^2, type = "l", ylim=c(0.002,0.014))
abline(h = var_uncond, col = "red", lty = 2)
title(main = "Warunkowa i bezwarunkowa wariancja zwrotów")

# Prognozy warunkowej wariancji (dopiero przy prognozie dla 10 000 okresów) 
# zbiegają w długim okresie do poziomu wariancji bezwarunkowej

########################
#VAULE-AT-RISK

# ins = ins[ins$Date > as.Date(ins$Date[1]), ] # ucięcie jednej brakującej obserwacji

# standaryzacja zwrotów i pierwszy kwantyl empiryczny in sample
ins$total_r_std = (ins$total_r - mean(ins$total_r, na.rm=T)) /
  sd(ins$total_r, na.rm = T)

(total_r_std_q01 = quantile(ins$total_r_std, 0.01, na.rm = T))

#### VaR w okresie in sample
ins.garch01arma11mu0@fit$sigma

# obliczanie wartości narażonej na ryzyko 
ins$VaR = total_r_std_q01 * ins.garch01arma11mu0@fit$sigma

# wyres zwrotów i VaR
plot(ins$Date, ins$total_r, col = "red", lwd = 1, type = "l", ylim=c(-0.15, 0.15),
     ylab = "zwroty vs. VaR", xlab = "Szereg w okresie IN-SAMPLE", main = "Oszacowania 1% Value at Risk")
abline(h = 0, lty = 2)
lines(ins$Date, ins$VaR, type = "l", col = "green")

# w ilu przypadkach straty przekroczyły zakładany poziom VaR?
sum(ins$total_r < ins$VaR) / length(ins$VaR)


# VAULE-AT-RISK

# ins = ins[ins$Date > as.Date(ins$Date[1]), ] # ucięcie jednej brakującej obserwacji

# Standaryzacja zwrotów i pierwszy kwantyl empiryczny in sample
ins$total_r_std = (ins$total_r - mean(ins$total_r, na.rm = TRUE)) / sd(ins$total_r, na.rm = TRUE)
total_r_std_q01 = quantile(ins$total_r_std, 0.01, na.rm = TRUE)

# VaR w okresie in sample
ins.garch01arma11mu0@fit$sigma

# Obliczanie wartości narażonej na ryzyko
ins$VaR = total_r_std_q01 * ins.garch01arma11mu0@fit$sigma

# Wykres zwrotów i VaR
plot(ins$Date, ins$total_r, col = "red", lwd = 1, type = "l", ylim = c(-0.15, 0.15),
     ylab = "zwroty vs. VaR", xlab = "Szereg w okresie IN-SAMPLE", main = "Oszacowania 1% Value at Risk")
abline(h = 0, lty = 2)
lines(ins$Date, ins$VaR, type = "l", col = "green")
legend("topright", legend = c("Zwroty", "VaR"), col = c("red", "green"), lty = 1)

# W ilu przypadkach straty przekroczyły zakładany poziom VaR?
przekroczenia_VaR = sum(ins$total_r < ins$VaR) / length(ins$VaR)
przekroczenia_VaR


#### VaR w okresie OUT-OF-SAMPLE

# 1-dniowa prognoza warunkowego odchylenia standardowego
# plot(ugarchforecast(ins.garch11ma1, n.ahead = 1))
sigma.forecast = ugarchforecast(ins.garch01arma11mu0, n.ahead = 1)
sigma.forecast2 = sigma.forecast@forecast$sigmaFor[1, 1]

# Szacowanie 1-dniowej VaR dla całego okresu OUT-OF-SAMPLE
dane$obs = 1:length(dane$total_r)
start  = dane$obs[dane$Date == as.Date(dane$Date[length(dane$Date)-n_outs+1])]
finish = dane$obs[dane$Date == as.Date(dane$Date[length(dane$Date)])]
data2 = dane[start:finish, ]
VaR = rep(NA, times = finish - start)

for (k in start:finish) {
  tmp.data = dane[dane$obs <= k, ]
  tmp.data$rstd = (tmp.data$total_r - mean(tmp.data$total_r, na.rm = T)) /
    sd(tmp.data$total_r, na.rm = T)
  q01 = quantile(tmp.data$rstd, 0.01, na.rm = T)
  spec = ugarchspec(variance.model = list(model = "sGARCH",
                                          garchOrder = c(0, 1)),
                    mean.model = list(armaOrder = c(1, 1),
                                      include.mean = F),
                    distribution.model = "std")
  tmp.garch01arma11mu0 = ugarchfit(spec = spec, data = na.omit(tmp.data$total_r))
  sigma.forecast  = ugarchforecast(tmp.garch01arma11mu0, n.ahead = 1)
  sigma.forecast2 = sigma.forecast@forecast$sigmaFor[1, 1]
  VaR[k - start + 1] = q01 * sigma.forecast2
}
data2$VaR = VaR

# wykres zwrotów i VaR w okresie OUT-OF-SAMPLE
plot(data2$Date, data2$total_r, col = "red", lwd = 1, type = "l", ylim=c(-0.15, 0.15),
     ylab = "zwroty vs VaR", xlab = "Szereg w okresie OUT-OF-SAMPLE", main = "Oszacowania 1% Value at Risk")
abline(h = 0, lty = 2)
lines(data2$Date, data2$VaR, type = "l", col = "green")

# w ilu przypadkach straty przekroczyły zakładany poziom VaR?
sum(data2$total_r < data2$VaR) / length(data2$VaR)




