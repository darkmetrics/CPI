# Поскольку работаем в облаке, 
# библиотеки приходится каждый раз ставить с нуля

#install.packages(c('tidyverse',
#                   'dplyr',
#                   'xlsx',
#                   'readxl',
#                   'zoo',
#                   'xts',
#                   'forecast',
#                   'ggplot2',
#                   'urca',
#                   'gridExtra',
#                   'Hmisc'))

library(tidyverse)
library(dplyr)
library(xlsx)
library(readxl)
library(zoo)
library(xts)
library(forecast)
library(ggplot2)
library(urca)
library(gridExtra)
library(Hmisc)

# Загрузим файл
cpi <- read.table('data/clean_data.csv', header = TRUE, sep=',')
cpi$month <- as.Date(cpi$month)
# отфильтруем значения до 2000 года - выкинем гиперинфляцию
cpi <- cpi[cpi$month>="2000-01-31", ]
glimpse(cpi)
view(cpi)

# посмотрим на графике, что все данные загрузились корректно
plot(cpi$month, cpi$foods*100, type='l', col='orange', 
     xlab = 'Месяц', ylab='Инфляция, %')
lines(cpi$month, cpi$nonfoods*100, type='l', col='green')
lines(cpi$month, cpi$services*100, type='l', col='royalblue')
legend('topright',
       cex = 0.75,
       legend = c('Прод. товары',
                  'Непрод. товары',
                  'Услуги'),
       fill = c('orange', 'green', 'royalblue'))


# посмотрим по-разному на ковариационную матрицу
cor(na.omit(cpi[ ,c('foods', 'nonfoods', 'services')]))
# естественно, товары между собой сильнее связаны, чем с услугами
# но всё равно корреляция заметная - может, нужна VAR?
vars <- diag(cov(as.matrix(na.omit(cpi[ ,c('foods', 'nonfoods', 'services')]))))

ggplot_df <- data.frame(names=names(vars^(1/2)), values=vars^(1/2)*100)
ggplot(ggplot_df, aes(x=names, y=values)) + geom_bar(stat = 'identity', fill='darkorange') +
  labs(x='Субиндекс', y='Ст. откл., %',
  title = 'Волатильность категорий 2000-2021')
# наибольшее значение у услуг - 1.2%, у продов 0.8%, у непродов 0.4%

# посмотрим на то же самое, но начиная с 2014
data_2014 <- na.omit(cpi[cpi$month>='2014-01-31', c('foods', 'nonfoods', 'services')])
cor(data_2014)
# корреляция между продами и непродами стала 2/3!
cov(data_2014)
ggplot_df <- data.frame(names=names(vars^(1/2)), 
                        values=diag(cov(data_2014))^(1/2)*100)
ggplot(ggplot_df, aes(x=names, y=values)) + geom_bar(stat = 'identity', fill='steelblue') +
  labs(x='Субиндекс', y='Ст. откл., %',
       title = 'Волатильность категорий 2000-2021')
barplot(diag(cov(data_2014)*100)^(1/2), 
        main='Выборочное ст. отклонение 2014-2021')
# картина сильно имзенилась: теперь 
# наибольшее значение у продов - 0.97%, у непродов 0.45%, у услуг 0.57%
# исходя из этого, модель, скорее всего, придётся оценивать и на всей выборке
# и на подвыборке 2014-2021 и сравнивать, потому что данных в подвыборке будет маловато
# но наибольший интерес для нас, особенно во втором случае, составляют продукты
# поскольку именно они вносят основной вклад в колебания ИПЦ сейчас


############################
# Модель для прод. товаров #
############################

# посмотрим на ряд
plot(cpi$month, cpi$foods*100, 
     type='l', 
     col='darkorange',
     main='Динамика цен на прод. товары',
     xlab='Месяц',
     ylab='Цены, %')

# здесь был самый сильный всплеск в 2015, без дамми не обойтись
# посмотрим на среднегодовую динамику
avgYearly <- function(ts, dates){
  ts <- na.omit(ts)
  years <- format(dates, '%Y')
  return(tapply(ts, years, mean))
}

sub_foods <- na.omit(cpi[ ,c('month', 'foods')])
years <- format(sub_foods$month, '%Y')
plot(unique(years), avgYearly(sub_foods$foods, sub_foods$month)*100, 
     type='l', 
     col='black',
     main='Динамика среднегодовых цен на услуги',
     xlab='Год',
     ylab='Цены, %')

# было три всплеска: в 2000, 2009, 2015

# посмотрим на PACF, ACF
ggtsdisplay(cpi$foods*100)
# можно предположить ARIMA((2,12) 0, (2,12)) или ARIMA((2,24) 0, (2,24))
# в начале виден небольшой тренд, проверим на стационарность
foods_adf <- ur.df(na.omit(cpi$foods), type='drift')
summary(foods_adf)
ndiffs(coredata(cpi$foods), test = 'kpss')
ndiffs(coredata(cpi$foods), test = 'adf')
# KPSS тест советует взять первую разность, ADF 2 - не брать разности 
# возможно, нестационарность связана с шоком 2015 года, и дамми это вылечит
# посмотрим на сезонность
ggseasonplot(ts(cpi$foods*100, 
                start = c(2000, 1), 
                end = c(2021, 12), 
                frequency = 12),
             main='Сезонность для прод. товаров',
             year.labels = FALSE)

# видно, что еда дороже всего под Новые год
# и дешевеет от января до августа, рост цен начинается в сентябре 

# перейдём к моделированию
foodAutoArima <- auto.arima(cpi$foods)
summary(foodAutoArima)
# Arima (4, 1, 3) 
# Train RMSE 2/3%, плохо
checkresiduals(foodAutoArima)
# распределение остатков ненормальное - это не страшно
# модель явно не уловила лаги 24 и 12
# не очень хорошо, что автокорреляция в остатках отвергается только на уровне 1%

# попробуем построить модель с сезонностью, допустим, ряд всё же стационарен
foodArima <- Arima(cpi$foods, 
                   order = c(2,0,2), 
                   seasonal = list("order" = c(1,0,1), "period" = 12))
summary(foodArima)
# Train RMSE 0.57%, уже лучше
# BIC, AIC улучшились 
checkresiduals(foodArima)
# p-value Ljung-Box = 0.376, уже лучше: в остатках нет автокорреляции
# по графику ошибки заметно, что наибольшие ошибки модели приходятся на 2000-2014
# за 2016-2021 ошибка минимальна
# максимум ошибки в 2015, надо создавать дамми
intervention = ts(c(
  rep(0, 178), 
  c(0.25,0.5,1,0.5,0.25), 
  rep(0,length(na.omit(cpi$foods))-178-5)
), 
start = c(2000, 1), 
frequency = 12)

# проверим на графике, что мы адекватно подобрали дамми
food_ts <- ts(na.omit(cpi$foods), start=c(2000,1), frequency = 12)
autoplot(intervention, series='Дамми', main='Продовольственная инфляция и дамми') +
  autolayer(food_ts*100, series='Инфляция')+
  xlab('Год')+ylab('Продуктовая инфляция, %') +
  guides(colour=guide_legend(title="Переменная"))+
  theme(plot.title = element_text(hjust = 0.5))

# теперь попробуем загнать дамми в модель
foodArimaD <- Arima(na.omit(cpi$foods), 
                   order = c(4,0,1), 
                   seasonal = list("order" = c(1,0,1), "period" = 12),
                   xreg=intervention)
summary(foodArimaD)
# RMSE 0.52%, не слишком хорошо
checkresiduals(foodArimaD)
# автокорреляции нет, распределение ошибок симметрично
# ПОПРОБОВАТЬ НА ПОДВЫБОРКЕ С 2014
# ETS для этого ряда значительно хуже, оно и не удивительно



##############################
# Модель для непрод. товаров #
##############################

# посмотрим на ряд
plot(cpi$month, cpi$nonfoods*100, 
     type='l', 
     col='darkgreen',
     main='Динамика цен на непрод. товары',
     xlab='Месяц',
     ylab='Цены, %')
# 1. начиная с 2005 года цены стабилизировались
# 2. цены на непрод. товары сильно выросли в 2021 году
# 3. нужна дамми для кризиса 2015 года



####################
# Модель для услуг #
####################

# посмотрим на ряд
plot(cpi$month, cpi$services*100, 
     type='l', 
     col='red',
     main='Динамика цен на услуги',
     xlab='Месяц',
     ylab='Цены, %')


# Ряд выводов по графику:
#   
# заметна выраженная сезонность
# видно, что волатильность цен на услуги всё время снижается
# возможно, стоит применить преобразование Бокса-Кокса
# кроме того, заметен тренд на снижение среднего роста цен вне зависимости от сезонности
# хотя небольшой рост инфляции наблюдался в 2014-15 годах, в целом не видится осмысленным использовать отдельную дамми в этом случае. С теоретической точки зрения, валютный шок скорее отразился в ценах на продовольственные и непродовольственные товары, чем в стоимости услуг.


# видно, что волатильность цен на услуги всё время снижается
# возможно, стоит применить преобразование Бокса-Кокса
# кроме того, заметен тренд на снижение среднего роста цен вне зависимости от сезонности 

sub <- na.omit(cpi[ ,c('month', 'services')])
plot(unique(years), avgYearly(sub$services, sub$month)*100, 
     type='l', 
     col='black',
     main='Динамика среднегодовых цен на услуги',
     xlab='Год',
     ylab='Цены, %')


# посмотрим на PACF, ACF
ggtsdisplay(cpi$services*100)
# можно предположить ARIMA((1,12) 0, 12)
# в начале виден небольшой тренд, проверим на стационарность
services_adf <- ur.df(na.omit(cpi$services), type='drift')
summary(services_adf)
ndiffs(coredata(cpi$services), test = 'kpss')
ndiffs(coredata(cpi$services), test = 'adf')
# KPSS тест советует взять первую разность, ADF 2 - не брать разности 

# посмотрим на сезонность
ggseasonplot(ts(cpi$services*100, 
                start = c(2000, 1), 
                end = c(2021, 12), 
                frequency = 12),
             main='Сезонность для услуг',
             year.labels = FALSE)

# видно, что услуги дорожают под Новый год и летом
# и дешевеют в первой половине осени

# перейдём к моделированию
servAutoArima <- auto.arima(cpi$services)
summary(servAutoArima)
# Arima (1, 1, 1) с дрифтом
# Train RMSE 1%, плохо
checkresiduals(servAutoArima)
# распределение остатков ненормальное - это не страшно
# модель явно плохо справляется с высокими значениями инфляции - 
# правый хвост распределения ошибко длиннее левого, и сильно
# не очень хорошо, что автокорреляция в остатках отвергается только на уровне 5%

# попробуем построить модель с сезонностью, допустим, ряд всё же стационарен
servArima <- Arima(cpi$services, 
                  order = c(1,0,1), 
                  seasonal = list("order" = c(1,0,1), "period" = 12))
summary(servArima)
# Train RMSE 0.7%, уже лучше
# BIC, AIC улучшились на 14%, неплохой результат
checkresiduals(servArima)
# p-value Ljung-Box = 2.8%, уже лучше: в остатках меньше автокорреляции 
# и радует, что распределение ошибок стало более симметричным
# хотя правый хвост всё равно больше левого
# по графику ошибки заметно, что наибольшие ошибки модели приходятся на 2000-2006
# за 2015-2021 ошибка минимальна

# 1. попробуем преобразование Бокса-Кокса
# по сути, это просто возведение ряда в разные степени
# вряд ли оно может помочь в нашей ситуации, но всё же
services_ts <- ts(cpi$services, 
                  start = c(2000, 1), 
                  end = c(2021, 12), 
                  frequency = 12)

simpleTrans <- function(ts){
  
  plot1 <-autoplot(sqrt(ts))+ ylab("")+xlab('Год')+
    ggtitle("Корень")+
    theme(plot.title = element_text(hjust = 0.5))
  plot2 <-autoplot(ts^(1/3))+ ylab("")+xlab('Год')+
    ggtitle("Кубический корень")+
    theme(plot.title = element_text(hjust = 0.5))
  plot3 <-autoplot(log(ts))+ ylab("")+xlab('Год')+
    ggtitle("Логарифм")+
    theme(plot.title = element_text(hjust = 0.5))
  plot4 <-autoplot(ts^(-1))+ ylab("")+xlab('Год')+
    ggtitle("Минус первая степень")+
    theme(plot.title = element_text(hjust = 0.5))
  
  #plotting plots together
  grid.arrange(plot1, plot2, plot3, plot4, nrow = 2)
}
  
simpleTrans(services_ts)

# ни одно из примитивных преобразований не подходит
# и даже при преобразовании заметно изменение хар-к процесса ряда после 2014

servicesBC <- BoxCox(na.omit(cpi$services), lambda = 'auto')
plot(cpi[!is.na(cpi$services), c('month')],
     servicesBC, 
     type='l', 
     col='red',
     main='Динамика цен на услуги - BoxCox',
     xlab='Месяц',
     ylab='Цены, %')
# применение Бокса-Кокса тоже не улучшает ситуацию
# Оптимальная лямбда 0.28, что близко к кубическому корню
# при этом снова видно изменение data generating process,
# на этот раз ближе к 2009 году


# 2. теперь попробуем построить модель на выборке с 2014 года
plot(cpi[cpi$month>='2014-01-31', c('month')],
     cpi[cpi$month>='2014-01-31', c('services')]*100, 
     type='l', 
     col='red',
     main='Динамика цен на услуги 2014-2021',
     xlab='Месяц',
     ylab='Цены, %')

services_2014 <- cpi[cpi$month>='2014-01-31', c('services')]
ndiffs(services_2014, test = 'adf')
ndiffs(services_2014, test = 'kpss')
ggtsdisplay(services_2014)
# можно предположить ARIMA((2,12) 0, (1,12))
servAutoArima14 <- auto.arima(services_2014)
summary(servAutoArima14)
# RMSE 0.4%, при этом информационные критерии ухудшились в три раза
# предлагается AR(5) модель, как ни удивительно, все коэффициенты значимы

servArima14 <- Arima(services_2014, 
                   order = c(2,0,1), 
                   seasonal = list("order" = c(1,0,1), "period" = 12))
summary(servArima14)
# Тут и ошибка 0.33% на train, и информационные критерии на 4.5% получше
checkresiduals(servArima14)
# ошибки распределены более-менее симметрично, но в данных осталась автокорреляция

# 3. попробуем ETS
servETS <- ets(na.omit(services_ts))
summary(servETS)
# ETS(A,Ad,A) 
# RMSE на train 0.7%, эту модель тоже надо попробовать при выборе итоговой спецификации
# посмотрим на декомпозицию ряда 
autoplot(servETS)
# распределение остатков симметричное, в данных есть автокорреляция
checkresiduals(servETS)

# построим ETS начиная с 2014 года, но думаю, качество будет не выше ARIMA
servETS14 <- ets(na.omit(window(services_ts, start=c(2014, 1))))
summary(servETS14)
# ETS(A,Ad,A) 
# RMSE на train 0.33%, эту модель тоже надо попробовать при выборе итоговой спецификации
# посмотрим на декомпозицию ряда 
autoplot(servETS14)
# распределение остатков симметричное, в данных есть автокорреляция
checkresiduals(servETS14)

# сравним ошибки для ARIMA и ETS на всей выборке
cbind('Forecast errors: ARIMA' = ts(na.omit(residuals(servArima)), start=c(2000, 1), frequency=12),
      'Forecast errors: ETS' = ts(na.omit(residuals(servArima)), start=c(2000, 1), frequency=12)) %>%
  autoplot(facet=TRUE) + xlab("Year") + ylab("")

# сравним ошибки на подвыборке с 2014 года
cbind('Forecast errors: ARIMA' = ts(na.omit(residuals(servArima14)), start=c(2014, 1), frequency=12),
      'Forecast errors: ETS' = residuals(servETS14,type='response')) %>%
  autoplot(facet=TRUE) + xlab("Year") + ylab("")
# у ETS ошибки даже ниже
# подумать над дамми, особенно для подвыборки с 2014