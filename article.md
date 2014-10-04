Анализ зависимости количества обращений пользователей от числа обслуживаемых рабочих мест
========================================================


Целью анализа является построение модели, выявлеющей закономерность в количестве обращений пользователей от числа обслуживаемых рабочих мест.

### Получение данных

Загружаем исходные данные

```r
SCandPC.raw<-readRDS("SC_and_PC.RDa")
# Dimension of dataset
dim(SCandPC.raw)
```

```
## [1] 642   5
```

```r
# First rows
head(SCandPC.raw)
```

```
##   Year Month   Org   PC  SC
## 1 2010     1 Org_1   34  33
## 2 2010     1 Org_2 1140 890
## 3 2010     1 Org_4    7  16
## 4 2010     1 Org_5  432 153
## 5 2010     1 Org_6    6   5
## 6 2010     1 Org_8  159  90
```

### Регрессионный анализ зависимости количества обращений пользователей от числа обслуживаемых рабочих мест как процесса

Проведем анализ только по всем предприятиям, для чего просуммируем данные по всем предприятиям для каждого периода и построим линейную модель КМНК.


```r
SCandPC.sum<-aggregate(cbind(PC,SC)~Month+Year,data=SCandPC.raw,sum)
dim(SCandPC.sum)
```

```
## [1] 52  4
```

```r
head(SCandPC.sum)
```

```
##   Month Year   PC   SC
## 1     1 2010 2552 1792
## 2     2 2010 2560 2139
## 3     3 2010 2577 2695
## 4     4 2010 2600 2464
## 5     5 2010 2606 1904
## 6     6 2010 2623 2140
```

```r
summary(lm0<-lm(SC~PC,data=SCandPC.sum))
```

```
## 
## Call:
## lm(formula = SC ~ PC, data = SCandPC.sum)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -964.2 -238.9   39.5  226.8 1450.5 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -708.884    579.648   -1.22     0.23    
## PC             1.203      0.185    6.50  3.6e-08 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 379 on 50 degrees of freedom
## Multiple R-squared:  0.458,	Adjusted R-squared:  0.447 
## F-statistic: 42.2 on 1 and 50 DF,  p-value: 3.65e-08
```

```r
oldpar<-par(mfrow=c(2,2))
plot(lm0)
```

![plot of chunk unnamed-chunk-2](./article_files/figure-html/unnamed-chunk-2.png) 

```r
par(oldpar)
```

Проведем тесты  на авторегерессию и гетероскедатичность


```r
library(car)
acf(residuals(lm0))
```

![plot of chunk unnamed-chunk-3](./article_files/figure-html/unnamed-chunk-3.png) 

```r
ncvTest(lm0)
```

```
## Non-constant Variance Score Test 
## Variance formula: ~ fitted.values 
## Chisquare = 0.6702    Df = 1     p = 0.413
```


Дисперсионный анализ зависимости остатков модели от времени


```r
y<-residuals(lm0)
x<-1:length(y)
summary(lm(y~x))
```

```
## 
## Call:
## lm(formula = y ~ x)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -1005.7  -245.8    28.5   200.4  1394.7 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)   -75.88     105.98   -0.72     0.48
## x               2.86       3.48    0.82     0.41
## 
## Residual standard error: 377 on 50 degrees of freedom
## Multiple R-squared:  0.0134,	Adjusted R-squared:  -0.00637 
## F-statistic: 0.677 on 1 and 50 DF,  p-value: 0.415
```

Иными словами, остатки модели - белый шум. Построим другую модель, которая учитывает только зависимость количества обращений пользователей от числа обслуживаемых рабочих мест и не зависит от времени.

### Построение предиктивной модели

Для начала разделим данные на два блока: данные для построения модели и данные для её верификации


```r
total.rows<-dim(SCandPC.raw)[1]
set.seed(118)
test.idx<-sample(1:total.rows,total.rows/4)
SCandPC.test<-SCandPC.raw[test.idx,]
dim(SCandPC.test)
```

```
## [1] 160   5
```

```r
SCandPC.mdl<-SCandPC.raw[-test.idx,]
dim(SCandPC.mdl)
```

```
## [1] 482   5
```

#### Модель без учета размера предприятия


```r
summary(lm1<-lm(SC~PC,data=SCandPC.mdl))
```

```
## 
## Call:
## lm(formula = SC ~ PC, data = SCandPC.mdl)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -481.0  -13.8    7.5   17.4  547.5 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  -8.5490     4.9585   -1.72    0.085 .  
## PC            1.0070     0.0109   92.17   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 92.4 on 480 degrees of freedom
## Multiple R-squared:  0.947,	Adjusted R-squared:  0.946 
## F-statistic: 8.5e+03 on 1 and 480 DF,  p-value: <2e-16
```

```r
confint(lm1)
```

```
##                2.5 % 97.5 %
## (Intercept) -18.2919  1.194
## PC            0.9855  1.028
```

Полученный результат хорошо согласуется с ранее опубликованной моделью. После исключения свободного члена получаем


```r
summary(lm1.1<-lm(SC~PC-1,data=SCandPC.mdl))
```

```
## 
## Call:
## lm(formula = SC ~ PC - 1, data = SCandPC.mdl)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -476.0  -20.4    0.0    9.1  550.4 
## 
## Coefficients:
##    Estimate Std. Error t value Pr(>|t|)    
## PC  0.99705    0.00929     107   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 92.6 on 481 degrees of freedom
## Multiple R-squared:  0.96,	Adjusted R-squared:  0.96 
## F-statistic: 1.15e+04 on 1 and 481 DF,  p-value: <2e-16
```

```r
confint(lm1.1)
```

```
##     2.5 % 97.5 %
## PC 0.9788  1.015
```

```r
sd(residuals(lm1.1))
```

```
## [1] 92.37
```

Графичеcки это выглядит следующим образом


```r
plot(SCandPC.mdl$SC~SCandPC.mdl$PC,xlab="Personal Computers, pcs", ylab="Service calls, pcs",main="Number of service calls by the number of computers",pch=19)
abline(lm1.1,col="red",lwd=2)
```

![plot of chunk unnamed-chunk-8](./article_files/figure-html/unnamed-chunk-8.png) 


#### Модель c учетом размеров предприятия

Как видно на предыдущем рисунке, имеются определенные провалы в данных. Для того, чтобы их избежать сгенерируем для построения модели случайные данные, равные сумме двух случаных событий.

Иными словами, симулируем определенное количество случайных событий для неких абстрактных холдингов, включающих в себя два случайных предприятия с известным количеством персональных компьютеров, равных их сумме и известным количеством обращений, также равным сумме обращений.


```r
set.seed(2014)
rows<-nrow(SCandPC.mdl)
iid1<-sample(1:rows,2000,replace=T)
iid2<-sample(1:rows,2000,replace=T)
 
test<-data.frame(PC=SCandPC.mdl$PC[iid1]+SCandPC.mdl$PC[iid2],SC=SCandPC.mdl$SC[iid1]+SCandPC.mdl$SC[iid2])
dim(test)
```

```
## [1] 2000    2
```
Снова определим коэффициенты линейной модели


```r
summary(lm1.2<-lm(SC~PC-1,data=test))
```

```
## 
## Call:
## lm(formula = SC ~ PC - 1, data = test)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -558.3  -51.6   -5.5   19.3  760.6 
## 
## Coefficients:
##    Estimate Std. Error t value Pr(>|t|)    
## PC  0.98765    0.00388     255   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 127 on 1999 degrees of freedom
## Multiple R-squared:  0.97,	Adjusted R-squared:  0.97 
## F-statistic: 6.49e+04 on 1 and 1999 DF,  p-value: <2e-16
```

```r
confint(lm1.2)
```

```
##    2.5 % 97.5 %
## PC  0.98 0.9952
```

Также предположим, что существуют три типа предприятий: малое, среднее и большое, для которых происходит качественное изменение в количестве обращений. Найдем эти границы сравнивая линейные модели по критерию Акаике.


```r
z.min<-c()
z.max<-c()
z.aic<-c()
for(i in seq(0,3000,by=50)) for(j in seq(50,2950,by=50)) {
  z.min<-c(z.min,i)  
  z.max<-c(z.max,j) 
  test$type<-"M"
  test$type[test$SC<i]<-"S"
  test$type[test$SC>j]<-"L"
  lmp<-lm(SC~PC+type-1,data=test)
  z.aic<-c(z.aic,AIC(lmp))
}
print(x1<-z.min[which(z.aic==min(z.aic))])
```

```
## [1] 700
```

```r
print(x2<-z.max[which(z.aic==min(z.aic))])
```

```
## [1] 1500
```

Построим результирующую линейную регрессию


```r
test$type<-"M"
test$type[test$PC<x1[1]]<-"S"
test$type[test$PC>x2[1]]<-"L"
summary(lmp<-lm(SC~PC+type-1,data=test))
```

```
## 
## Call:
## lm(formula = SC ~ PC + type - 1, data = test)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -569.7  -44.0    8.4   32.1  764.6 
## 
## Coefficients:
##       Estimate Std. Error t value Pr(>|t|)    
## PC      0.9566     0.0124   76.84   <2e-16 ***
## typeL  60.1808    25.2032    2.39   0.0170 *  
## typeM  45.8208    14.0696    3.26   0.0011 ** 
## typeS -12.6632     4.0550   -3.12   0.0018 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 126 on 1996 degrees of freedom
## Multiple R-squared:  0.971,	Adjusted R-squared:  0.971 
## F-statistic: 1.66e+04 on 4 and 1996 DF,  p-value: <2e-16
```

```r
confint(lmp) 
```

```
##          2.5 %  97.5 %
## PC      0.9322   0.981
## typeL  10.7534 109.608
## typeM  18.2282  73.414
## typeS -20.6158  -4.711
```

```r
plot(test$SC~test$PC,pch=19,col="grey",cex=0.5,xlab="Personal Computers, pcs", ylab="Service calls, pcs",main="Number of service calls by the number of computers")
points(SCandPC.mdl$SC~SCandPC.mdl$PC,pch=19)

abline(lm1.2,col="red",lwd=2)

fit1<-data.frame(PC<-1:3000)
fit1$type<-"M"
fit1$type[fit1$PC<x1[1]]<-"S"
fit1$type[fit1$PC>x2[1]]<-"L"
fit1$SC<-predict(lmp,newdata=fit1)
lines(fit1$PC,fit1$SC,col="blue")
```

![plot of chunk x1](./article_files/figure-html/x1.png) 

Проведем дисперсионный анализ моделей


```r
anova(lm1.2,lmp)
```

```
## Analysis of Variance Table
## 
## Model 1: SC ~ PC - 1
## Model 2: SC ~ PC + type - 1
##   Res.Df      RSS Df Sum of Sq    F  Pr(>F)    
## 1   1999 32388887                              
## 2   1996 31733306  3    655581 13.8 7.1e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

#### Многоуровневая модель

Подберем многоуровневую модель. Для начала построим предыдущую модель, а затем  проведем дисперсионный анализ моделей


```r
library(lme4)
```

```
## Loading required package: Matrix
## Loading required package: Rcpp
```

```r
summary(M0<-lmer(SC~PC+(1|type),data=test))
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: SC ~ PC + (1 | type)
##    Data: test
## 
## REML criterion at convergence: 25032
## 
## Scaled residuals: 
##    Min     1Q Median     3Q    Max 
## -4.519 -0.351  0.070  0.256  6.070 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  type     (Intercept)  1190     34.5   
##  Residual             15899    126.1   
## Number of obs: 2000, groups:  type, 3
## 
## Fixed effects:
##             Estimate Std. Error t value
## (Intercept)  23.8401    23.3199     1.0
## PC            0.9630     0.0115    83.5
## 
## Correlation of Fixed Effects:
##    (Intr)
## PC -0.487
```

```r
coef(M0)
```

```
## $type
##   (Intercept)    PC
## L       46.19 0.963
## M       38.79 0.963
## S      -13.46 0.963
## 
## attr(,"class")
## [1] "coef.mer"
```

```r
summary(M1<-lmer(SC~PC+(1+PC|type),data=test))
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: SC ~ PC + (1 + PC | type)
##    Data: test
## 
## REML criterion at convergence: 24862
## 
## Scaled residuals: 
##    Min     1Q Median     3Q    Max 
## -4.838 -0.276 -0.057  0.202  6.143 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev. Corr 
##  type     (Intercept) 1.48e+04 121.491       
##           PC          6.49e-02   0.255  -1.00
##  Residual             1.46e+04 120.629       
## Number of obs: 2000, groups:  type, 3
## 
## Fixed effects:
##             Estimate Std. Error t value
## (Intercept)  -78.414     70.371   -1.11
## PC             0.997      0.147    6.77
## 
## Correlation of Fixed Effects:
##    (Intr)
## PC -0.997
```

```r
coef(M1)
```

```
## $type
##   (Intercept)     PC
## L     -101.94 1.0464
## M     -148.94 1.1449
## S       15.65 0.7997
## 
## attr(,"class")
## [1] "coef.mer"
```

```r
plot(test$SC~test$PC,pch=19,col="grey",cex=0.5,xlab="Personal Computers, pcs", ylab="Service calls, pcs",main="Number of service calls by the number of computers")
points(SCandPC.mdl$SC~SCandPC.mdl$PC,pch=19)

points(test$PC,predict(M1),col="blue",pch=19,lwd=2)
abline(lm1.1,col="red",lwd=2)
```

![plot of chunk unnamed-chunk-13](./article_files/figure-html/unnamed-chunk-13.png) 

```r
anova(M0,M1)
```

```
## refitting model(s) with ML (instead of REML)
```

```
## Data: test
## Models:
## M0: SC ~ PC + (1 | type)
## M1: SC ~ PC + (1 + PC | type)
##    Df   AIC   BIC logLik deviance Chisq Chi Df Pr(>Chisq)    
## M0  4 25040 25062 -12516    25032                            
## M1  6 24877 24911 -12432    24865   167      2     <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

Как видно из расчетов, вторая модель оказывается лучше.
