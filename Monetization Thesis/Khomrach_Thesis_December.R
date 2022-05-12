library(dplyr)
library(tidyr)
library(ggplot2)
library(qwraps2)
library(MASS)
library(gridExtra)
library(corrplot)
library(xtable)
library(DescTools)
library(fastDummies)
library(caret)
library(stargazer)
library(sandwich)
library(nnet)
library(ROCit)
library(reshape2)
### Загружаем наши данные
D <- read.csv("DF2.csv", sep=";", dec=",", header=TRUE)
### Проверяем их, нужно будет обязательно перевести все числа в числовые данные
head(D)
str(D)
D$Rating <- D$positive/D$negative
### Приступим к визуализации и дескриптивному анализу данных. Для начала - таблица, описывающая статистику:
my_summary <-
  list("Игроков онлайн (ccu)" =
         list("min"       = ~ min(ccu),
              "max"       = ~ max(ccu),
              "mean (sd)" = ~ qwraps2::mean_sd(ccu)
  ),
  "Медианное двухнедельное игровое время" =
    list("min"       = ~ min(median_2weeks),
         "max"       = ~ max(median_2weeks),
         "mean (sd)" = ~ qwraps2::mean_sd(median_2weeks)
    ),
  "Медианное общее игровое время" =
    list("min"       = ~ min(median_forever),
         "max"       = ~ max(median_forever),
         "mean (sd)" = ~ qwraps2::mean_sd(median_forever)
    ),
  "Рейтинг" =
    list("min"       = ~ min(Rating),
         "max"       = ~ max(Rating),
         "mean (sd)" = ~ qwraps2::mean_sd(Rating)
    )
  )
summary_by_genre <- summary_table(dplyr::group_by(D, Changing.The.Game), my_summary)
summary_by_genre1 <- summary_table(dplyr::group_by(D, MMORPG), my_summary)
summary_by_genre2 <- summary_table(dplyr::group_by(D, First.Person.Shooter), my_summary)
summary_by_genre3 <- summary_table(dplyr::group_by(D, Simulation), my_summary)
summary_by_genre4 <- summary_table(dplyr::group_by(D, Strategy), my_summary)
summary_by_genre1
summary_by_genre2
summary_by_genre3
summary_by_genre4

length(D$Seller_Rank_abs)
D$Revenue_Proxy <- 2/(1+D$Top.seller.rank)
str(D)
### Построим ящик с усами по нескольким жанрам:
D2 <- subset(D, MMORPG == 1 | Strategy == 1 | Simulation == 1 | Action == 1 | First.Person.Shooter == 1)
ggplot(D2, aes(x=genre, y=Rating, fill=genre)) + 
  geom_boxplot() + 
  scale_y_log10() +
  ggtitle("Распределения рейтинга по жанрам")
ggplot(D2, aes(x=genre, y=Revenue_Proxy, fill=genre)) + 
  geom_boxplot() + 
  scale_y_log10() +
  xlab("Жанр") +
  ylab("Прокси-выручка") +
  ggtitle("Распределение прокси выручки по жанрам") +
  scale_fill_discrete(name = "Жанры", labels = c("MMORPG", "Strategy", "Simulator", "Action", "FPS"))

### Влияет ли рейтинг на выручку видеоигр?
plot_11 <- ggplot(D2, aes(x=Rating, y=Revenue_Proxy, color = "red")) + 
  geom_point(alpha=1) + xlab("Соотношение отзывов") +
  ylab("Прокси-выручка") + ggtitle("Диаграмма разброса выручки и рейтинга")

plot_1 <-plot(data=D, log(Revenue_Proxy) ~ Rating, 
     main="Разброс выручки и рейтинга", 
     xlab="Соотношение положительных и отрицательных отзывов", 
     ylab = "Логарифм Прокси-выручки",
     pch=3, col=4)
### А игровое время?
plot_22 <- ggplot(D2, aes(x=ccu, y=Revenue_Proxy, color = "blue")) + 
  geom_point(alpha=1) + xlab("Медианное игровое время") +
  ylab("Прокси-выручка")+ scale_x_log10() + ggtitle("Диаграмма разброса выручки и количества игроков онлайн")
plot_2 <-plot(data=D, log(Revenue_Proxy) ~ log(ccu), 
              main="Разброс выручки и рейтинга", 
              xlab="Логарифм количества игроков онлайн", 
              ylab = "Логарифм прокси-выручки",
              pch=3, col=2)
### Каковы в целом соотношения влияющих факторов?
D3 <- cbind(D$Revenue_Proxy, D$median_forever, D$ccu, D$Rating, D$Infinite.Money.Hole, D$But.First...You.ll.Need.A.Contract, D$Time.Is.Money, D$Horse.Armor, D$Changing.The.Game, D$Expansive.Expansions, D$It.s.Not.Gambling..We.Swear)
colnames(D3) <- c("Revenue_Proxy", "median_forever", "ccu", "Rating", "Infinite.Money.Hole", "Contract", "Pay-to-Skip", "Horse_Armor", "Altering_Gameplay", "Expansions", "Lootboxes")
D3 <- as.data.frame(D3)
corrplot(cor(D3, use = "complete.obs"))
c3 <- cor(D3, use = "complete.obs")
print(xtable(c3), type="latex")
write.csv(c3, "c.csv")
### Наконец, логистическая регрессия:
Y <- D$class_percentile
D3$MMORPG <- D$MMORPG
D3$Strategy <- D$Strategy
D3$FPS <- D$First.Person.Shooter
D3$Simulator <- D$Simulation
D3$Y <- Y
D3$Y <- as.factor(D3$Y)
D3$lccu <- log(D3$ccu)
D3$lccu[is.infinite(D3$lccu)] <- 0
D3$Rating_sq <- (D3$Rating)^2
m <- polr(Y ~ lccu + Rating + Rating_sq + FPS*`Pay-to-Skip` + FPS*Horse_Armor + Simulator*`Pay-to-Skip` + Simulator*Horse_Armor + MMORPG*`Pay-to-Skip` + MMORPG*Horse_Armor + Strategy*`Pay-to-Skip` + Strategy*Horse_Armor, D3, Hess=TRUE)
str(D3)
(D3$Rating)^2
summary(m)
### Построим регрессию только с константами
oim <- polr(Y ~ 1, D3)
summary(oim)
### Рассчитаем псевдо-R-квадраты
R_squared <- PseudoR2(m, which = c("CoxSnell","Nagelkerke","McFadden"))
R_squared
R_squared1 <- PseudoR2(m1, which = c("CoxSnell","Nagelkerke","McFadden"))
R_squared2 <- PseudoR2(m2, which = c("CoxSnell","Nagelkerke","McFadden"))
R_squared3 <- PseudoR2(m3, which = c("CoxSnell","Nagelkerke","McFadden"))
R_squared4 <- PseudoR2(m4, which = c("CoxSnell","Nagelkerke","McFadden"))
R_squared1[3]
### Оценим точность модели:
m$fitted.values
predict(m)
chisq.test(D3$Y,predict(m))
chisq.test(D3$Y,predict(m2))
Confusion <- confusionMatrix(D3$Y,predict(m))
summary_con <- Confusion$overall
summary_con
### Значимость коэффициентов?
OLRestimates <- coef(summary(m))
p <- pnorm(abs(OLRestimates[, "t value"]), lower.tail = FALSE) * 2
OLRestimates_P <- cbind(OLRestimates, "p value" = p)
OLRestimates_P
write.csv(OLRestimates_P, file = "regression_results.csv", row.names=TRUE)
cse = function(reg) {
  rob = sqrt(diag(vcovHC(reg, type = "HC0")))
  return(rob)  }
stargazer(m, title="Модель позиции в рейтинге по продажам", digits=3, type="text", out = "table_MB_3.html", dep.var.caption = "Позиция в рейтинге по выручке")
### Пробуем биномиальные регрессии
D3$Top1perc <- D$X1.
D3$Top5perc <- D$X5.
D3$Top10perc <- D$X10.
D3$Top25perc <- D$X25.
m1 <- glm(Top1perc ~ lccu + Rating + Rating_sq + FPS*`Pay-to-Skip` + FPS*Horse_Armor + Simulator*`Pay-to-Skip` + Simulator*Horse_Armor + MMORPG*`Pay-to-Skip` + MMORPG*Horse_Armor + Strategy*`Pay-to-Skip` + Strategy*Horse_Armor, D3, family = binomial)
m2 <- glm(Top5perc ~ lccu + Rating + Rating_sq + FPS*`Pay-to-Skip` + FPS*Horse_Armor + Simulator*`Pay-to-Skip` + Simulator*Horse_Armor + MMORPG*`Pay-to-Skip` + MMORPG*Horse_Armor + Strategy*`Pay-to-Skip` + Strategy*Horse_Armor, D3, family = binomial)
m3 <- glm(Top10perc ~ lccu + Rating + Rating_sq + FPS*`Pay-to-Skip` + FPS*Horse_Armor + Simulator*`Pay-to-Skip` + Simulator*Horse_Armor + MMORPG*`Pay-to-Skip` + MMORPG*Horse_Armor + Strategy*`Pay-to-Skip` + Strategy*Horse_Armor, D3, family = binomial) 
m4 <- glm(Top25perc ~ lccu + Rating + Rating_sq + FPS*`Pay-to-Skip` + FPS*Horse_Armor + Simulator*`Pay-to-Skip` + Simulator*Horse_Armor + MMORPG*`Pay-to-Skip` + MMORPG*Horse_Armor + Strategy*`Pay-to-Skip` + Strategy*Horse_Armor, D3, family = binomial)
stargazer(m, m1, m2, m3, m4, add.lines = list(c("Pseudo R-squared", R_squared[3], R_squared1[3], R_squared2[3], R_squared3[3], R_squared4[3]), digits = 3), title="Модель позиции в рейтинге по продажам", digits=3, type="text", out = "table_MB_4.html", dep.var.caption = "Позиция в рейтинге по выручке")


##### Мобилки

D <- read.csv("APPMagic.csv", sep=";", dec=",", header=TRUE)

my_summary <-
  list("Прокси-DAU" =
         list("min"       = ~ min(est_DAU),
              "max"       = ~ max(est_DAU),
              "mean (sd)" = ~ qwraps2::mean_sd(est_DAU)
         ),
       "LTV на западных рынках" =
         list("min"       = ~ min(LTV.West),
              "max"       = ~ max(LTV.West),
              "mean (sd)" = ~ qwraps2::mean_sd(LTV.West)
         ),
       "LTV на азиатских рынках" =
         list("min"       = ~ min(LTV.East),
              "max"       = ~ max(LTV.East),
              "mean (sd)" = ~ qwraps2::mean_sd(LTV.East)
         ),
       "ROI" =
         list("min"       = ~ min(ROI),
              "max"       = ~ max(ROI),
              "mean (sd)" = ~ qwraps2::mean_sd(ROI)
         )
  )
summary_by_genre <- summary_table(dplyr::group_by(D, sell_ad), my_summary)
summary_by_genre
### LTV по регионам
LTVS <- as.data.frame(cbind(D$LTV.West, D$LTV.East))
colnames(LTVS) <- c("LTV-Запад", "LTV-Восток")
LTVS %>% 
  pivot_longer(`LTV-Запад`:`LTV-Восток`, 
               names_to = "Name", 
               values_to = "value") %>% 
  ggplot(aes(x = value, 
             fill = Name)) + geom_density(alpha = 0.2) + labs(title = "Распределения LTV по рынкам", caption = "(Источник: AppMagic)", x =  "Пожизненная ценность игрока", y = "Плотность", fill = "Категория")
str(LTVS)
LTVS$`LTV-Запад` <- as.numeric(LTVS$`LTV-Запад`)
LTVS$`LTV-Восток` <- as.numeric(LTVS$`LTV-Восток`)
### Все в числовые
asNumeric <- function(x) as.numeric(x)
chrNumeric <- function(d) modifyList(d, lapply(d[, sapply(d, is.character)], asNumeric))
Names <- D$п.їgame
D <- cbind(Names, chrNumeric(D))
DM <- melt(D, measure.vars=13:18)
ggplot(DM[DM$value == 1, ], aes(x=variable, y=ROI, fill=variable)) + 
  geom_boxplot() + 
  scale_y_log10() + labs(title = "Размах значений ROI по жанрам", caption = "(Источник: AppMagic, ВШЭ, авторские расчёты)", x =  "Жанр", y = "Пожизненная ценность игрока", fill = "Жанры")
str(D)
### Извлекаем ковариаты:
D3 <- cbind(D$Names, D$sell_ad, D$in_app_purchases, D$est_DAU, D$asian_share, D$sports, D$strategy, D$action, D$rpg, D$casual, D$casino, D$ROI)
D3 <- as.data.frame(D3)
DM <- DM[DM$value == 1, ]
DM$est_DAU <- as.numeric(DM$est_DAU)
### Линейная регрессия:
m <- lm(ROI ~ log(est_DAU) + asian_share + sell_ad*variable + in_app_purchases*variable, DM)
m1 <- lm(ROI ~ log(est_DAU) + asian_share + sell_ad + in_app_purchases + variable, DM)
summary(m)
summary(m1)
stargazer(m, m1, digits = 3, se=list(cse(m),cse(m1)), title="Модель регрессии ROI на факторы монетизации", type="html", out = "table_MB_S.html", dep.var.caption = "Показатель ROI")
anova(m1, m)
