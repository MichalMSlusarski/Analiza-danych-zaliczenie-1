library(tidyverse)
library(rmarkdown)
library(knitr)
library(vioplot)

#Załaduj zbiór danych, podając nazwę pliku, czy zawiera nagłówki oraz typ separatora

dane_all <- read.csv('bulbs.csv', T, sep = ';')

#Główne informacje o danych w kolumnach

summary(dane_all)
plot(dane$time_in_hours, xlab = 'Indeks obserwacji', ylab = 'Czas działania żarówki [h]')
plot(dane_all$price_in_PLN, xlab = 'Indeks obserwacji', ylab = 'Czas działania żarówki [h]')
plot(dane$time_in_hours, xlab = 'Indeks obserwacji', ylab = 'Czas działania żarówki [h]')
plot(dane$time_in_hours, xlab = 'Indeks obserwacji', ylab = 'Czas działania żarówki [h]')

info_table <- summarise_all(dane)
cena_preparatu <- dane[]
dane <- dane[dane$product_type != 'CoatItYourself']

check_if_any_NA <- function(data_set) {
  
  val <- 0 #empty vector to store column indexes
  
  for (i in ncol(data_set)) {
    print(data_set[i,])
    if(anyNA(data_set[,i])) {
      append(val, i, after = length(val + 1))
      #print(val)
    }
    else {
      #print(val)
      next
    }
  }
  
  return(val)
}

qplot(seq_along(dane$time_in_hours.Length), dane$time_in_hours)

ggplot(dane, aes(dane$time_in_hours, dane$price_in_PLN) + geom_point() + geom_smooth(method="lm"))

# Szczegółowe pytania, które stawia przed Tobą redaktor naczelna, są następujące:
#   
# Czy rzeczywiście żarówki o podwójnej bańce mają dłuższy średni czas życia niż żarówki o pojedynczej bańce?

regular_bulbs_no_spray <- subset(regular_bulbs, regular_bulbs$sprayed == "none")
double_bulbs_no_spray <- subset(double_bulbs, regular_bulbs$sprayed == "none")

mean(regular_bulbs_no_spray$time_in_hours)
mean(double_bulbs_no_spray$time_in_hours)

# Czy rzeczywiście spray przedłuża średni czas życia żarówki? Obu typów?

regular_bulbs_sprayed <- setdiff(regular_bulbs, regular_bulbs_no_spray)
double_bulbs_sprayed <- setdiff(double_bulbs, double_bulbs_no_spray)

var(regular_bulbs_sprayed$time_in_hours)
var(regular_bulbs_no_spray$time_in_hours)
mean(double_bulbs_sprayed$time_in_hours)

t_test_regular_bulbs <- t.test(regular_bulbs_sprayed$time_in_hours, regular_bulbs_no_spray$time_in_hour, alternative='less', var.equal=F, conf.level = .95)
  #H0 - średnia sprayowana jest większa od średniej niesprayowanej, H1 - średnia sprayowana NIE jest...
t_test_double_bulbs <- t.test(double_bulbs_sprayed$time_in_hours, double_bulbs_no_spray$time_in_hour, alternative='less', var.equal=F, conf.level = .95)
  #H0 - średnia sprayowana jest większa od średniej niesprayowanej, H1 - średnia sprayowana NIE jest...
boxplot(regular_bulbs_no_spray$time_in_hours, 
        regular_bulbs_sprayed$time_in_hours, 
        double_bulbs_no_spray$time_in_hours, 
        double_bulbs_sprayed$time_in_hours,
        xlab = 'Typ żarówek',
        ylab = 'Czas działania [h]',
        varwidth = F,
        outline = F,
        main = 'Porównanie typów żarówek',
        at = c(1,2,3,4),
        names = c('Zwykła', 'Zwykła - spray', 'Podwójna', 'Podwójna - spray'),
        col = c('lightblue', 'lightgreen', 'yellow', 'orange')
        )

# Czy używanie którejś z tych alternatyw do zwykłych żarówek o pojedynczej bańce ma sens ekonomiczny? Uzasadnij swoją odpowiedź.



# Może mógłbyś/mogłabyś zaproponować producentowi jakieś zmiany, dzięki którym wzrośnie użyteczność oraz ekonomiczny sens stosowania sprayu?


# Dla każdej żarówki policz, jaka jest cena jednej godziny jej użytkowania. Dla żarówek sprayowanych co godzinę, dolicz cenę jednego psiku preparatem (żarówki należy psikać co godzinę).

dane$price_per_hour <- NA
dane$price_per_hour <- dane$price_in_PLN / dane$time_in_hours
dane$price_per_hour <- round(dane$price_per_hour, digits = 4)
dane$price_per_hour <- ifelse(dane$sprayed == "CoatItYourself", dane$price_per_hour + 100, dane$price_per_hour)

# Porównując sens ekonomiczny użytkowania różnych żarówek, warto porównywać ze sobą średnią cenę jednej godziny ich użytkowania.
unique(dane$product_type)
regular_bulbs <- subset(dane, dane$product_type == 'regular bulb')
double_bulbs <- subset(dane, dane$product_type == 'DoubleBulb')
mean(double_bulbs$price_per_hour)
mean(regular_bulbs$price_per_hour)
boxplot(double_bulbs$price_per_hour, regular_bulbs$price_per_hour)
vioplot(double_bulbs$price_per_hour, regular_bulbs$price_per_hour)

# Zakładając dzienne użycie żarówki 4 godziny/dobę, policz na ile lat średnio starcza jedna żarówka.

av_bulb_usage <- 4

how_long_bulb_lasts <- mean(dane$time_in_hours) / av_bulb_usage

# Przyjmij pewne założenia dotyczące liczby żarówek w gospodarstwie domowym. Policz, na jak długo starczy w tym modelowym gospodarstwie domowym jedno opakowanie sprayu.

av_bulbs_per_household <- 67 #dane z artykułu https://www.fortnightly.com/fortnightly/2015/12-0/how-many-lights-home-energy-dept-counted
number_of_bulbs_per_1_box <- 200 #wynika z treści zadania

time_it_takes_to_empty_1_box <- number_of_bulbs_per_1_box / av_bulbs_per_household
#starczy na niecałe 3 godziny

