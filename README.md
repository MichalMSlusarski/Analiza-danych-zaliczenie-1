## Praca zaliczeniowa z przedmiotu Analiza Danych w R.
### Analiza danych dot. efektywności różnych typów żarówek

Przed przystąpieniem do utworzenia niniejszego dokumentu załadowane zostały następujące
biblioteki (może być wymagana instalacja):

```{r biblio, eval=T, message = F, results='hide'}
  library(dplyr)
  library(vioplot)
  library(rmarkdown)
  library(knitr)
```

Biblioteka `dplyr` zawiera przydatne funkcje ułatwiające pracę na zbiorach danych. `vioplot` w łatwy sposób pozwala tworzyć wykresy skrzypcowe. Biblioteki `rmarkdown` i `knitr` posłużyły do stworzenia niniejszego dokumentu i jego wersji html.

Aby załadować dane z pliku 'bulbs.csv', wykorzystano następującą funkcję. W argumentach podano, że plik ma nagłówki oraz określono rodzaj separatora - ';'.

```{r ladowanie_danych, eval=T, results='hide'}
  bulbs <- read.csv('bulbs.csv', header = T, sep = ';')
```

### Sprawdzenie poprawności danych

Przed przystąpieniem do analizy, dokonano sprawdzenia zbioru pod kątem jego zawartości.

```{r}
  summary(bulbs)
```

Używając funkcji `summary` dostrzec można, że w zestawie danych znajdują się dwie kolumny z danymi numerycznymi i trzy kolumny zawierające zmienne typu `char`. Liczba kolumn odpowiada liczbie zmiennych, liczba obserwacji to 401. 

Z treści zadania wynika, że w zbiorze danych znajduje się cena preparatu *CoatItYourself*. Wiadomo również, że przetestowanych zostało 400 żarówek, a obserwacji jest 401. Biorąc pod uwagę te informacje, można podjąć próbę ekstrakcji ceny preparatu ze zbioru:

*Uwaga techniczna: Poniższe kroki są robione nieco 'na wyrost'. Dla jednego elementu, można by po prostu znaleźć i przepisać cenę sprayu manualnie.*

```{r}
  unique(bulbs$product_type)
```

Z pomocą powyższej funkcji zidentyfikowano typy produktów w zbiorze, a co najważniejsze to **w jaki sposób są one zapisane**, by nie popełnić błędu w pisowni. Nazwa preparatu zapisana jest jako `CoatItYourself` - w formacie *PascalCase*.

```{r ekstrkcja_2, eval=T}
  COAT_IT_PRICE <- bulbs$price_in_PLN[bulbs$product_type == 'CoatItYourself']
  bulbs <- subset(bulbs, product_type != 'CoatItYourself')
```

Cena preparatu przypisana została do `COAT_IT_PRICE`. Zapis wielką literą to sposób na oznaczenie stałej na zasadzie konwencji. Następnie zmodyfikowano zbiór za pomocą funkcji `subset` pozbywając się produktu `CoatItYourself`. W analizie korzystano głównie z notacji **$** w celu dostępu do zawartości kolumn. Wydaje się ona być zwięzła i przejrzysta.

W kolejnym korku, dokonano sprawdzenia występowania wśród obserwacji wartości typu `NaN`. Wykorzystana została funkcja `apply` za argument przyjmująca zbiór danych `bulbs`, margines (2) oraz funkcję `anyNA` - zwracającą wartość `TRUE`, jeśli w kolumnie znajduje się wartość `NaN`. Użycie funkcji `apply` pozwala zbadać wszystkie kolumny jednocześnie.

```{r}
  apply(bulbs, 2, anyNA)
```

Jak wskazują zwrócone wartości, tylko w jednej kolumnie można znaleźć brakujące obserwacje - `manufacture_plant`. Szczęśliwie, wartości zawarte w tej kolumnie nie są konieczne do odpowiedzenia na postawione w analizie pytania. Można zignorować zaistniałe braki.

Ostatnim etapem pre-procesowania zbioru było sprawdzenie go pod kątem obserwacji odstających. Najprostszym sposobem ich znalezienia jest ponowne wywołanie funkcji `summary`.

```{r}
  summary(bulbs)
```

Analiza powyższych danych pokazuje, że w kolumnie `time_in_hours` istnieją obserwacje wyraźnie odbiegające od reszty. Wskazuje na to nie tylko różnica między średnią `Mean` a wartością maksymalną `Max`. Zwraca uwagę przede wszystkim znaczna (ponad 7 krotna) różnica między 3. kwartylem a wartością maksymalną, w porównaniu do różnic między 1. kwartylem a medianą (kwartylem 2 rzędu) i 3. kwartylem.

Zobrazowaniu tej obserwacji posłużyć może wykres kropkowy:
```{r}
  plot(bulbs$time_in_hours, main = 'Czas działania żarówek', ylab = 'Czas działania [h]')
```

Wykres wskazuje wyraźnie na pojedynczą, znacznie odstającą obserwację - równą 31870 godzin. Wartość ta jest najprawdopodobniej błędem, ponieważ pomiar trwać musiałby ponad 1327 dni (zakładając ciągłe użytkowanie żarówki) lub ponad 21 lat zakładając użytkowanie 4 godziny dziennie (jak w zadaniu). Obserwacja została usunięta ze zbioru.

```{r}
  bulbs <- subset(bulbs, bulbs$time_in_hours < 30000)
```

### Odpowiedzi na pytania redaktor naczelnej

Po zakończeniu przygotowania danych, przystąpiono do analizy. W celu ułatwienia pracy z danymi, utworzono przydatne podzbiory:

```{r}
  regular_bulbs <- subset(bulbs, bulbs$product_type == 'regular bulb') #zwykłe żarówki
  double_bulbs <- subset(bulbs, bulbs$product_type == 'DoubleBulb') #podwójne żarówki
  
  regular_bulbs_no_spray <- subset(regular_bulbs, regular_bulbs$sprayed == "none") #zwykłe bez sprayu
  double_bulbs_no_spray <- subset(double_bulbs, regular_bulbs$sprayed == "none") #podwójne bez sprayu
  
  regular_bulbs_sprayed <- setdiff(regular_bulbs, regular_bulbs_no_spray) #zwykłe sprayowane
  double_bulbs_sprayed <- setdiff(double_bulbs, double_bulbs_no_spray) #podwójne sprayowane
```

Żarówki podzielono według wszystkich badanych typów. Ponownie wykorzystana została funkcja `subset` oraz `setdiff` z pakietu `dplyr`. `Setdiff` odszukuje wiersze, które pojawiają się w pierwszej ramce danych lub tabeli, ale nie w drugiej i na tej podstawie tworzy ramkę danych.

#### Czy rzeczywiście żarówki o podwójnej bańce mają dłuższy średni czas życia niż żarówki o pojedynczej bańce?

Porównanie średnich rozpoczęto od wizualizacji danych za pomocą wykresów skrzypcowych, z naniesionymi dodatkowo punktami wyznaczającymi średnie wartości dla obu zbiorów. Wywołana funkcja `vioplot` przyjmuje szereg argumentów, z czego dwa najważniejsze to zadane kolumny - reszta ma charakter estetyczny. Za pomocą funkcji `points` dodano punkty reprezentujące średnie.

```{r}
  vioplot(regular_bulbs$time_in_hours, double_bulbs$time_in_hours,
          xlab = 'Typ żarówek',
          ylab = 'Czas działania [h]',
          main = 'Porównanie typów żarówek',
          at = c(1,2),
          names = c('Zwykła', 'Podwójna'),
          col = c('lightblue', 'lightgreen'))
  points(1, mean(regular_bulbs$time_in_hours), col="red", pch = 19)
  points(2, mean(double_bulbs$time_in_hours), col="red", pch = 19)
```

 <img src="1.jpg" alt="" width="300" height="300"> 

Wizualna reprezentacja wskazuje, że średnia czasu działania żarówek podwójnych jest wyższa niż żarówek zwykłych. Jednakże, aby móc stwierdzić, że jest to różnica istotna statystycznie, należy przeprowadzić test. Przed wybraniem testu równości średnich, posłużono się testem Shapiro-Wika, celem ustalenia, czy dane mają rozkład normalny:

```{r}
  t1 <- shapiro.test(regular_bulbs$time_in_hours)
  t2 <- shapiro.test(double_bulbs$time_in_hours)
  print(paste("Uzyskano p-wartość:", t1$p.value))
  print(paste("Uzyskano p-wartość:", t2$p.value))
```

Uzyskane wyniki `p-value` pozwalają stwierdzić, że dla poziomu istotności $\alpha = 0.05$ należy odrzucić hipotezę zerową o normalności rozkładów w obydwu przypadkach. Skłania to do użycia testu nieparametrycznego - np. testu Wilcoxona. W `R` jest on realizowany przez funkcję `wilcox.test`.

Na podstawie danych wizualnych, postawiono hipotezę zerową $H_0$: *średnia czasu świecenia żarówek podwójnych **nie** jest większa od średniej czasu świecenia żarówek zwykłych*. Poziom istotności $\alpha = 0.05$. Dodatkowo za pomocą warunku `if` porównano wynik z przyjętą wartością $\alpha$, aby za pomocą funkcji `print` zwrócić wynik z opisem. Zabieg ten powtórzono dla wszystkich późniejszych testów porównawczych. Aby nie powtarzać zbyt często tych samych części kodu, stworzono prostą funkcję `describe` przyjmującą za argument wynik testu. Założono dla ułatwienia, że wartość $\alpha$ we wszystkich testach pozostanie na poziomie 0.05 (domyślny w `R`). 

```{r}
  w1 <- wilcox.test(regular_bulbs_sprayed$time_in_hours, regular_bulbs_no_spray$time_in_hour, alternative='greater', conf.level = .95)
  
  describe <- function(result) {
    if(result$p.value >= 0.05) {
      print(paste("Uzyskano p-wartość:", result$p.value, 'hipoteza zerowa nie została odrzucona'))
    } else {
      print(paste("Uzyskano p-wartość:", result$p.value, 'hipoteza zerowa została odrzucona, przyjęto hipotezę alternatywną'))
    }
  }
  describe(w1)
```

Wynik testu - p-wartość < $\alpha = 0.05$ - każe odrzucić hipotezę zerową i stwierdzić, że różnica wielkości średnich jest istotna statystycznie. Można zakładać, że **żarówki o podwójej bańce świecą średnio dłużej, niż zwykłe**.


#### Czy rzeczywiście spray przedłuża średni czas życia żarówki? Obu typów?

Analogicznie do poprzedniego pytania, tu też analizę poprzedza wizualizacja danych. Jako, że badanych kategorii jest więcej, zostały zebrane w listę `cat_list`, w celu oszczędzenia czasu na ich przepisywanie i uniknięcia ewentualnych pomyłek. Za pomocą funkcji `lapply` przypisano do zmiennej pomocniczej `means` wartości średnich dla wszystkich kategorii. Następnie uzupełniono nimi wykres wykorzystując pętlę `for`.

```{r}
  cat_list <- list(regular_bulbs_no_spray$time_in_hours, 
        regular_bulbs_sprayed$time_in_hours, 
        double_bulbs_no_spray$time_in_hours, 
        double_bulbs_sprayed$time_in_hours)
  vioplot(cat_list,
          xlab = 'Typ żarówek',
          ylab = 'Czas działania [h]',
          main = 'Porównanie podtypów żarówek',
          at = c(1,2,3,4),
          names = c('Zwykła', 'Zwykła + spray', 'Podwójna', 'Podwójna + spray'),
          col = c('lightblue', 'lightgreen', 'yellow', 'orange'))
  means <- lapply(cat_list, mean)
  
  for (i in 1:4) {
    points(i, means[[i]], col = 'red', pch = 19)
  }
  
```

Przystąpiono do testowania hipotezy $H_0$ o normalności rozkładów testem Shapiro-Wilka ($\alpha = 0.05$):

```{r}
  for (i in 1:4) {
    res <- shapiro.test(cat_list[[i]])
    describe(res)
  }
  #funkcji describe można użyć także w przypadku testów normalności
```

Uzyskane p-wartości pozwalają odrzucić hipotezę o normalności rozkładów dla żarówek zwykłych pokrytych sprayem oraz podwójnych niepokrytych. Oznacza to, że porównywane są średnie pochodzące i niepochodzące z rozkładów normalnych. Tu również skorzystano z testu nieparametrycznego, przyjmując taki sam poziom ufności $\alpha = 0.05$.

```{r}
  w2 <- wilcox.test(regular_bulbs_no_spray$time_in_hours, regular_bulbs_sprayed$time_in_hours, alternative='greater', conf.level = .95)
  w3 <- wilcox.test(double_bulbs_no_spray$time_in_hours, double_bulbs_sprayed$time_in_hours, alternative='greater', conf.level = .95)
  
  describe(w2)
  describe(w3)
```

Wynik obydwu testów, gdzie p-wartość > $\alpha = 0.05$, pozwolił przyjąć hipotezę zerową - **średnia po zastosowaniu sprayu *nie* jest istotnie większa!** Podważa to ekonomiczny sens wykorzystania sprayu z perspektywy klienta.

#### Czy używanie którejś z tych alternatyw do zwykłych żarówek o pojedynczej bańce ma sens ekonomiczny? Uzasadnij swoją odpowiedź.

Przed odpowiedzeniem na pytanie o sens ekonomiczny, podjęto się odpowiedzi na pytania pomocnicze. 

*Zakładając dzienne użycie żarówki 4 godziny/dobę, policz na ile lat średnio starcza jedna żarówka.*

```{r}
  av_bulb_usage <- 4
  how_long_bulb_lasts_years <- mean(bulbs$time_in_hours) / av_bulb_usage
  how_long_bulb_lasts_years <- how_long_bulb_lasts_years / 365
  how_long_bulb_lasts_years
```

Żarówka starcza średnio na 2.51 roku użytkowania.

*Przyjmij pewne założenia dotyczące liczby żarówek w gospodarstwie domowym. Policz, na jak długo starczy w tym modelowym       gospodarstwie domowym jedno opakowanie sprayu.*

```{r}
  av_bulbs_per_household <- 67 #dane z artykułu   https://www.fortnightly.com/fortnightly/2015/12-0/how-many-lights-home-energy-dept-counted
  number_of_bulbs_per_1_box <- 200 #wynika z treści zadania
  hours_it_takes_to_empty_1_box <- number_of_bulbs_per_1_box / av_bulbs_per_household
  hours_it_takes_to_empty_1_box
```

Jedno opakowanie sprayu starczy na nie więcej niż 3 godziny użytkowania. Do codziennego stosowania nie wystarczy jedno opakowanie sprayu. To kolejny argument na jego niekorzyść.

*Dla każdej żarówki policz, jaka jest cena jednej godziny jej użytkowania. Dla żarówek sprayowanych co godzinę, dolicz cenę jednego psiku preparatem (żarówki należy psikać co godzinę).*

Dla zbioru głównego `bulbs` utworzona została nowa kolumna `price_per_hour`, do której przypisano stosunek ceny do czasu użytkowania. Korzystając z notacji *$*, bez wskazania pozycji, `R` automatycznie utworzy nową kolumnę z prawej strony. Wyniki zaokrąglono, przy użyciu funkcji `round`. Funkcja `ifelse` jako argument przyjmuje warunek logiczny, na podstawie którego wykonuje działanie (tutaj dodawanie ceny preparatu), w wypadku przeciwnym zostawia wartość niezmienioną (3 argument).

```{r}
  bulbs$price_per_hour <- NA #tworzenie nowej, pustej kolumny
  bulbs$price_per_hour <- bulbs$price_in_PLN / bulbs$time_in_hours
  bulbs$price_per_hour <- round(bulbs$price_per_hour, digits = 4)
  bulbs$price_per_hour <- ifelse(bulbs$sprayed == "CoatItYourself", bulbs$price_per_hour + COAT_IT_PRICE, bulbs$price_per_hour)
    #post scriptum: Tutaj przyznaję się do popełnienia błędu, z którego zdałem sobie sprawę już po oddaniu pracy. Zamiast ceny jednego psiku, doliczyłem koszt całego preparatu.
  head(bulbs) #wskazanie, że kolumna została utworzona poprawnie
```

*Porównując sens ekonomiczny użytkowania różnych żarówek, warto porównywać ze sobą średnią cenę jednej godziny ich użytkowania.*

Porównanie średniej ceny jednej godziny użytkowania żarówki odbyło się ponownie na zasadzie testu statystycznego. Wzięte pod uwagę zostały średnie koszty godzinnego użytkowania żarówek dwóch typów, nieosprayowanych. Stwierdzono uprzednio, że spray nie ma wpływu na poprawę długości działania. Na tym etapie, rozstrzygana jest wyłącznie ekonomiczność użycia podwójnej żarówki. 

Wywołując funkcje testujące zastosowano tym razem inną notację, w nawiasach kwadratowych podając warunki wyznaczające analizowany zbiór. Jest to alternatywne podejście do stosowanego wcześniej tworzenia podzbiorów. Na początku sprawdzono normalność rozkładów:

```{r}
  t3 <- shapiro.test(bulbs$price_per_hour[bulbs$sprayed == 'none' & bulbs$product_type == 'regular bulb'])
  t4 <- shapiro.test(bulbs$price_per_hour[bulbs$sprayed == 'none' & bulbs$product_type == 'DoubleBulb'])
  describe(t3)
  describe(t4)
```

Wyniki testu Shapiro-Wilka z założeniami takimi jak w poprzednich przypadkach, każą odrzucić hipotezę o normalności rozkładów. Ponownie do porównania średnich wykorzystany zostanie test Wilcoxona. Hipoteza zerowa $H_0$: *Średnia cena godziny użytkowania podwójnej żarówki **nie** jest statystycznie większa od zwykłej*

```{r}
  w4 <- wilcox.test(bulbs$price_per_hour[bulbs$sprayed == 'none' & bulbs$product_type == 'DoubleBulb'],
              bulbs$price_per_hour[bulbs$sprayed == 'none' & bulbs$product_type == 'regular bulb'],
              alternative='greater', conf.level = .95)
  describe(w4)
```
P-wartość < $\alpha = 0.05$ każe odrzucić hipotezę zerową, przyjmując hipotezę alternatywną - **średnia cena godziny użytkowania bańki podwójnej jest statystycznie większa od zwykłej**.

###### Wnioski:
Wszelkie powyższe obliczenia nie znajdują ekonomicznego (z punktu widzenia klienta) uzasadnienia dla stosowania sprayu:

  1. Jak wykazały testy nie wydłuża on znacząco długości życia żarówek
  2. Jedno opakowanie starcza wyłącznie na niecałe 3 godziny użytkowania
  3. Jest bardzo drogi w porównaniu do uzyskiwanych efektów

Jeśli chodzi o ekonomiczne uzasadnienie korzystania z baniek podwójnych, to faktycznie poprawiają one długość świecenia. Niemniej, ich koszt godzinowy nie rekompensuje dłuższego działania. Mogą one znaleźć nabywców, którym zależy na ograniczeniu odpadów lub zmniejszeniu potrzeby częstszych zakupów, za co są w stanie zapłacić więcej.

##### Może mógłbyś/mogłabyś zaproponować producentowi jakieś zmiany, dzięki którym wzrośnie użyteczność oraz ekonomiczny sens stosowania sprayu?

Proponowane zmiany:

  1. Dalsze testowanie sprayu w celu zwiększenia jego właściwości
  2. Praca nad obniżeniem kosztów sprayu
  3. Poprawa wydajności (tak, aby żarówki nie wymagały cogodzinnej interwencji)
