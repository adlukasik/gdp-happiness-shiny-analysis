# Interaktywna Analiza: Pieniądze a Szczęście (GDP vs Happiness)

Projekt zrealizowany w ramach zaliczenia przedmiotu "Programowanie w R". Aplikacja bada zależność między PKB per capita a poziomem szczęścia (Cantril Ladder) w ujęciu globalnym i kontynentalnym w latach 2014-2023.

## Linki do prjektu
* **[Interaktywna Aplikacja (R Shiny)](https://adam-lukasik.shinyapps.io/gdp-happiness-analysis/)**
* **[Raport (HTML)](https://adlukasik.github.io/gdp-happiness-shiny-analysis/app/)**

## O Projekcie

Celem projektu jest zweryfikowanie hipotezy o wpływie zamożności kraju na poziom zadowolenia jego mieszkańców. Aplikacja pozwala użytkownikowi na samodzielną eksplorację danych, filtrowanie według regionów oraz analizę trendów w czasie.

**Główne wnioski z analizy (EDA):**
* Istnieje silna dodatnia korelacja między PKB a szczęściem.
* Zależność ta najlepiej opisywana jest przez **model logarytmiczny** (prawo malejących przychodów) – wzrost dochodów w krajach uboższych daje większy przyrost szczęścia niż w krajach bogatych.

## Funkcjonalności Aplikacji

Aplikacja została zbudowana w pakiecie **R Shiny** i oferuje 4 główne moduły analityczne:

1.  **Wykres Punktowy (Scatter Plot):**
    * Wizualizacja korelacji PKB vs Szczęście.
    * Możliwość nałożenia linii trendu: brak, liniowa, **logarytmiczna**, pierwiastkowa.
    * Filtrowanie po kontynentach i zakresie lat.
2.  **Statystyki:**
    * Automatycznie generowane podsumowanie statystyczne (min, max, średnia, kwartyle) dla wybranego podzbioru danych.
3.  **Rozkład i Nierówności (Boxplot):**
    * Analiza rozkładu zmiennych wewnątrz kontynentów.
    * Identyfikacja dysproporcji w regionach (np. duże nierówności w Azji vs stabilność w Oceanii).
4.  **Zmiana w czasie:**
    * Wykresy liniowe pokazujące dynamikę zmian średniego PKB i szczęścia na przestrzeni dekady (2014-2023).

## Technologie i Pakiety

Projekt wykorzystuje język **R** oraz następujące biblioteki:

* `shiny` - silnik aplikacji interaktywnej.
* `bslib` - nowoczesny motyw graficzny ("pulse") i czcionki Google.
* `dplyr` - manipulacja i agregacja danych.
* `ggplot2` - zaawansowana wizualizacja danych.
* `scales` - formatowanie osi (waluty).

## Struktura Danych

Dane pochodzą z **Our World in Data** 
Repozytorium zawiera katalog `dane/` z plikami niezbędnymi do uruchomienia aplikacji:
* `gdp-per-capita-worldbank.csv`
* `happiness-cantril-ladder.csv`
* `kontynenty.csv`
