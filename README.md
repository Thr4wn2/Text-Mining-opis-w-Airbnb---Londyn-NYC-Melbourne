#  Text Mining opisów Airbnb – Londyn, NYC, Melbourne

Projekt zaliczeniowy z przedmiotu Text Mining.  
Autorzy: **Eliza Brzywczy (SRS), Jan Mikusek (Plik R)** | Data: czerwiec 2026
---

##  Opis projektu

Skrypt R przeprowadza analizę text mining opisów ofert Airbnb z trzech miast: **Londynu, Nowego Jorku i Melbourne**. Projekt identyfikuje słowa charakterystyczne dla każdego miasta, wykrywa ukryte tematy w opisach oraz analizuje różnice językowe między dzielnicami Nowego Jorku.

Wyniki prezentowane są w postaci interaktywnego raportu HTML z wykresami, chmurami słów i tabelami.

---

##  Zastosowane techniki

| Technika | Opis |
|---|---|
| **Analiza częstości słów** | Top słowa globalnie i per miasto, chmury słów |
| **TF-IDF** | Słowa charakterystyczne dla miast i dzielnic NYC |
| **Topic Modeling (LDA)** | Wykrywanie ukrytych tematów dla k=3,4,6 |
| **Analiza dzielnic NYC** | TF-IDF i LDA per dzielnica (Manhattan, Brooklyn itd.) |

---

##  Struktura repozytorium

```
├── airbnb_text_mining.R     # główny skrypt analityczny
├── przygotuj_dane.R         # skrypt do przygotowania małych plików CSV
├── london_small.csv         # 1000 opisów z Londynu
├── nyc_small.csv            # 1000 opisów z NYC
├── melbourne_small.csv      # 1000 opisów z Melbourne
└── README.md
```

---

##  Jak uruchomić

### 1. Wymagania

R w wersji **4.0 lub nowszej** oraz następujące pakiety:

```r
install.packages(c(
  "tm", "tidyverse", "tidytext", "topicmodels",
  "wordcloud", "ggplot2", "RColorBrewer"
))
```

### 2. Dane

Pliki `*_small.csv` są już gotowe w repozytorium — nie trzeba pobierać oryginalnych dużych plików.


### 3. Generowanie raportu HTML

Otwórz `Projekt_zaliczeniowy.R` w RStudio i kliknij **Compile Report** (lub użyj skrótu `Ctrl+Shift+K`).

Raport HTML zostanie wygenerowany automatycznie z pełnym spisem treści, wykresami i komentarzami.

---

##  Wyniki analizy

Raport zawiera:

- **Chmury słów** — globalna i TF-IDF, pokazujące dominujące słowa w opisach
- **Top 20 słów** — wykres słupkowy najczęstszych słów we wszystkich miastach
- **TF-IDF per miasto** — co wyróżnia język opisów Londynu, NYC i Melbourne (np. *tube* w Londynie, *subway* w NYC, *tram* w Melbourne)
- **LDA per miasto** — jakie tematy dominują w opisach każdego miasta (lokalizacja, wyposażenie, atmosfera)
- **Analiza dzielnic NYC** — TF-IDF i LDA per dzielnica (Manhattan, Brooklyn, Williamsburg, Harlem i inne)
- **Rozkład gamma** — prawdopodobieństwo przynależności opisów do tematów LDA

---

##  Specyfikacja wymagań

Projekt realizuje wymagania opisane w dokumencie **SRS (Software Requirements Specification)**:

-  Wczytanie danych z plików CSV z kodowaniem UTF-8
-  Usuwanie tagów HTML, URL, stopwords, znaków specjalnych i słów specyficznych dla Airbnb
-  Analiza częstości słów i macierz TDM
-  Wyznaczenie TF-IDF dla miast i dzielnic NYC
-  Modele LDA dla k=3, k=4, k=6
-  Wykresy beta (top słowa per temat) i gamma (rozkład przynależności)
-  Chmury słów, wykresy słupkowe, tabele
-  Obsługa pustych dokumentów po czyszczeniu tekstu
-  Obsługa brakujących wartości (`NA`)

---

##  Dane źródłowe

Dane pochodzą z serwisu [Inside Airbnb](http://insideairbnb.com/get-the-data/) — publicznie dostępne zbiory danych o ofertach Airbnb.  
W projekcie użyto próbek po **1000 opisów** z każdego miasta (losowanie z `set.seed(42)`).

---

##  Uwagi techniczne

- Kolumna z dzielnicami pochodzi z pola `neighbourhood_cleansed` (ustandaryzowane przez Airbnb), a nie `host_neighbourhood` (wypełniane przez gospodarzy — często puste)
- Analiza dzielnic NYC ograniczona do dzielnic z co najmniej 30 opisami
- Interpretacja gamma: opisy Airbnb są z natury wielotematyczne (lokalizacja + wyposażenie + atmosfera), dlatego rozkład γ jest zbliżony między miastami — bardziej informatywne są wykresy β pokazujące słowa dominujące w każdym temacie

Readme wykonane z pomocą Claude
