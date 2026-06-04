#' # Zmiany względem skryptów z zajęć
#'
#' Poniżej zestawienie wszystkich modyfikacji wprowadzonych na potrzeby
#' tego projektu w porównaniu do kodu omawianego na zajęciach.
#'
#' ## Dane i preprocessing
#'
#' - **Dane Airbnb zamiast pojedynczych plików tekstowych** – zamiast wczytywać
#'   dokumenty z katalogu, wczytujemy trzy pliki CSV z opisami mieszkań
#'   i losujemy próbkę 1000 opisów z każdego miasta (`sample_n`, `set.seed(42)`).
#' - **Rozszerzone czyszczenie** – dodano usuwanie tagów HTML (`<br/>`, `<[^>]+>`),
#'   adresów URL, znaków `@`, `~`, `|` oraz artefaktów złego kodowania (`â€`),
#'   które są specyficzne dla danych Airbnb eksportowanych jako CSV.
#' - **Własna lista stopwords Airbnb** – usunięto słowa zbyt ogólne dla tego
#'   kontekstu: nazwy miast, "apartment", "bedroom", "can", "will" itp.
#' - **Wyciągnięcie `cleaned_texts`** – po czyszczeniu zapisujemy teksty jako
#'   wektor `cleaned_texts = sapply(corpus, ...)`. Na zajęciach tego nie było;
#'   tu jest konieczne żeby LDA i TF-IDF działały na tych samych, czystych danych.
#'
#' ## Analiza częstości
#'
#' - **Podział na miasta** – dodano wykres top 15 słów osobno dla każdego miasta
#'   (`facet_wrap`), na zajęciach był tylko widok globalny.
#'
#' ## TF-IDF
#'
#' - **TF-IDF per miasto z `tidytext`** – na zajęciach TF-IDF liczono globalnie
#'   przez `weightTfIdf` w macierzy `tm`. Tu dodano podejście `bind_tf_idf`
#'   z pakietu `tidytext`, które liczy TF-IDF traktując każde miasto jako
#'   osobny "dokument zbiorczy" – dzięki temu wykres pokazuje słowa
#'   charakterystyczne dla Londynu vs NYC vs Melbourne.
#'
#' ## LDA
#'
#' - **`reorder_within` zamiast `reorder`** – na zajęciach słowa na wykresach
#'   `facet_wrap` nie były posortowane poprawnie wewnątrz każdego panelu.
#'   `reorder_within` + `scale_x_reordered` z pakietu `tidytext` to naprawia.
#' - **`k` jako parametr funkcji** – na zajęciach funkcja odwoływała się do
#'   zmiennej globalnej `number_of_topics`. Zmieniono na normalny argument
#'   z wartością domyślną `k = 4`.
#' - **Wejście: `cleaned_texts` zamiast `data$text`** – kluczowa poprawka:
#'   na zajęciach funkcja budowała nowy korpus od zera wewnątrz siebie,
#'   przez co LDA widziało surowy tekst z HTML i stopwords. Tu przekazujemy
#'   już wyczyszczone teksty.
#' - **LDA osobno per miasto** – dodano pętlę porównującą tematy między miastami.
#' - **Macierz gamma** – dodano analizę przynależności opisów do tematów
#'   z poprawnym mapowaniem indeksów po usunięciu pustych dokumentów.
#'
#' ## Analiza dzielnic NYC
#'
#' - **Nowa sekcja: TF-IDF i LDA per dzielnica** – dodano analizę wyłącznie
#'   dla NYC z podziałem na `host_neighbourhood`. Pozwala odpowiedzieć na pytanie
#'   czy Manhattan, Brooklyn i inne dzielnice różnią się językiem opisów,
#'   co daje konkretny wymiar analityczny niemożliwy przy samym podziale na miasta.
#' - **Filtr minimalnej liczby opisów** – dzielnice z mniej niż 30 opisami są
#'   pomijane, żeby TF-IDF i LDA miały wystarczająco dużo danych do sensownych wyników.
