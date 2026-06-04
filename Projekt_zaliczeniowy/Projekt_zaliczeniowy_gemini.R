#' ---
#' title: "Text Mining opisów Airbnb – Londyn, NYC, Melbourne"
#' author: " "
#' date:   " "
#' output:
#'   html_document:
#'     df_print: paged
#'     theme: readable
#'     highlight: kate
#'     toc: true
#'     toc_depth: 3
#'     toc_float:
#'       collapsed: false
#'       smooth_scroll: true
#'     code_folding: show
#'     number_sections: false
#' ---

knitr::opts_chunk$set(message = FALSE, warning = FALSE)


# ============================================================
# WYMAGANE PAKIETY
# ============================================================
library(tm)
library(tidyverse)
library(tidytext)
library(topicmodels)
library(wordcloud)
library(ggplot2)
library(RColorBrewer)
library(cluster)      # klastrowanie
library(factoextra)   # wizualizacje klastrów (fviz_nbclust, fviz_cluster)
library(DT)           # interaktywne tabele datatable()


# ============================================================
# 0. FUNKCJA POMOCNICZA: TOP TERMINY WEDŁUG TEMATU LDA
# ============================================================
# Wejście: wektor tekstów (już wyczyszczony), liczba tematów k
# Wyjście: wykres top-10 słów na temat (lub ramka danych)

top_terms_by_topic_LDA <- function(input_text,
                                   k = 4,
                                   plot = TRUE) {
  corpus_lda <- VCorpus(VectorSource(input_text))
  dtm <- DocumentTermMatrix(corpus_lda)
  
  # Usuń puste wiersze (dokumenty bez żadnych tokenów) – powodują błąd LDA
  unique_idx <- unique(dtm$i)
  dtm <- dtm[unique_idx, ]
  
  # Model LDA
  lda_model <- LDA(dtm, k = k, control = list(seed = 42))
  
  # Wyciągnij rozkład beta (prawdopodobieństwo słowa w temacie)
  topics_tidy <- tidy(lda_model, matrix = "beta")
  
  # Top 10 słów dla każdego tematu
  top_terms <- topics_tidy %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
  
  if (plot) {
    top_terms %>%
      mutate(term = reorder_within(term, beta, topic)) %>%
      ggplot(aes(term, beta, fill = factor(topic))) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~topic, scales = "free_y") +
      scale_x_reordered() +
      labs(
        title = paste0("LDA – top 10 słów dla k = ", k, " tematów"),
        x = NULL,
        y = "β (ważność słowa w temacie)"
      ) +
      coord_flip() +
      theme_minimal(base_size = 11) +
      scale_fill_brewer(palette = "Set2")
  } else {
    return(top_terms)
  }
}


# ============================================================
# 1. WCZYTANIE DANYCH
# ============================================================
# Pliki: london.csv.gz, nyc.csv.gz, melbourne.csv.gz
# Każdy plik powinien zawierać kolumnę `description`
# Upewnij się, że pliki znajdują się w katalogu roboczym (setwd)

# setwd("ścieżka/do/katalogu/z/plikami")   # <-- odkomentuj i dostosuj

londyn    <- read.csv("london_small.csv",    stringsAsFactors = FALSE, encoding = "UTF-8")
nyc       <- read.csv("nyc_small.csv",       stringsAsFactors = FALSE, encoding = "UTF-8")
melbourne <- read.csv("melbourne_small.csv", stringsAsFactors = FALSE, encoding = "UTF-8")

df_londyn    <- data.frame(city = "London",    text = londyn$description)
df_nyc       <- data.frame(city = "NYC",       text = nyc$description)
df_melbourne <- data.frame(city = "Melbourne", text = melbourne$description)

#Łączymy w jeden zbiór
data <- rbind(df_londyn, df_nyc, df_melbourne)

cat("Łączna liczba opisów:", nrow(data), "\n")
cat("Rozkład według miast:\n")
print(table(data$city))


# ============================================================
# 2. BUDOWA KORPUSU I CZYSZCZENIE TEKSTU
# ============================================================

corpus <- VCorpus(VectorSource(data$text))

# Zapewnienie kodowania UTF-8 w całym korpusie
corpus <- tm_map(corpus, content_transformer(function(x) iconv(x, to = "UTF-8", sub = "byte")))

# Funkcja pomocnicza: zamień wzorzec na spację
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))

# Usuń tagi HTML, adresy URL i inne artefakty
corpus <- tm_map(corpus, toSpace, "<br/>")                        # tagi HTML nowe linie (specyficzne dla Airbnb)
corpus <- tm_map(corpus, toSpace, "<[^>]+>")                      # pozostałe tagi HTML
corpus <- tm_map(corpus, toSpace, "(s?)(f|ht)tp(s?)://\\S+\\b")   # adresy URL
corpus <- tm_map(corpus, toSpace, "http\\w*")                     # pozostałości http
corpus <- tm_map(corpus, toSpace, "@\\w+")                        # nazwy użytkowników @
corpus <- tm_map(corpus, toSpace, "\\|")                          # linia pionowa
corpus <- tm_map(corpus, toSpace, "[ \t]{2,}")                    # wielokrotne spacje/tabulatory
corpus <- tm_map(corpus, toSpace, "www")
corpus <- tm_map(corpus, toSpace, "~")
corpus <- tm_map(corpus, toSpace, "â€")                           # artefakty złego kodowania

# Standardowe operacje czyszczenia
corpus <- tm_map(corpus, content_transformer(tolower))   # zamiana na małe litery
corpus <- tm_map(corpus, removeNumbers)                  # usunięcie liczb
corpus <- tm_map(corpus, removeWords, stopwords("english")) # usunięcie stopwords
corpus <- tm_map(corpus, removePunctuation)              # usunięcie interpunkcji

# Usunięcie słów zbyt ogólnych lub specyficznych dla danych Airbnb
# (nie wnoszą informacji różnicującej między ofertami/miastami)
corpus <- tm_map(corpus, removeWords, c(
  "apartment", "room", "bed", "bedroom", "place",
  "can", "will", "just", "also", "well", "get",
  "london", "york", "new", "melbourne", "city"
))

corpus <- tm_map(corpus, stripWhitespace)                # usunięcie zbędnych spacji

# Wyciągnięcie wyczyszczonych tekstów z korpusu jako wektor
# WAŻNE: używamy ich w LDA zamiast surowego data$text,
# żeby model nie widział "the", "and", tagów HTML itp.
cleaned_texts <- sapply(corpus, function(doc) doc$content)

# Zachowaj też mapowanie indeks -> miasto (potrzebne do gamma w sekcji 6C)
# Niektóre dokumenty mogą być puste po czyszczeniu – zapamiętujemy które
nonempty_idx <- which(nchar(trimws(cleaned_texts)) > 0)


# ============================================================
# 3. MACIERZ CZĘSTOŚCI TDM
# ============================================================

# TDM (Term-Document Matrix) – wiersze = słowa, kolumny = dokumenty
tdm   <- TermDocumentMatrix(corpus)
tdm_m <- as.matrix(tdm)

# Zliczenie częstości słów (globalnie)
v      <- sort(rowSums(tdm_m), decreasing = TRUE)
tdm_df <- data.frame(word = names(v), freq = v)

cat("\nTop 15 najczęstszych słów (globalnie):\n")
print(head(tdm_df, 15))


# ============================================================
# 4. ANALIZA CZĘSTOŚCI SŁÓW
# ============================================================

# --- 4A. Wykres słupkowy – top 20 globalnie ---
tdm_df %>%
  head(20) %>%
  ggplot(aes(x = reorder(word, freq), y = freq)) +
  geom_col(fill = "#2C7BB6") +
  coord_flip() +
  labs(
    title = "Top 20 najczęstszych słów – wszystkie miasta",
    x = NULL,
    y = "Częstość"
  ) +
  theme_minimal(base_size = 12)


# --- 4B. Chmura słów (globalna) ---
set.seed(42)
wordcloud(
  words  = tdm_df$word,
  freq   = tdm_df$freq,
  min.freq = 20,
  max.words = 100,
  random.order = FALSE,
  colors = brewer.pal(8, "Dark2"),
  scale  = c(4, 0.5)
)
title("Chmura słów – wszystkie miasta łącznie")


# --- 4C. Porównanie top 15 słów między miastami ---
# Tidy tokenizacja z podziałem na miasto
words_by_city <- data %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  filter(
    !word %in% c(
      "apartment", "room", "bed", "bedroom", "place",
      "can", "will", "just", "also", "well", "get",
      "london", "york", "new", "melbourne", "city",
      "br", "amp"
    ),
    nchar(word) > 2,          # odfiltruj zbyt krótkie tokeny
    !grepl("^[0-9]+$", word)  # odfiltruj same liczby
  ) %>%
  count(city, word, sort = TRUE)

# Top 15 słów dla każdego miasta
words_by_city %>%
  group_by(city) %>%
  top_n(15, n) %>%
  ungroup() %>%
  mutate(word = reorder_within(word, n, city)) %>%
  ggplot(aes(word, n, fill = city)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~city, scales = "free_y") +
  scale_x_reordered() +
  coord_flip() +
  labs(
    title = "Top 15 słów według miasta",
    x = NULL,
    y = "Liczba wystąpień"
  ) +
  theme_minimal(base_size = 11) +
  scale_fill_brewer(palette = "Set1")


# ============================================================
# 5. ANALIZA TF-IDF
# ============================================================
# TF-IDF pozwala znaleźć słowa CHARAKTERYSTYCZNE dla danego
# dokumentu (tu: dla danego MIASTA), a nie tylko najczęstsze.

# --- 5A. TF-IDF z tidytext – słowa charakterystyczne dla każdego miasta ---
city_tfidf <- words_by_city %>%
  bind_tf_idf(word, city, n) %>%
  arrange(city, desc(tf_idf))

cat("\nNajbardziej charakterystyczne słowa według TF-IDF:\n")
city_tfidf %>%
  group_by(city) %>%
  top_n(5, tf_idf) %>%
  print(n = 15)

# Wykres – top 12 słów wg TF-IDF na miasto
city_tfidf %>%
  group_by(city) %>%
  top_n(12, tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder_within(word, tf_idf, city)) %>%
  ggplot(aes(word, tf_idf, fill = city)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~city, scales = "free_y") +
  scale_x_reordered() +
  coord_flip() +
  labs(
    title = "TF-IDF – słowa najbardziej charakterystyczne dla każdego miasta",
    subtitle = "Im wyższy TF-IDF, tym słowo bardziej unikalne dla danego miasta",
    x = NULL,
    y = "TF-IDF"
  ) +
  theme_minimal(base_size = 11) +
  scale_fill_brewer(palette = "Set1")


# --- 5B. TF-IDF z pakietem tm (oryginalne podejście z zajęć) ---
tdm_tfidf   <- TermDocumentMatrix(corpus,
                                  control = list(
                                    weighting = function(x) weightTfIdf(x, normalize = FALSE)
                                  ))
tdm_tfidf_m <- as.matrix(tdm_tfidf)

v_tfidf      <- sort(rowSums(tdm_tfidf_m), decreasing = TRUE)
tdm_tfidf_df <- data.frame(word = names(v_tfidf), freq = v_tfidf)

cat("\nTop 15 słów według TF-IDF (macierz tm):\n")
print(head(tdm_tfidf_df, 15))

# Chmura słów TF-IDF
set.seed(42)
wordcloud(
  words  = tdm_tfidf_df$word,
  freq   = tdm_tfidf_df$freq,
  min.freq = 1,
  max.words = 80,
  random.order = FALSE,
  colors = brewer.pal(8, "Set2"),
  scale  = c(3.5, 0.4)
)
title("Chmura słów TF-IDF – wszystkie miasta")

# Porównanie: częstość vs TF-IDF (wykres punktowy top 50)
compare_df <- tdm_df %>%
  head(200) %>%
  inner_join(tdm_tfidf_df, by = "word", suffix = c("_raw", "_tfidf"))

compare_df %>%
  head(50) %>%
  ggplot(aes(x = freq_raw, y = freq_tfidf, label = word)) +
  geom_point(color = "#D7191C", alpha = 0.7, size = 2) +
  ggrepel::geom_text_repel(size = 3, max.overlaps = 20) +
  labs(
    title = "Porównanie: surowa częstość vs TF-IDF",
    x = "Surowa częstość",
    y = "Suma TF-IDF"
  ) +
  theme_minimal(base_size = 11)


# ============================================================
# 6. TOPIC MODELING – LDA
# ============================================================
# Modelowanie tematów pozwala odkryć ukryte tematy
# przewijające się w opisach ofert Airbnb.

# --- 6A. LDA dla wszystkich miast łącznie ---
# Używamy cleaned_texts (wyczyszczony korpus) zamiast surowego data$text
cat("\n--- LDA: k = 3 tematy (wszystkie miasta) ---\n")
print(top_terms_by_topic_LDA(cleaned_texts, k = 3))

cat("\n--- LDA: k = 4 tematy (wszystkie miasta) ---\n")
print(top_terms_by_topic_LDA(cleaned_texts, k = 4))

cat("\n--- LDA: k = 6 tematów (wszystkie miasta) ---\n")
print(top_terms_by_topic_LDA(cleaned_texts, k = 6))


# --- 6B. LDA osobno dla każdego miasta (k = 3) ---
# Używamy cleaned_texts z indeksowaniem po mieście
for (miasto in c("London", "NYC", "Melbourne")) {
  cat(paste0("\n--- LDA k=3: ", miasto, " ---\n"))
  idx_miasta   <- which(data$city == miasto)          # indeksy wierszy dla danego miasta
  teksty_miasta <- cleaned_texts[idx_miasta]           # wyczyszczone teksty tylko tego miasta
  p <- top_terms_by_topic_LDA(teksty_miasta, k = 3)
  print(p + ggtitle(paste0("LDA (k=3) – ", miasto)))
}


# --- 6C. Dokładniejsza analiza LDA: prawdopodobieństwo tematów w dokumentach ---
# Budujemy model LDA raz dla całego zbioru (k=4)
# Używamy już wyczyszczonego `corpus` – NIE budujemy go od nowa

dtm_full <- DocumentTermMatrix(corpus)

# Zapamiętaj które dokumenty są niepuste (indeksy w oryginalnym data)
# żeby poprawnie przypisać miasto do dokumentu po usunięciu pustych wierszy
nonempty_doc_ids <- unique(dtm_full$i)          # indeksy niepustych dokumentów w DTM
dtm_full         <- dtm_full[nonempty_doc_ids, ] # usuń puste dokumenty

lda_model <- LDA(dtm_full, k = 4, control = list(seed = 42))

# Macierz gamma: prawdopodobieństwo przynależności każdego dokumentu do tematu
# document w gamma odpowiada kolejnym wierszom DTM po usunięciu pustych
gamma_df <- tidy(lda_model, matrix = "gamma") %>%
  mutate(doc_id = as.integer(document)) %>%  # doc_id = pozycja w odfiltrowanym DTM
  select(-document)

# Odwzoruj doc_id z gamma na oryginalny indeks w data (przez nonempty_doc_ids)
gamma_df <- gamma_df %>%
  mutate(orig_idx = nonempty_doc_ids[doc_id])  # oryginalny indeks wiersza w data

# Dołącz etykietę miasta korzystając z oryginalnego indeksu
gamma_city <- gamma_df %>%
  mutate(city = data$city[orig_idx]) %>%
  drop_na()

# Rozkład tematów według miasta (box plot)
gamma_city %>%
  mutate(topic = paste0("Temat ", topic)) %>%
  ggplot(aes(x = city, y = gamma, fill = city)) +
  geom_boxplot(outlier.size = 0.5, alpha = 0.8) +
  facet_wrap(~topic) +
  labs(
    title = "Rozkład przynależności do tematów LDA według miasta",
    subtitle = "γ = prawdopodobieństwo przynależności dokumentu do tematu",
    x = NULL,
    y = "γ (gamma)"
  ) +
  theme_minimal(base_size = 11) +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position = "none")


# ============================================================
# 7. KLASTROWANIE K-MEANS
# ============================================================
# Klastrowanie pozwala wykryć naturalne grupy opisów Airbnb –
# np. oferty budżetowe, luksusowe, turystyczne (jak sugerowała prowadząca).
# Używamy macierzy TF-IDF (nie surowych częstości) przed klastrowaniem,
# żeby rzadkie ale charakterystyczne słowa miały odpowiednią wagę.

# Budujemy DTM na wyczyszczonym korpusie (bez stemmingu) z wagami TF-IDF
dtm_cluster <- DocumentTermMatrix(
  corpus,
  control = list(
    weighting = function(x) weightTfIdf(x, normalize = TRUE),
    minDocFreq = 5     # ignoruj słowa rzadziej niż w 5 dokumentach
  )
)

# Usuń puste wiersze
dtm_cluster   <- dtm_cluster[unique(dtm_cluster$i), ]
dtm_cluster_m <- as.matrix(dtm_cluster)

cat("Wymiary macierzy DTM (TF-IDF) do klastrowania:", dim(dtm_cluster_m), "\n")


# --- 7A. Dobór liczby klastrów – metoda sylwetki (z zajęć) ---
# Metoda sylwetki mierzy jak dobrze każdy punkt pasuje do swojego klastra.
# Szukamy maksimum na wykresie.
# UWAGA: na 3000 dokumentach może chwilę trwać

set.seed(123)
fviz_nbclust(dtm_cluster_m, kmeans, method = "silhouette", k.max = 8) +
  labs(title  = "Dobór liczby klastrów – metoda sylwetki",
       subtitle = "Wybieramy k z najwyższą średnią szerokością sylwetki")


# --- 7B. Klastrowanie k-means dla wybranych k ---
# Sprawdzamy k = 3 i k = 4 (jak z zajęć: różne k)

for (k in c(3, 4)) {
  
  cat(paste0("\n========== KLASTROWANIE k = ", k, " ==========\n"))
  
  set.seed(123)
  klastrowanie <- kmeans(dtm_cluster_m, centers = k, nstart = 25)
  
  # Wizualizacja klastrów w przestrzeni 2D (PCA) – z zajęć
  print(
    fviz_cluster(
      list(data = dtm_cluster_m, cluster = klastrowanie$cluster),
      geom       = "point",
      ellipse    = TRUE,
      ellipse.type = "convex",
      main       = paste0("Wizualizacja klastrów k-means (k = ", k, ")"),
      ggtheme    = theme_minimal(base_size = 11)
    )
  )
  
  # Podsumowanie klastrów: liczba dokumentów + top 5 słów (z zajęć)
  cluster_info <- lapply(1:k, function(i) {
    idx       <- which(klastrowanie$cluster == i)
    docs_sub  <- dtm_cluster_m[idx, , drop = FALSE]
    word_freq <- sort(colSums(docs_sub), decreasing = TRUE)
    top_words <- paste(names(word_freq)[1:5], collapse = ", ")
    data.frame(
      Klaster            = i,
      Liczba_opisow      = length(idx),
      Top_5_slow         = top_words,
      stringsAsFactors   = FALSE
    )
  })
  cluster_info_df <- do.call(rbind, cluster_info)
  
  # DODANE: dołącz miasto do tabeli
  valid_idx <- as.integer(rownames(dtm_cluster_m))  # oryginalne indeksy w data
  doc_table <- data.frame(
    Opis_nr  = valid_idx,
    Miasto   = data$city[valid_idx],
    Klaster  = klastrowanie$cluster,
    stringsAsFactors = FALSE
  ) %>%
    left_join(cluster_info_df, by = "Klaster")
  
  # Interaktywna tabela (z zajęć)
  print(
    datatable(
      doc_table,
      caption  = paste0("k = ", k, ": opisy, klastry, najczęstsze słowa"),
      rownames = FALSE,
      options  = list(pageLength = 10)
    )
  )
  
  # Rozkład miast w klastrach 
  doc_table %>%
    count(Klaster, Miasto) %>%
    mutate(Klaster = factor(Klaster)) %>%
    ggplot(aes(x = Klaster, y = n, fill = Miasto)) +
    geom_col(position = "fill") +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(
      title    = paste0("Skład miast w klastrach (k = ", k, ")"),
      subtitle = "Czy klastry odpowiadają miastom czy segmentom ofert?",
      x        = "Klaster", y = "Udział"
    ) +
    theme_minimal(base_size = 11) +
    scale_fill_brewer(palette = "Set1") -> p_miasta
  print(p_miasta)
  
  # Chmury słów dla każdego klastra (z zajęć)
  for (i in 1:k) {
    idx       <- which(klastrowanie$cluster == i)
    docs_sub  <- dtm_cluster_m[idx, , drop = FALSE]
    word_freq <- colSums(docs_sub)
    suppressWarnings({
      par(mar = c(0, 0, 2, 0))
      wordcloud(names(word_freq), freq = word_freq,
                max.words = 30, random.order = FALSE,
                colors = brewer.pal(8, "Dark2"), scale = c(3, 0.5))
      title(paste0("Chmura słów – Klaster ", i, " (k=", k, ")"))
      par(mar = c(5, 4, 4, 2))
    })
  }
}


# ============================================================
# 8. PODSUMOWANIE WYNIKÓW
# ============================================================

cat("\n========================================================\n")
cat("PODSUMOWANIE PROJEKTU – TEXT MINING OPISÓW AIRBNB\n")
cat("========================================================\n\n")

cat("Analizowane miasta: Londyn, NYC, Melbourne\n")
cat("Liczba opisów na miasto: 1000 (próbka losowa)\n\n")

cat("--- TOP 5 słów globalnie (częstość) ---\n")
print(head(tdm_df, 5))

cat("\n--- TOP 5 słów globalnie (TF-IDF) ---\n")
print(head(tdm_tfidf_df, 5))

cat("\n--- Słowa najbardziej charakterystyczne dla każdego miasta (TF-IDF) ---\n")
city_tfidf %>%
  group_by(city) %>%
  top_n(3, tf_idf) %>%
  select(city, word, tf_idf) %>%
  print(n = 9)

cat("\n--- Top słowa w LDA (k=4, wszystkie miasta) ---\n")
top_terms_by_topic_LDA(cleaned_texts, k = 4, plot = FALSE) %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  print(n = 20)

cat("\n--- Klastrowanie k=3: liczba opisów per klaster ---\n")
set.seed(123)
km_summary <- kmeans(dtm_cluster_m, centers = 3, nstart = 25)
table(Klaster = km_summary$cluster,
      Miasto  = data$city[as.integer(rownames(dtm_cluster_m))]) %>%
  print()

cat("\n========================================================\n")
cat("Projekt zakończony.\n")
cat("========================================================\n")