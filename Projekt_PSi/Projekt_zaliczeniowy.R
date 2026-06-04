#' ---
#' title: "Text Mining opisów Airbnb – Londyn, NYC, Melbourne"
#' author: " "
#' date: "`r format(Sys.Date(), '%d.%m.%Y')`"
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
#'     number_sections: true
#' ---

knitr::opts_chunk$set(message = FALSE, warning = FALSE)

#' # Wymagane pakiety
#'
#' Ładujemy wszystkie biblioteki na początku

library(tm)
library(tidyverse)
library(tidytext)
library(topicmodels)
library(wordcloud)
library(ggplot2)
library(RColorBrewer)


#' # Funkcja pomocnicza – LDA
#'
#' Zamiast kopiować ten sam blok kodu przy każdym wywołaniu LDA, wydzielamy
#' go do funkcji. Przyjmuje wektor tekstów i liczbę tematów k, zwraca wykres
#' lub ramkę danych (gdy `plot = FALSE`).

top_terms_by_topic_LDA <- function(input_text, k = 4, plot = TRUE) {
  
  corpus_lda <- VCorpus(VectorSource(input_text))
  dtm        <- DocumentTermMatrix(corpus_lda)
  
  # Puste dokumenty (same stopwords albo bardzo krótkie opisy) powodują błąd LDA
  dtm <- dtm[unique(dtm$i), ]
  
  lda_model   <- LDA(dtm, k = k, control = list(seed = 42))
  topics_tidy <- tidy(lda_model, matrix = "beta")
  
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
        x     = NULL,
        y     = "β (ważność słowa w temacie)"
      ) +
      coord_flip() +
      theme_minimal(base_size = 11) +
      scale_fill_brewer(palette = "Set2")
  } else {
    return(top_terms)
  }
}


#' # Wczytanie danych
#'
#' Korzystamy z trzech zbiorów Airbnb: Londyn, Nowy Jork i Melbourne.
#' Każdy plik zawiera pełne dane o ogłoszeniach — nas interesuje kolumna
#' `description`, czyli opis mieszkania napisany przez gospodarza.
#' Losujemy po 1000 opisów z każdego miasta, żeby czas obliczeń był rozsądny.
#'
#' **Pliki muszą znajdować się w katalogu roboczym.** Jeśli potrzeba, odkomentuj
#' i dostosuj poniższy `setwd`.

# setwd("ścieżka/do/katalogu/z/plikami")

# Wczytujemy przygotowane wcześniej małe pliki CSV (po 1000 wierszy z każdego
# miasta) - Kolumny: city, description,
# host_neighbourhood.
londyn    <- read.csv("london_small.csv",    stringsAsFactors = FALSE, encoding = "UTF-8")
nyc       <- read.csv("nyc_small.csv",       stringsAsFactors = FALSE, encoding = "UTF-8")
melbourne <- read.csv("melbourne_small.csv", stringsAsFactors = FALSE, encoding = "UTF-8")

data <- rbind(londyn, nyc, melbourne) %>%
  filter(!is.na(description), nchar(trimws(description)) > 0)

cat("Łączna liczba opisów:", nrow(data), "\n")
cat("Rozkład według miast:\n")
print(table(data$city))
cat("\nLiczba opisów z wypełnioną dzielnicą:\n")
print(table(data$city[!is.na(data$neighbourhood_cleansed) &
                        nchar(trimws(data$neighbourhood_cleansed)) > 0]))


#' # Czyszczenie tekstu
#'
#' Opisy z Airbnb są surowe — zawierają tagi HTML (`<br/>`), adresy URL,
#' znaki specjalne i artefakty złego kodowania. Usuwamy je wszystkie zanim
#' zaczniemy liczyć słowa.
#'
#' Dodatkowo usuwamy słowa, które pojawiają się wszędzie i nic nie mówią
#' o charakterze oferty: nazwy miast, ogólne słowa jak "can", "will", "get",
#' oraz oczywiste słowa Airbnb jak "apartment", "bedroom" itp.

corpus <- VCorpus(VectorSource(data$description))

corpus <- tm_map(corpus, content_transformer(function(x) iconv(x, to = "UTF-8", sub = "byte")))

toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))

corpus <- tm_map(corpus, toSpace, "<br/>")
corpus <- tm_map(corpus, toSpace, "<[^>]+>")
corpus <- tm_map(corpus, toSpace, "(s?)(f|ht)tp(s?)://\\S+\\b")
corpus <- tm_map(corpus, toSpace, "http\\w*")
corpus <- tm_map(corpus, toSpace, "@\\w+")
corpus <- tm_map(corpus, toSpace, "\\|")
corpus <- tm_map(corpus, toSpace, "[ \t]{2,}")
corpus <- tm_map(corpus, toSpace, "www")
corpus <- tm_map(corpus, toSpace, "~")
corpus <- tm_map(corpus, toSpace, "â€")

corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, removePunctuation)

corpus <- tm_map(corpus, removeWords, c(
  "apartment", "room", "bed", "bedroom", "place",
  "can", "will", "just", "also", "well", "get",
  "london", "york", "new", "melbourne", "city"
))

corpus <- tm_map(corpus, stripWhitespace)

# Zapamiętujemy wyczyszczone teksty jako zwykły wektor –
# LDA będzie działać na tych tekstach, nie na surowym data$text
cleaned_texts <- sapply(corpus, function(doc) doc$content)
nonempty_idx  <- which(nchar(trimws(cleaned_texts)) > 0)


#' # Analiza częstości słów
#'
#' Zanim przejdziemy do bardziej zaawansowanych metod, sprawdzamy które słowa
#' w ogóle dominują w opisach. To dobry punkt wyjścia do interpretacji
#' późniejszych wyników TF-IDF i LDA.

#' ## Macierz dokumentów (TDM)

tdm   <- TermDocumentMatrix(corpus)
tdm_m <- as.matrix(tdm)
v     <- sort(rowSums(tdm_m), decreasing = TRUE)
tdm_df <- data.frame(word = names(v), freq = v)

cat("Top 15 najczęstszych słów:\n")
print(head(tdm_df, 15))

#' ## Wykres – top 20 słów (wszystkie miasta łącznie)

tdm_df %>%
  head(20) %>%
  ggplot(aes(x = reorder(word, freq), y = freq)) +
  geom_col(fill = "#2C7BB6") +
  coord_flip() +
  labs(title = "Top 20 najczęstszych słów – wszystkie miasta",
       x = NULL, y = "Częstość") +
  theme_minimal(base_size = 12)

#' ## Chmura słów – wszystkie miasta łącznie

set.seed(42)
suppressWarnings({
  par(mar = c(0, 0, 2, 0))
  wordcloud(words = tdm_df$word, freq = tdm_df$freq,
            min.freq = 20, max.words = 60,
            random.order = FALSE,
            colors = brewer.pal(8, "Dark2"),
            scale = c(4, 0.8))
  title("Chmura słów – wszystkie miasta łącznie")
  par(mar = c(5, 4, 4, 2))
})

#' ## Top 15 słów per miasto

words_by_city <- data %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words, by = "word") %>%
  filter(
    !word %in% c(
      "apartment", "room", "bed", "bedroom", "place",
      "can", "will", "just", "also", "well", "get",
      "london", "york", "new", "melbourne", "city", "br", "amp"
    ),
    nchar(word) > 2,
    !grepl("^[0-9]+$", word)
  ) %>%
  count(city, word, sort = TRUE)

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
  labs(title = "Top 15 słów według miasta",
       x = NULL, y = "Liczba wystąpień") +
  theme_minimal(base_size = 11) +
  scale_fill_brewer(palette = "Set1")


#' # Analiza TF-IDF
#'
#' Sama częstość słów nie mówi wszystkiego – słowo "walk" pojawia się często
#' w każdym mieście, więc nie odróżnia Londynu od Melbourne. TF-IDF nagradza
#' słowa, które są częste w opisach jednego miasta, ale rzadkie w pozostałych.
#' Dzięki temu dostajemy słowa naprawdę charakterystyczne dla każdej lokalizacji.

#' ## TF-IDF per miasto – słowa wyróżniające każde miasto

city_tfidf <- words_by_city %>%
  bind_tf_idf(word, city, n) %>%
  arrange(city, desc(tf_idf))

cat("Najbardziej charakterystyczne słowa według TF-IDF:\n")
city_tfidf %>%
  group_by(city) %>%
  top_n(5, tf_idf) %>%
  print(n = 15)

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
    title    = "TF-IDF – słowa najbardziej charakterystyczne dla każdego miasta",
    subtitle = "Im wyższy TF-IDF, tym słowo bardziej unikalne dla danego miasta",
    x = NULL, y = "TF-IDF"
  ) +
  theme_minimal(base_size = 11) +
  scale_fill_brewer(palette = "Set1")

#' ## TF-IDF z pakietem tm + chmura słów

tdm_tfidf   <- TermDocumentMatrix(corpus,
                                  control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))
tdm_tfidf_m  <- as.matrix(tdm_tfidf)
v_tfidf      <- sort(rowSums(tdm_tfidf_m), decreasing = TRUE)
tdm_tfidf_df <- data.frame(word = names(v_tfidf), freq = v_tfidf)

cat("Top 15 słów według TF-IDF:\n")
print(head(tdm_tfidf_df, 15))

set.seed(42)
suppressWarnings({
  par(mar = c(0, 0, 2, 0))
  wordcloud(words = tdm_tfidf_df$word, freq = tdm_tfidf_df$freq,
            min.freq = 1, max.words = 80,
            random.order = FALSE,
            colors = brewer.pal(8, "Set2"),
            scale = c(3.5, 0.4))
  title("Chmura słów TF-IDF – wszystkie miasta")
  par(mar = c(5, 4, 4, 2))
})


#' # Topic Modeling – LDA
#'
#'
#' Kluczowa uwaga techniczna: przekazujemy `cleaned_texts` (już wyczyszczone
#' teksty) zamiast surowego `data$text`. Bez tego LDA widziałoby "the", "and"
#' i tagi HTML jako dominujące "tematy".

#' ## LDA dla różnych k – wszystkie miasta

print(top_terms_by_topic_LDA(cleaned_texts, k = 3))
print(top_terms_by_topic_LDA(cleaned_texts, k = 4))
print(top_terms_by_topic_LDA(cleaned_texts, k = 6))

#' ## LDA osobno dla każdego miasta (k = 3)
#'
#' Sprawdzamy czy tematy różnią się między miastami –
#' czy Londyn "mówi" o czym innym niż Melbourne.

for (miasto in c("London", "NYC", "Melbourne")) {
  idx_miasta    <- which(data$city == miasto)
  teksty_miasta <- cleaned_texts[idx_miasta]
  p <- top_terms_by_topic_LDA(teksty_miasta, k = 3)
  print(p + ggtitle(paste0("LDA (k=3) – ", miasto)))
}

#' ## Rozkład gamma – przynależność opisów do tematów
#'
#' Gamma (γ) to prawdopodobieństwo że dany opis należy do danego tematu.
#' Jeśli tematy są dobrze rozdzielone, opisy jednego miasta powinny mieć
#' wyraźnie wyższe γ dla jednego tematu niż dla pozostałych.

dtm_full         <- DocumentTermMatrix(corpus)
nonempty_doc_ids <- unique(dtm_full$i)
dtm_full         <- dtm_full[nonempty_doc_ids, ]

lda_model <- LDA(dtm_full, k = 4, control = list(seed = 42))

gamma_df <- tidy(lda_model, matrix = "gamma") %>%
  mutate(doc_id   = as.integer(document),
         orig_idx = nonempty_doc_ids[as.integer(document)]) %>%
  select(-document)

gamma_city <- gamma_df %>%
  mutate(city = data$city[orig_idx]) %>%
  drop_na()

gamma_city %>%
  mutate(topic = paste0("Temat ", topic)) %>%
  ggplot(aes(x = city, y = gamma, fill = city)) +
  geom_boxplot(outlier.size = 0.5, alpha = 0.8) +
  facet_wrap(~topic) +
  labs(
    title    = "Rozkład przynależności do tematów LDA według miasta",
    subtitle = "γ = prawdopodobieństwo przynależności opisu do tematu",
    x = NULL, y = "γ (gamma)"
  ) +
  theme_minimal(base_size = 11) +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position = "none")


#' # Analiza dzielnic NYC – TF-IDF i LDA
#'
#' Nowy Jork jest jedynym miastem gdzie dzielnice są na tyle charakterystyczne,
#' że warto je analizować osobno. Manhattan, Brooklyn, Queens czy Bronx to
#' zupełnie różne rynki — sprawdzamy czy różnią się też językiem opisów.
#'
#' Żeby wyniki były czytelne, bierzemy tylko dzielnice z co najmniej 30 opisami.

#' ## Przygotowanie danych NYC

nyc_data <- data %>%
  filter(city == "NYC",
         !is.na(neighbourhood_cleansed),
         nchar(trimws(neighbourhood_cleansed)) > 0) %>%
  mutate(neighbourhood = trimws(neighbourhood_cleansed))

# Zachowaj tylko dzielnice z wystarczającą liczbą opisów
top_neighbourhoods <- nyc_data %>%
  count(neighbourhood, sort = TRUE) %>%
  filter(n >= 30) %>%
  pull(neighbourhood)

nyc_data <- nyc_data %>%
  filter(neighbourhood %in% top_neighbourhoods)

cat("Dzielnice NYC w analizie:\n")
print(sort(table(nyc_data$neighbourhood), decreasing = TRUE))

#' ## TF-IDF – słowa charakterystyczne dla każdej dzielnicy NYC

nyc_words <- nyc_data %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words, by = "word") %>%
  filter(
    !word %in% c(
      "apartment", "room", "bed", "bedroom", "place",
      "can", "will", "just", "also", "well", "get",
      "york", "new", "city", "br", "amp"
    ),
    nchar(word) > 2,
    !grepl("^[0-9]+$", word)
  ) %>%
  count(neighbourhood, word, sort = TRUE)

nyc_tfidf <- nyc_words %>%
  bind_tf_idf(word, neighbourhood, n) %>%
  arrange(neighbourhood, desc(tf_idf))

# Wykres – top 8 słów per dzielnica
nyc_tfidf %>%
  group_by(neighbourhood) %>%
  top_n(8, tf_idf) %>%
  ungroup() %>%
  mutate(
    neighbourhood = as.character(neighbourhood),
    word          = as.character(word),
    word          = reorder_within(word, tf_idf, neighbourhood)
  ) %>%
  ggplot(aes(word, tf_idf, fill = neighbourhood)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~neighbourhood, scales = "free_y", ncol = 3) +
  scale_x_reordered() +
  coord_flip() +
  labs(
    title    = "TF-IDF – słowa charakterystyczne dla dzielnic NYC",
    subtitle = "Co wyróżnia język opisów Manhattanu, Brooklynu i pozostałych dzielnic?",
    x = NULL, y = "TF-IDF"
  ) +
  theme_minimal(base_size = 10) +
  scale_fill_brewer(palette = "Set3")

#' ## LDA – tematy w opisach dzielnic NYC
#'
#' Budujemy osobny model LDA tylko dla NYC, żeby sprawdzić jakie tematy
#' pojawiają się w opisach i czy różne dzielnice mają różne profile tematyczne.

# Wyczyszczone teksty tylko dla NYC (z już istniejącego cleaned_texts)
nyc_idx           <- which(data$city == "NYC")
nyc_cleaned       <- cleaned_texts[nyc_idx]

print(top_terms_by_topic_LDA(nyc_cleaned, k = 3) +
        ggtitle("LDA (k=3) – opisy Airbnb w Nowym Jorku"))

# Rozkład gamma per dzielnica
nyc_corpus <- VCorpus(VectorSource(nyc_cleaned))
nyc_dtm    <- DocumentTermMatrix(nyc_corpus)
nyc_nonempty <- unique(nyc_dtm$i)
nyc_dtm    <- nyc_dtm[nyc_nonempty, ]

nyc_lda    <- LDA(nyc_dtm, k = 3, control = list(seed = 42))

nyc_gamma <- tidy(nyc_lda, matrix = "gamma") %>%
  mutate(
    local_idx  = as.integer(document),          # pozycja w NYC DTM
    orig_idx   = nyc_idx[nyc_nonempty[local_idx]], # oryginalny indeks w data
    neighbourhood = nyc_data$neighbourhood[
      match(orig_idx, which(data$city == "NYC"))
    ]
  ) %>%
  filter(!is.na(neighbourhood))

nyc_gamma %>%
  mutate(topic = paste0("Temat ", topic)) %>%
  ggplot(aes(x = neighbourhood, y = gamma, fill = neighbourhood)) +
  geom_boxplot(outlier.size = 0.4, alpha = 0.8) +
  facet_wrap(~topic) +
  coord_flip() +
  labs(
    title    = "Rozkład tematów LDA w dzielnicach NYC",
    subtitle = "",
    x = NULL, y = "γ (gamma)"
  ) +
  theme_minimal(base_size = 10) +
  scale_fill_brewer(palette = "Set3") +
  theme(legend.position = "none")


#' # Podsumowanie wyników

cat("\n=== TOP 5 słów globalnie (częstość) ===\n")
print(head(tdm_df, 5))

cat("\n=== TOP 5 słów globalnie (TF-IDF) ===\n")
print(head(tdm_tfidf_df, 5))

cat("\n=== Słowa najbardziej charakterystyczne per miasto (TF-IDF) ===\n")
city_tfidf %>%
  group_by(city) %>%
  top_n(3, tf_idf) %>%
  select(city, word, tf_idf) %>%
  print(n = 9)

cat("\n=== Top słowa w LDA (k=4) ===\n")
top_terms_by_topic_LDA(cleaned_texts, k = 4, plot = FALSE) %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  print(n = 20)
