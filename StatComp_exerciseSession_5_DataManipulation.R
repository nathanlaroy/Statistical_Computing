library(microbenchmark)

# 1
library(tidyverse)
library(data.table)

df <- read.table(file = "https://raw.githubusercontent.com/kplevoet/texts/main/doyle/baskervilles_TAG.txt",
                 quote = "", # enable in-text quotes to be read as such
                 header = T)

df_tibble <- as_tibble(df)
df_dt <- data.table(df)

byte_sizes <- c(object.size(df), object.size(df_tibble), object.size(df_dt))
# [1] 2888200 2888336 2888816

# 2
df_subset <- subset(df, df$upos == "PUNCT")
df_tibble_subset <- df %>% filter(upos == "PUNCT")
df_dt_subset <- copy(df_dt[upos == "PUNCT"])

microbenchmark(expr =  subset(df, df$upos == "PUNCT"))
microbenchmark(expr = df %>% filter(upos == "PUNCT"))
microbenchmark(expr = copy(df_dt[upos == "PUNCT"]))

# 3
df <- within(df, {token_low <- tolower(df$token)
                  lemma_low <- tolower(df$lemma)})
df_tibble <- df_tibble %>% mutate(token_low = tolower(token), lemma_low = tolower(lemma))
df_dt[, c("token_low", "lemma_low") := .(tolower(token), tolower(lemma))]

system.time(within(df, {token_low <- tolower(df$token)
                        lemma_low <- tolower(df$lemma)}))
system.time(df_tibble %>% mutate(token_low = tolower(token), 
                                 lemma_low = tolower(lemma)))
system.time(df_dt[, c("token_low", "lemma_low") := .(tolower(token), tolower(lemma))])

# 4
df_freq <- data.frame(with(df, table(lemma_low)))
df_tibble_freq <- df_tibble %>% count(lemma_low, name = "freq")
dt_freq <- df_dt[, .N, lemma_low]

system.time(data.frame(with(df, table(lemma_low))))
system.time(df_tibble %>% count(lemma_low, name = "freq"))
system.time(df_dt[, .N, lemma_low])

# 5
df_freq_order <- with(df_freq, df_freq[order(Freq, decreasing = T),])
