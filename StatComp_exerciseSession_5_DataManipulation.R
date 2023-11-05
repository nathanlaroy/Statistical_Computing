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
system.time(df_dt[, c("token_low", "lemma_low") := .(tolower(token), 
                                                     tolower(lemma))])

# 4
df_freq <- data.frame(with(df, table(lemma_low)))
df_tibble_freq <- df_tibble %>% count(lemma_low, name = "freq")
dt_freq <- df_dt[, .N, lemma_low]

system.time(data.frame(with(df, table(lemma_low))))
system.time(df_tibble %>% count(lemma_low, name = "freq"))
system.time(df_dt[, .N, lemma_low])

# 5
df_freq_order <- with(df_freq, df_freq[order(Freq, decreasing = T),])
df_tibble_order <- df_tibble %>% count(lemma_low, name = "freq", sort = T)
dt_order <- dt_freq[order(N, decreasing = T)]

# 6
upos_type <- data.frame(upos = c("NOUN", "PROPN", "ADJ", "VERB", "ADV", "NUM",
                                 "DET", "PRON", "ADP", "CCONJ", "SCONJ", "PART",
                                 "AUX", "INTJ"),
                        type = c(rep("Content", 6), rep("Function", 8)))

df$order <- data.frame(order = 1:length(df[, 1]))
df_merged_unsorted <- merge(x = upos_type, y = df, by.x = "upos", by.y = "upos", all = T)
df_merged <- df_merged_unsorted[order(df_merged_unsorted$order[,1]),]
df_merged <- subset(df_merged, select = -c(order))

df_tibble_merged <- full_join(x = df_tibble, y = tibble(upos_type),
                              by = "upos")

df_dt_order <- copy(df_dt[, order := c(1:length(df_dt$doc_id))])
df_dt_merged <- merge(df_dt_order, data.table(upos_type), by = "upos", all = T)
df_dt_merged <- subset(df_dt_merged[order(order),], select = -c(order))

# 7
total_n <- sum(df_freq_order$Freq)

perc <- prop.table(df_freq_order$Freq)*100
df_perc <- cbind(df_freq_order, perc)
system.time(reshape(df_perc, varying = list(c("Freq", "perc")),
             v.names = "Value", timevar = "Count", idvar = "lemma_low",
             times = c("Freq", "Perc"), direction = "long"))


tibble_perc <- df_tibble_order %>% add_column(perc = perc)
system.time(tibble_perc_long <- tibble_perc %>% pivot_longer(cols = c("freq","perc"),
                                                names_to = "Kind",
                                                values_to = "Count"))

dt_order[, Perc := perc]
system.time(reshape(dt_order, varying = list(c("N", "Perc")),
             v.names = "Value", timevar = "Count", idvar = "lemma_low",
             times = c("Freq", "Perc"), direction = "long"))
