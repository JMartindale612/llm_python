## llama classification

source("constructs.R")


# library(tidyllm)
# library(rollama) # https://jbgruber.github.io/rollama/
# library(here)
# library(glue)

library(tidyverse)
library(quanteda)
library(quanteda.dictionaries)
library(quanteda.textmodels)
library(quanteda.textplots)
library(quanteda.textstats)
library(readtext)

data <- read_csv("data/donald_representative_domain-group_txt.csv")

data <- data %>%
  mutate(domain_group = factor(domain_group))

data_subset <- data %>%
  group_by(domain_group) %>%
  slice_sample(n = 100) %>%
  ungroup()

write_csv(data_subset, "data/data_sentiment_llm.csv")

data_test <- data %>%
  group_by(domain_group) %>%
  slice_sample(n = 20) %>%
  ungroup()

# ping_ollama()
# pull_model("all-minilm")
# 
# str(data)
# 
# str(constructs)

restorative_nostalgia_dict <- dictionary(list(
  restorative_nostalgia = c(
    # Historical framing
    "earlier", "once was", "ancestor", "heritage",
    "legacy", "roots", "traditional", "foundation", "forefather",
    "before", "tradition",  "proud", "values",
    "pride", "past", "now", "changes", "used to be", "was",
    "young", "youth", "legacy", "ancestral", "yesterday",
    
    # Idealization/Positive Sentiment About the Past
    "golden", "glorious", "idyllic", "rose-tinted", "home",
    "prosperity", "cherished", "storied", "lost glory", "once proud",
    "better", "authentic", "peaceful", "pleasant", "real country",
    "greatness", "impressive", "lost paradise",
    
    # Negativity About the Present or Future
    "decline", "decay", "losing", "lost", "eroding", "erosion", "moral",
    "falling apart", "unravel", "broken", "astray",
    "worse", "tomorrow", "future", "bittersweet",
    
    # Superiority / group membership
    "our people", "our", "us", "them", "their", "patriots", 
    "group", "we", "real", "outsiders", "majority", "minority", "belong",
    "native", "superior", "inferior",
    
    # Calls to Restore or Revive the Past
    "bring back", "restore", "reclaim", "revive", "take back",
    "great again", "return", "renaissance", "reset", "security", "fix",
    "recapture",
    
    # Terms associated with identity fusion and extremism
    "homeland", "motherland", "fatherland", "family", "loyalty",
    "blood", "brother", "sister", "daughters", "sons", "conspirators",
    "stealing", "stolen", "resistance", "glory", "admire", "bloodline",
    "kin", "traitor", "demolish", "pollute", "scum", "impure", 
    "brainwashed", "subjugate", "overwhelmed", "under siege", "running out of time",
    "betray", "betrayal", "sell", "sold", "conspire", "collude",
    # Martyrdom
    "defend", "protect", "fight", "self-defence", "self-defense", "preserve"
  )
))


### Quanteda ---

str(data_test)

test <- corpus("data/data_sentiment_llm.csv")
test2 <- readtext("data/data_sentiment_llm.csv")

corpus_obj <- corpus(data, text_field = "article_text")
tokens_obj <- tokens(corpus_obj, remove_punct = TRUE)
tokens_obj <- tokens_tolower(tokens_obj)
dfm_obj <- dfm(tokens_obj, dictionary = restorative_nostalgia_dict)
nostalgia_counts <- convert(dfm_obj, to = "data.frame")
data <- cbind(data, nostalgia_counts[, -1])  # Exclude doc_id column if needed
data$nostalgia_proportion <- data$restorative_nostalgia / ntoken(tokens_obj)




