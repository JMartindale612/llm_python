## llama classification

# source("constructs.R")


# library(tidyllm)
# library(rollama) # https://jbgruber.github.io/rollama/
# library(here)
# library(glue)

source("dictionary.R")

library(tidyverse)
library(quanteda)
library(quanteda.dictionaries)
library(quanteda.textmodels)
library(quanteda.textplots)
library(quanteda.textstats)
library(readtext)
library(easystats)
require(marginaleffects)
library(betareg)
library(emmeans)

data <- read_csv("data/donald_representative_domain-group_txt.csv")

data <- data %>%
  mutate(domain_group = factor(domain_group))

data$domain_group <- relevel(factor(data$domain_group), ref = "LB00")

data_subset <- data %>%
  group_by(domain_group) %>%
  slice_sample(n = 100) %>%
  ungroup()






#### Things to change ######
# 1. Use all documents rather than just this subset
# 2. Peer 

## Dictionaries ----

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
    
    # Terms from Bonacchi & Acerbi
  )
))



### Quanteda ---

# 1. Tokenize
corpus_obj <- corpus(data, text_field = "article_text")
toks <- tokens(corpus_obj, remove_punct = TRUE)
toks <- tokens_tolower(toks)

# 2. Convert to dfm
dfm_all <- dfm(toks)

# 3. Lookup dictionary, aggregating under the single dictionary key
dfm_nostalgia <- dfm_lookup(dfm_all, 
                            dictionary = restorative_nostalgia_dict, 
                            levels = 1)

# 4. Convert to data.frame
nostalgia_counts <- convert(dfm_nostalgia, to = "data.frame")
head(nostalgia_counts)

# 'nostalgia_counts' will have 2 columns: "document" and "restorative_nostalgia"
# cbind or merge with your original data

data <- cbind(
  data,
  restorative_nostalgia = nostalgia_counts$restorative_nostalgia
)

# 5. Proportion of nostalgia
data$nostalgia_proportion <- data$restorative_nostalgia / ntoken(dfm_all)
str(data)

table(data$domain_group)


descriptives <- data %>%
  group_by(domain_group) %>%
  summarise(
    mean_nostalgia = mean(restorative_nostalgia, na.rm = TRUE),
    sum_nostalgia  = sum(restorative_nostalgia, na.rm = TRUE),
    mean_proportion = mean(nostalgia_proportion, na.rm = TRUE)
  ) %>%
  arrange(desc(mean_nostalgia))

write_csv(data, "data/data_with_sentiment.csv")
write_csv(descriptives, "data/descriptives_donald.csv")


# Refit the model
betareg_model <- betareg(nostalgia_proportion ~ domain_group, data = data)
summary(betareg_model)

saveRDS(betareg_model, "data/betareg_model.rds")
my_model <- readRDS("data/betareg_model.rds")

# Now run emmeans with explicit data
emmeans_full <- pairs(emmeans(betareg_model, ~ domain_group, data = data), adjust = "bonferroni")

# exp(coef(betareg_model))
# 
# 
# pairs(emmeans(betareg_model, ~ domain_group, data = data), adjust = "bonferroni")

emmeans_results <- as.data.frame(emmeans(betareg_model, ~domain_group))

ggplot(emmeans_results, aes(x = reorder(domain_group, emmean), y = emmean)) +
  geom_point() +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.2) +
  coord_flip() +
  labs(title = "Estimated Nostalgia Proportion by Domain Group",
       x = "Domain Group",
       y = "Estimated Proportion (Beta Regression)") +
  theme_minimal()

summary(betareg_model)
emmeans_results


emmeans_results$probability <- plogis(emmeans_results$emmean)

emmeans_results$scaled <- 1 + 4 * emmeans_results$probability

str(data)

# Calculate number of tokens per document
data <- data %>%
  mutate(token_count = ntoken(article_text))

descriptives <- data %>%
  group_by(domain_group) %>%
  summarise(
    mean_nostalgia = mean(restorative_nostalgia, na.rm = TRUE),
    sum_nostalgia = sum(restorative_nostalgia, na.rm = TRUE),
    mean_proportion = mean(nostalgia_proportion, na.rm = TRUE),
    mean_tokens = mean(token_count, na.rm = TRUE)  # Add mean token count
  ) %>%
  arrange(desc(mean_nostalgia))



descriptives


color_map <- c(
  "L" = "#D55E00",  # Left (Orange-Red)
  "R" = "#0072B2",  # Right (Blue)
  "CT" = "#E69F00", # Conspiratorial (Yellow-Orange)
  "LB" = "#009E73"  # Least-Biased (Green)
)

color_map <- c(
  "L01" = "#D81B60", "L03" = "#D81B60", "L05" = "#D81B60",  # Left (red, orange, purple)
  "R01" = "#1E88E5", "R03" = "#1E88E5", "R05" = "#1E88E5",  # Right (blue, green, purple)
  "CT01" = "#FFC107", "CT02" = "#FFC107", "CT03" = "#FFC107", 
  "CT04" = "#FFC107", "CT05" = "#FFC107",  # CT (yellow/orange)
  "LB00" = "#004D40"  # Least-Biased (Green)
)

emmeans_results$color_group <- emmeans_results$domain_group

# Extract ideological group prefix (L, R, CT, LB) for color mapping
emmeans_results$group <- substr(emmeans_results$domain_group, 1, 2)

# Improved plot
# Update plot
ggplot(emmeans_results, aes(x = emmean, y = reorder(domain_group, emmean), color = color_group)) +
  geom_point(size = 3) +  
  geom_errorbarh(aes(xmin = asymp.LCL, xmax = asymp.UCL), height = 0.2) +
  scale_color_manual(values = color_map) +
  labs(
    title = "Estimated Nostalgia Proportion by Domain Group",
    subtitle = "Beta regression estimates with 95% confidence intervals",
    x = "Estimated Nostalgia (Logit Scale)",
    y = "Domain Group"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    panel.grid.major.x = element_line(color = "gray80"),
    panel.grid.major.y = element_blank(),
    legend.position = "none"
  )









color_map <- c(
  "L" = "#D55E00",  # Left (Orange-Red)
  "R" = "#0072B2",  # Right (Blue)
  "CT" = "#E69F00", # Conspiratorial (Yellow-Orange)
  "LB" = "#009E73"  # Least-Biased (Green)
)



emmeans_results$group <- substr(emmeans_results$domain_group, 1, 2)


ggplot(emmeans_results, aes(x = emmean, y = reorder(domain_group, emmean), color = group)) +
  geom_point(size = 3) +  # Use points for spacing
  geom_errorbarh(aes(xmin = asymp.LCL, xmax = asymp.UCL), height = 0.2) +  # Horizontal error bars
  scale_color_manual(values = color_map) +  # Apply color mapping
  labs(
    title = "Estimated Nostalgia Proportion by Domain Group",
    subtitle = "Beta regression estimates with 95% confidence intervals",
    x = "Estimated Nostalgia (Logit Scale)",
    y = "Domain Group"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    panel.grid.major.x = element_line(color = "gray80"),
    panel.grid.major.y = element_blank(),
    legend.position = "none"  # Remove legend if not needed
  )










### Separating variables

# Create new variables from domain_group
data <- data %>%
  mutate(
    leaning = case_when(
      str_detect(domain_group, "^CT") ~ "CT",   # Conspiratorial
      str_detect(domain_group, "^LB") ~ "LB",   # Least-Biased
      str_detect(domain_group, "^L")  ~ "L",    # Left
      str_detect(domain_group, "^R")  ~ "R"     # Right
    ),
    strength = as.numeric(str_extract(domain_group, "[0-9]+")) # Extract numbers
  )

# Check if new variables are correct
table(data$leaning, data$strength)

# Set reference level for leaning (LB00 as baseline)
data$leaning <- relevel(factor(data$leaning), ref = "LB")

# Ensure strength is numeric (so it is treated as ordinal)
data$strength <- as.numeric(data$strength)


interaction_model <- betareg(nostalgia_proportion ~ leaning * strength, data = data)

# View summary
summary(interaction_model)


test <- data %>%
  filter(!is.na(nostalgia_proportion) & is.finite(nostalgia_proportion))

simple_model <- betareg(nostalgia_proportion ~ leaning + strength, data = data)
summary(simple_model)

data$strength_collapsed <- case_when(
  data$strength %in% c(00, 01) ~ "low",
  data$strength %in% c(02, 03) ~ "moderate",
  data$strength %in% c(04, 05) ~ "high"
)
data$strength_collapsed <- factor(data$strength_collapsed, levels = c("low", "moderate", "high"))

model_fact_int <- betareg(nostalgia_proportion ~ leaning * strength_collapsed, data = data)
summary(model_fact_int)



# Fit the beta regression model
beta_model_quality <- betareg(nostalgia_proportion ~ domain_quality, data = data)

# Summarize results
summary(beta_model_quality)


ggplot(data, aes(x = domain_quality, y = nostalgia_proportion)) +
  geom_point(alpha = 0.4) +  # Scatterplot with transparency
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +  # Beta regression line
  labs(
    title = "Relationship Between Nostalgia and Domain Quality",
    x = "Domain Quality",
    y = "Nostalgia Proportion"
  ) +
  theme_minimal()







## Marginal effects package ----

estimate_contrasts(lm_model)


means <- estimate_means(model, by = "domain_group")
means

ggplot(iris, aes(x = domain_group, y = nostalgia_proportion)) +
  # Add base data
  geom_violin(aes(fill = domain_group), color = "white") +
  geom_jitter(width = 0.1, height = 0, alpha = 0.5, size = 3) +
  # Add pointrange and line for means
  geom_line(data = means, aes(y = Mean, group = 1), linewidth = 1) +
  geom_pointrange(
    data = means,
    aes(y = Mean, ymin = CI_low, ymax = CI_high),
    size = 1,
    color = "white"
  ) +
  # Improve colors
  scale_fill_manual(values = c("pink", "lightblue", "lightgreen")) +
  theme_minimal()
