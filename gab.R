# dictionary 

source("setup.R")

## Gab Hate Corpus can be found at https://osf.io/edua3/
## Coding manual https://osf.io/nqt6h

## Coding for the messages are as follows:
# Text = Message text
# Hate = 
# HD = Assaults on human dignity
# CV = Calls for violence
# VO = Vulgarity / Offensive Language directed at an individual
# NH = Not hateful
# REL = Religion / Spiritual beliefs (anti-Muslim, anti-Christian etc.)
# RAE = Race or ethnicity (anti-Asian, anti-Latino, anti-black, anti-Arab, anti-semitism etc.)
# SXO = Sexual orientation
# GEN = Gender (anti-man, anti-women, anti-trans etc.)
# IDL = Ideology (conservative / liberal / leftist / right-wing)
# NAT = Nationality / regionalism
# POL = Political identification (Democrat / Republication etc.)
# MPH = (Mental / physical status, physical disability etc.)
# EX = Explicit
# IM = Implicit

gab <- as.data.frame(fread("data/gab/gab_annotations.tsv"))

gab <- gab %>%
  group_by(ID) %>%
  summarize(
    # Keep the text for reference (just take the first text per ID)
    ID = first(ID),
    Text = first(Text),
    # Compute sums across all raters
    Hate_sum  = sum(Hate,  na.rm = TRUE),
    HD_sum  = sum(HD,  na.rm = TRUE),
    CV_sum  = sum(CV,  na.rm = TRUE),
    VO_sum  = sum(VO,  na.rm = TRUE),
    REL_sum  = sum(REL,  na.rm = TRUE),
    RAE_sum  = sum(RAE,  na.rm = TRUE),
    SXO_sum  = sum(SXO,  na.rm = TRUE),
    GEN_sum  = sum(GEN,  na.rm = TRUE),
    IDL_sum  = sum(IDL,  na.rm = TRUE),
    NAT_sum  = sum(NAT,  na.rm = TRUE),
    POL_sum  = sum(POL,  na.rm = TRUE),
    MPH_sum  = sum(MPH,  na.rm = TRUE),
    EX_sum  = sum(EX,  na.rm = TRUE),
    IM_sum  = sum(IM,  na.rm = TRUE)
  ) %>%
  ungroup() %>%
  rename(text = Text)

# Tokenisation & sentiment generation ----

# 1. Tokenize
corpus_obj <- corpus(gab, text_field = "text")
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

gab <- cbind(
  gab,
  restorative_nostalgia = nostalgia_counts$restorative_nostalgia
)

# 5. Proportion of nostalgia
gab$nostalgia_proportion <- gab$restorative_nostalgia / ntoken(dfm_all)

table(data$domain_group)

str(gab)

write_csv(gab, file = "data/gab/gab_nostalgia.csv")

## Sentiment analysis ----

gab <- read_csv("data/gab/gab_nostalgia.csv")
gab <- gab %>%
  mutate(across(ends_with("_sum"), as.integer))

str(gab)

# Overall hate
hate_betareg <- betareg(nostalgia_proportion ~ Hate_sum, 
                         data = gab)
summary(hate_betareg)
saveRDS(hate_betareg, "data/gab/hate_betareg.rds")
hate_betareg <- readRDS("data/gab/hate_betareg.rds")

# Hate categories
hatetype_betareg <- betareg(nostalgia_proportion ~ HD_sum + CV_sum + VO_sum, 
                            data = gab)
summary(hatetype_betareg)
saveRDS(hatetype_betareg, "data/gab/hatetype_betareg.rds")
hatetype_betareg <- readRDS("data/gab/hatetype_betareg.rds")

# Hate targets
hatetarget_betareg <- betareg(nostalgia_proportion ~ REL_sum + 
                                RAE_sum + SXO_sum + GEN_sum + 
                                IDL_sum + NAT_sum + POL_sum + 
                                MPH_sum + EX_sum + IM_sum, 
                            data = gab)
summary(hatetarget_betareg)
saveRDS(hatetarget_betareg, "data/gab/hatetarget_betareg.rds")


hatetargetonly_betareg <- betareg(nostalgia_proportion ~ REL_sum + 
                                RAE_sum + SXO_sum + GEN_sum + 
                                IDL_sum + NAT_sum + POL_sum + 
                                MPH_sum,
                              data = gab)
summary(hatetargetonly_betareg)
saveRDS(hatetargetonly_betareg, "data/gab/hatetargetonly_betareg.rds")

hateovert_betareg <- betareg(nostalgia_proportion ~ EX_sum + IM_sum, 
                              data = gab)
summary(hateovert_betareg)
saveRDS(hatetarget_betareg, "data/gab/hateovert_betareg.rds")





my_model <- readRDS("data/gab/hatetarget_betareg.rds")


cormat <- gab %>%
  select(nostalgia_proportion,
    Hate_sum:IM_sum) %>%
  triangle_correlation_matrix()



head(gab)








# Alternate cleaning options

# Try it as a categorical variable, for targets of hate
gab <- gab %>%
  mutate(target_category = case_when(
    REL_sum > 0 ~ "Race/Ethnicity",
    RAE_sum > 0 ~ "Race/Ethnicity",
    SXO_sum > 0 ~ "Race/Ethnicity",
    GEN_sum > 0 ~ "Race/Ethnicity",
    IDL_sum > 0 ~ "Race/Ethnicity",
    NAT_sum > 0 ~ "Race/Ethnicity",
    POL_sum > 0 ~ "Race/Ethnicity",
    MPH_sum > 0 ~ "Race/Ethnicity",
    EX_sum  > 0 ~ "Race/Ethnicity",
    IM_sum  > 0 ~ "Race/Ethnicity",
    TRUE ~ "None"
  ))

gab$target_category <- as.factor(gab$target_category)

# Try it as a multilevel model
gab_long <- gab %>%
  select(ID, nostalgia_proportion, RAE_sum, SXO_sum, GEN_sum, ...) %>%
  pivot_longer(cols = ends_with("_sum"), 
               names_to = "category", 
               values_to = "sum_annotators")

# using glmmTMB for a Beta model with random intercept by doc
library(glmmTMB)

# Suppose the outcome is "nostalgia_proportion" in (0,1)
# We'll treat "category" as a fixed effect, and "ID" as a random intercept
mod_mixed <- glmmTMB(
  nostalgia_proportion ~ category + (1 | ID),
  data = gab_long,
  family = beta_family(link = "logit")
)
summary(mod_mixed)