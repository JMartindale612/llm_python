# Web scraping

source("setup.R")

library(tidyverse)
library(rvest)

# Daily Mail - https://www.dailymail.co.uk/news/brexit/index.html
## https://www.dailymail.co.uk/news/article-13904113/BORIS-JOHNSON-Macron-not-hesitate-Cuban-heeled-bootee-Brexit-Britain-got-revenge-G7-summit.html
# Guardian -https://www.theguardian.com/politics/eu-referendum
## https://www.theguardian.com/politics/2025/mar/03/britain-is-back-did-ukraine-crisis-talks-create-a-post-brexit-turning-point
# BBC - https://www.bbc.co.uk/news/politics/uk_leaves_the_eu
## https://www.bbc.co.uk/news/uk-northern-ireland-68951063

html <- read_html("http://rvest.tidyverse.org/")

html

html <- minimal_html("
  <h1>This is a heading</h1>
  <p id='first'>This is a paragraph</p>
  <p class='important'>This is an important paragraph</p>
")


bbc_article <- read_html("https://www.bbc.co.uk/news/uk-northern-ireland-68951063/")
bbc_paras <- bbc_article %>% html_elements("p")
bbc_h1 <- bbc_article %>% html_elements("h1")
bbc_h2 <- bbc_article %>% html_elements("h2")
bbc_h3 <- bbc_article %>% html_elements("h3")

bbc_paras %>% html_text2()
bbc_h1 %>% html_text2()
bbc_h2 %>% html_text2()


guard_art <- read_html("https://www.theguardian.com/politics/2025/mar/03/britain-is-back-did-ukraine-crisis-talks-create-a-post-brexit-turning-point")
g_paras <- guard_art %>% html_elements("p")
g_h1 <- guard_art %>% html_elements("h1")
g_h2 <- guard_art %>% html_elements("h2")

g_paras %>% html_text2()
g_h1 %>% html_text2()
g_h2 %>% html_text2()

m_art <- read_html("https://www.dailymail.co.uk/news/article-13904113/BORIS-JOHNSON-Macron-not-hesitate-Cuban-heeled-bootee-Brexit-Britain-got-revenge-G7-summit.html")
m_paras <- m_art %>% html_elements("p")
m_h1 <- m_art %>% html_elements("h1")
m_h2 <- m_art %>% html_elements("h2")

one <- m_paras %>% html_text2()
m_h1 %>% html_text2()
m_h2 %>% html_text2()
str(one)

test <- as.character(g_paras)
test
str(test)

str(m_paras)
m_paras


### ChatGPT ----

extract_article_text <- function(url, domain_name) {
  page <- read_html(url)
  paragraphs <- page %>%
    html_elements("p") %>%
    html_text2()
  
  # Collapse paragraphs into a single string
  article_text <- paste(paragraphs, collapse = " ")
  
  # Return a 1-row data frame
  tibble(
    url    = url,
    domain = domain_name,
    text   = article_text
    )
}

# Example usage
daily_mail_url <- "https://www.dailymail.co.uk/news/article-13904113/BORIS-JOHNSON-Macron-not-hesitate-Cuban-heeled-bootee-Brexit-Britain-got-revenge-G7-summit.html"

dm_one_article <- extract_article_text(daily_mail_url, "daily_mail")
dm_one_article
str(dm_one_article)



search_url <- "https://www.dailymail.co.uk/home/search.html?offset=0&size=50&searchPhrase=brexit&sort=recent"

page       <- read_html(search_url)

# Inspect the HTML to see how article links are identified. 
# Often the link has a CSS class like `.sch-res-title a` or `.linkro-darkred > a`
# for the Daily Mail search.  Adjust the below selector as needed.
article_links <- page %>%
  html_elements(".sch-res-title a") %>%
  html_attr("href")

