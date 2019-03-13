library(rvest)
library(dplyr)
library(tidyverse)
library(stringr)
library(tibble)
library(RSQLite)
library(sentimentr)
# Reviews data function

# This is a custom function to get product details
# Inputs: keyword and page
# Output: A dataframe with name, rating, desc, review and so on
product <- function(keyword, page) {
  url <- "https://www.amazon.com/product-reviews/%s/ref=cm_cr_arp_d_paging_btm_%i?ie=UTF8&reviewerType=all_reviews&pageNumber=%i"
  # URLencode replaces ASCII ( spaces etc) characters with a "%" followed by two hexadecimal digits.
  # using sprintf function create the complete url of the specifc
  # keyword and its page
  # Finally,  read_html pulls the content as a xml_document with header content and body
  amazon <- sprintf(url, URLencode(keyword), page, page)
  reviewpage <- read_html(amazon)


  # '%>%' piping is the tidyverse style of pulling conents
  # Pull html nodes 
  # So, basically pulls all authors from the nodes and then
  # gets the text, if present else is a empty character
  # substritute "By" with nothing. The dot "." means from the piped output of html_text.
  profile_name <- reviewpage %>%
    html_nodes(".review-byline .author") %>%
    html_text(trim = T) %>%
    gsub("By", "", .)

# Same as above, but str_extract digits and you convert it to numeric as  this is rating
  personal_rating <- reviewpage %>%
    html_nodes("#cm_cr-review_list  .review-rating") %>%
    html_text() %>%
    str_extract("\\d") %>%
    as.numeric()


  description <- reviewpage %>%
    html_nodes("#cm_cr-review_list .a-color-base") %>%
    html_text(trim = T)


  review <- reviewpage %>%
    html_nodes(".review-text") %>%
    html_text(trim = T)

# here we are presenting text that has Verified Purchase keyword
  verification <- reviewpage %>%
    html_nodes(".review-data.a-spacing-mini") %>%
    html_text(trim = T) %>%
    grepl("Verified Purchase", .)

# Finally create a dataframe with the extracted contents
  product_id <- keyword
  df <- data.frame(profile_name, personal_rating, description, review, verification, product_id)
}
# This is connecting to database, and creating tables
ma <- dbConnect(SQLite(), dbname = "x.sqlite")
dbSendQuery(conn = ma, "create table maintable(
            product_id1 text primary key,
            name1 text,
            price1 num,
            ratings1 num,
            no_of_reviews1 num)
            without rowid")
dbSendQuery(conn = ma, "create table reviewtable(
            profile_name text,
            personal_rating num,
            description text,
            review text,
            verification logical,
            product_id1 text,
            foreign key(product_id1) references maintable(product_id1))")
# instead of running product function above on multiple pages like this
# prodcut(keyword = "Good", page = 1)
# prodcut(keyword = "Good", page = 2)
# prodcut(keyword = "Good", page = 3) and so on is repetitive of the same thing. Instead, map function helps to parallely map across all this and, you are wrapping it in another function.
# So, something like map(1:10, productfunc) --> This gives you list of dataframes.
# Something like list( df1 = page1data,
# df2= page2data , df3= page3data and so on)
# Since, the columns of all the dataframes are same, you can just pipe bind_rows function to get as a Single large dataframe
# And, finally write to the review table in the database
multiple_page <- function(keyword, page) {
  x <- purr::map(1:page, function(n) {
    product(keyword, n)
  }) %>%
    bind_rows()
  dbWriteTable(conn = ma, name = "reviewtable", value = x, row.names = F, header = T, overwrite = T)
}

multiple_page("B0756CYWWD", 4)
product("B0756CYWWD", 1)

url <- "https://www.amazon.com/product-reviews/%s/ref=cm_cr_arp_d_paging_btm_%i?ie=UTF8&reviewerType=all_reviews&pageNumber=%i"
amazon <- sprintf(url, URLencode(keyword), page, page)
reviewpage <- read_html(amazon)



profile_name <- reviewpage %>%
  html_nodes(".review-byline .author") %>%
  html_text(trim = T) %>%
  gsub("By", "", .)


# main page function

main_page <- function(keyword) {
  main <- "https://www.amazon.com/gp/product/%s"
  item <- sprintf(main, URLencode(keyword))
  items <- read_html(item)
  name <- items %>%
    html_node("#productTitle") %>%
    html_text(trim = T)

  ratings <- items %>%
    html_node(".arp-rating-out-of-text") %>%
    html_text() %>%
    str_extract("\\d{1}.\\d{1}") %>%
    as.numeric()

  price <- items %>%
    html_node("#priceblock_ourprice") %>%
    html_text(trim = T) %>%
    gsub("\\$", "", .) %>%
    as.numeric()

  no_of_reviews <- items %>%
    html_node("#acrCustomerReviewText") %>%
    html_text(trim = T) %>%
    gsub("[A-z]", "", .) %>%
    gsub(",", "", .) %>%
    as.numeric()

  product_id <- keyword

  df <- data_frame(product_id, name, price, ratings, no_of_reviews)

  dbWriteTable(conn = ma, name = "maintable", value = df, row.names = F, header = T, overwrite = T)
}
main_page("B0756CYWWD")

a <- dbGetQuery(conn = ma, "select * from maintable m
              join reviewtable r
              on m.product_id=r.product_id")
head(a)
dbGetQuery(conn = ma, "select * from maintable")

sent_agg <- with(a, sentiment_by(review))
head(sent_agg)
par(mfrow = c(1, 2))
with(a, hist(personal_rating))
with(sent_agg, hist(ave_sentiment))
mean(a$personal_rating)
mean(sent_agg$ave_sentiment)

# do the sentiment anlaysis 
best_reviews <- slice(a, top_n(sent_agg, 10, ave_sentiment)$element_id)
with(best_reviews, sentiment_by(review)) %>% highlight()

worst_reviews <- slice(a, top_n(sent_agg, 10, -ave_sentiment)$element_id)
with(worst_reviews, sentiment_by(review)) %>% highlight()

library(dplyr)
library(stringr)
library(tidytext)

get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")

b <- a$review
text_tb <- tibble(
  c = seq_along(b),
  text = b
)
head(text_tb)
x <- text_tb %>%
  unnest_tokens(word, text)
x
x %>%
  count(word, sort = T)
x %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)
x %>%
  right_join(get_sentiments("nrc")) %>%
  filter(!is.na(sentiment)) %>%
  count(sentiment, sort = TRUE)

bing_word_counts <- x %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
# plotting using gglot
bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ggplot(aes(reorder(word, n), n, fill = sentiment)) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment", x = NULL) +
  coord_flip()

library(wordcloud)

x %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

library(reshape2)
x %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(
    colors = c("#F8766D", "#00BFC4"),
    max.words = 100
  )
# on x, add new cols word_count, and index
# then innerjoin with get_sentiments
# group the data frame by index column and then summarize score for the group in a new column sentiment score
# then add what method of this sentiment score was , here it is afinn

afinn <- x %>%
  mutate(
    word_count = 1:n(),
    index = word_count %/% 500 + 1
  ) %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(index) %>%
  summarise(sentiment = sum(score)) %>%
  mutate(method = "AFINN")
afinn
