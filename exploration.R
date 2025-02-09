## Organisations ##
library(readr)
library(jsonlite)
library(tidyverse)

# Map
library(plotly)

all_countries <- read_csv("all_countries.csv")

merged_countries <- all_countries %>%
  group_by(tolower(name)) %>%
  count()


country <- c()
countries <- c()

for (i in 1:length(claims)) {
  country <- claims[[i]]$contentLocations
  countries <- c(countries, country)
}

countries <- as.data.frame(unlist(countries))

df_countries <- merge(countries, 
                      all_countries, 
                      by.x = "unlist(countries)", 
                      by.y = "@id")

count_by_country <- df_countries %>%
  group_by(name) %>%
  count()

library(countrycode)


count_by_country_weird <- count_by_country
weird <- c("Africa", 
           "Baltic states", 
           "Central Europe", 
           "Chechnya", 
           "CIS", 
           "Daesh", 
           "David Icke", 
           "Eastern Europe", 
           "EU", "Europe", "GDR", "International", "Islam", "Kirgizstan", 
           "Kosovo", "Kurdistan", "Middle East", 
           "Middle East and Africa", "The West", "West", 
           "Yugoslavia")

count_by_country_weird <- subset(count_by_country_weird, count_by_country_weird$name %in% weird)

africa <- data.frame(continents$Three_Letter_Country_Code[continents$Continent_Name=="Africa"], 8)
europe <- data.frame(continents$Three_Letter_Country_Code[continents$Continent_Name=="Europe"], 362)
baltic <- data.frame(c("LV", "LT", "EE"), 116)
eastern <- data.frame(c("BGR", "HUN", "CZE", "UKR", "MDA", "BLR", "RUS", "SVK", "ROu", "POL"), 48)
eu <- data.frame(c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", 
                   "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT",
                   "NLD", "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE", "GBR"), 924)

names(africa) <- c("name", "n")
names(europe) <- c("name", "n")
names(baltic) <- c("name", "n")
names(eastern) <- c("name", "n")
names(eu) <- c("name", "n")

continents <- rbind(africa, europe, baltic, eastern, eu)
continents_count <- continents %>%
  group_by(name) %>%
  summarise(sum(n))

countries_2 <- subset(count_by_country, !(count_by_country$name %in% weird))
countries_2$name <- countrycode(countries_2$name, "country.name", "iso3c")

all_the_countries <- merge(continents_count, countries_2, by = "name", all = T)
all_the_countries[is.na(all_the_countries)] = 0
all_the_countries$all <- all_the_countries$`sum(n)` + all_the_countries$n


# light grey boundaries
l <- list(color = toRGB("black"), width = 0.5)

# specify map projection/options
g <- list(
  showframe = FALSE,
  showcoastlines = T,
  projection = list(type = 'Mercator')
)

p <- plot_geo(all_the_countries) %>%
  add_trace(
    z = ~all, color = ~all, colors = 'Reds',
    text = ~name, locations = ~name, marker = list(line = l)
  ) %>%
  colorbar(title = 'Number of articles') %>%
  layout(
    title = 'Number of articles targeting each country or region',
    geo = g
  )

p





# Total
all_news_articles <- read_csv("all_news_articles.csv")
all_organizations <- read_csv("all_organisations.csv")

merged_a_o <- merge(all_news_articles, 
                    all_organizations, 
                    by.x = "author",
                    by.y = "@id")
total_org <- length((unique(merged_a_o$author)))
total_org





# Most active plot

articles_organisations <- 
  count_by_org <- merged_a_o %>%
  group_by(tolower(name.y)) %>%
  count()

top10 <- head(articles_organisations[order(-articles_organisations$n),], n = 10)
colnames(top10) <- c("Organisation", "Total")

top10_plot <- ggplot(top10) +
  geom_bar(aes(y=Total, 
               x=Organisation), 
           stat="identity", 
           fill = rgb(0.1,0.4,0.5,0.7)) +
  coord_flip() +
  theme_minimal() +
  ggtitle("Number of claims by Organisation") +
  xlab("Organisation") +
  ylab("Number of claims")

top10_plotly <- ggplotly(top10_plot)
top10_plotly





# Topics graph




get_top_topics <- function(organisation) {
  
  all_keywords <- read_csv("all_keywords.csv")
  claims <- jsonlite::read_json("claims.json")
  all_claim_reviews <- read_csv("all_claim_reviews.csv")
  all_organizations <- read_csv("all_organisations.csv")
  all_news_articles <- read_csv("all_news_articles.csv")
  
  org <- organisation
  org_id <- all_organizations$`@id`[all_organizations$name==org]
  articles_by_org <- subset(all_news_articles, all_news_articles$author==org_id)
  claim_by_org <- articles_by_org$claim
  
  keywords <- c()
  for (i in 1:length(claims)) {
    
    if(claims[[i]][["@id"]] %in% claim_by_org) {
      keywords <- c(keywords, claims[[i]][["keywords"]])
    }
  }
  
  keywords_df <- as.data.frame(unlist(keywords))
  
  org_keywords <- merge(all_keywords, keywords_df, 
                        by.x = "@id",
                        by.y = "unlist(keywords)")
  
  keywords_count_org <- org_keywords %>%
    group_by(tolower(name)) %>%
    count()
  
  top_topics <- head(keywords_count_org[order(-keywords_count_org$n),], n = 10)
  colnames(top_topics) <- c("Topic", "Total")
  
  topics_plot <- ggplot(top_topics) +
    geom_bar(aes(y=Total, 
                 x=Topic), 
             stat="identity", 
             fill = rgb(0.1,0.4,0.5,0.7)) +
    coord_flip() +
    theme_minimal() +
    ggtitle(paste0("Top Keywords for the organisation ", organisation)) +
    xlab("Keywords") +
    ylab("Frequency")
  
  topics_plotly <- ggplotly(topics_plot)
  topics_plotly
  
}

get_top_topics("Sputnik Arabic")



# Total article

get_total_articles <- function(organisation) {
  
  org <- organisation
  org_id <- all_organizations$`@id`[all_organizations$name==org]
  articles_by_org <- subset(all_news_articles, all_news_articles$author==org_id)
  length(articles_by_org$X1)
}

get_total_articles("Sputnik Arabic")



# Prefered languge


get_languages <- function(organisation) {
  all_languages <- read_csv("all_languages.csv")
  
  org <- organisation
  org_id <- all_organizations$`@id`[all_organizations$name==org]
  articles_by_org <- subset(all_news_articles, all_news_articles$author==org_id)
  
  languages <- merge(articles_by_org, all_languages, 
                     by.x = "inLanguage", by.y = "@id")
  
  languages_count_org <- languages %>%
    group_by(tolower(name.y)) %>%
    count()
  
  colnames(languages_count_org) <- c("Language", "Total")
  
  language_plot <- ggplot(languages_count_org, aes(x="", y=Total, fill=Language)) +
    geom_bar(stat="identity", width=1, color="white") +
    coord_polar("y", start=0) +
    ggtitle(paste0("Languages of the articles from ", organisation)) +
    theme_void() # remove background, grid, numeric labels
  
  language_plot
  
}

get_languages("Sputnik Arabic")




# Activity over time

dates <- c()

for (i in 1:length(claims)) {
  dates[i] <- claims[[i]]$datePublished
  
}

dates <- gsub('.{18}$', '', dates)
dates_df <- data.frame(dates, 1)

time_series <- dates_df %>% 
  group_by(dates) %>% 
  summarise(frequency = n())


time_series_plot <- ggplot(time_series, aes(x=dates, y=frequency, group = 1)) +
  geom_line( color="steelblue") + 
  geom_point() +
  xlab("") +
  scale_x_discrete(breaks=dates[seq(1,length(dates),by=2000)]) +
  theme_minimal()
time_series_plotly <- ggplotly(time_series_plot)



# word2vec

library(wordVectors)

text <- all_claim_reviews$claimReviewed
text <- tolower(text)
text <- gsub('[[:punct:] ]+',' ', text)

writeLines(text, "summary_text.txt")
if (!file.exists("vectors.bin")) {model = train_word2vec("summary_text.txt","vectors.bin",vectors=200,threads=4,window=12,iter=5,negative_samples=0)} else model = read.vectors("vectors.bin")

model %>% closest_to("france")

nazi = closest_to(model, model["nazi"],150)
fishy = model[[nazi$word, average=F]]
plot(fishy, method="pca",  col = "red")



library(wordcloud)
library(tm, SnowballC)


get_wordcloud <- function(country) {
  
  country <- country
  country_id <- all_countries$`@id`[all_countries$name==country]
  
  
  reviews <- c()
  for (i in 1:length(claims)) {
    
    if((country_id %in% unlist(claims[[i]]$contentLocations))) {
      reviews <- c(reviews, claims[[i]][["claimReview"]])
    }
  }
  
  text <- all_claim_reviews$claimReviewed[all_claim_reviews$`@id` %in% reviews]
  text <- removeWords(text, stopwords("english"))
  corpus <- VCorpus(VectorSource(text))
  ## make lower case
  corpus <- tm_map(corpus, content_transformer(tolower))
  ## remove white space
  corpus <- tm_map(corpus, stripWhitespace)
  ## remove numbers
  corpus <- tm_map(corpus, removeNumbers)
  ## remove punctuation
  corpus <- tm_map(corpus, removePunctuation)
  pal2 <- brewer.pal(8,"Dark2")
  wordcloud(corpus, max.words = 20, scale=c(8,.2), min.freq=3, colors = pal2)
}

get_wordcloud("France")


all_keywords <- read_csv("all_keywords.csv")
claims <- jsonlite::read_json("claims.json")
all_claim_reviews <- read_csv("all_claim_reviews.csv")
all_organizations <- read_csv("all_organisations.csv")
all_news_articles <- read_csv("all_news_articles.csv")
continents <- read.csv("continents")
all_countries <- read_csv("all_countries.csv")
all_languages <- read_csv("all_languages.csv")


# Keywords -> Keyword ID -> Claim ID -> Claim -> Author -> Organisation

claim_key <- list()
v1 <- c()
v2 <- c()
for (i in 1:length(claims)) {
  v1 <-  c(v1, rep(claims[[i]]$`@id`, length(claims[[i]]$keywords)))
  v2 <-  c(v2, claims[[i]]$keywords)
}


df <- data.frame(unlist(v1), unlist(v2))
df2 <- merge(all_claim_reviews, df, by.x = "itemReviewed", by.y = "unlist.v1.")
df3 <- merge(df2, all_keywords, by.x = "unlist.v2.", by.y = "@id")
df4 <- merge(all_news_articles, df3, by.x = "claim", by.y = "itemReviewed")
df5 <- merge(df4, all_organizations, by.x = "author", by.y = "@id")
org_key <- data.frame(df5[,23], df5[,27])

write_csv(df5, "a_lot_of_data.csv")



#et's build an adjacency matrix


# build the graph object
network <- graph_from_adjacency_matrix(data)
# Transform it in a graph format
library(igraph)
network <- graph_from_adjacency_matrix(data)

# Transform it in a JSON format for d3.js
library(d3r)
library(networkD3)
data_json <- d3_igraph(network)

# Save this file
write(data_json, "data.json")
graph_data <- jsonlite::read_json("data.json")


source <- c()
for (i in 1:29635) {
  source <- c(source, graph_data[["links"]][[i]][["source"]])
}

target <- c()
for (i in 1:29635) {
  target <- c(target, graph_data[["links"]][[i]][["target"]])
}

graph_df <- data.frame(source, target)

graph_select <- graph_df %>%
  group_by(source) %>%
  count() %>%
  filter(n > 200)

graph_select <- merge(graph_df, graph_select, by = "source")

graph_select <- subset(graph_select, graph_select$n > 700)

p <- simpleNetwork(graph_df_small, height="100px", width="100px",        
                   Source = 1,                 # column number of source
                   Target = 2,                 # column number of target
                   linkDistance = 10,          # distance between node. Increase this value to have more space between nodes
                   charge = -900,                # numeric value indicating either the strength of the node repulsion (negative value) or attraction (positive value)
                   fontSize = 14,               # size of the node names
                   fontFamily = "serif",       # font og node names
                   linkColour = "#666",        # colour of edges, MUST be a common colour for the whole graph
                   nodeColour = "#69b3a2",     # colour of nodes, MUST be a common colour for the whole graph
                   opacity = 0.9,              # opacity of nodes. 0=transparent. 1=no transparency
                   zoom = T                    # Can you zoom on the figure?
)
p






library(lubridate)


claims_w_emb <- read_csv("claims_w_emb.csv")

d <- claims_w_emb %>%
  arrange(yearPublished)

df_keywords <- d %>%
  mutate(month = floor_date(datePublished, unit="month")) %>%
  select(id, month, keyword_names) %>%
  mutate(keyword_names = str_replace(keyword_names, "\\[", "") %>%
           str_replace("\\]", "") %>%
           str_replace_all("\'", "") %>%
           str_trim() %>%
           str_split(", ") )  %>%
  unnest(keyword_names) 
common_keywords <- df_keywords %>%
  group_by(keyword_names) %>%
  count() %>%
  arrange(-n) %>%
  filter(n >= 100) %>%
  pull(keyword_names)

keyword_cts <-  df_keywords %>%
  filter(keyword_names %in% common_keywords) %>%
  group_by(month, keyword_names) %>%
  count()
keyword_grp <- highlight_key(keyword_cts, ~keyword_names)
p <- ggplot(keyword_grp, aes(x=month, 
                             y=n)) +
  geom_line(col="darkgrey") +
  labs(x="Month",
       y="Number of Articles per Keyword/Month",
       title="Changes in Keywords over Time") +
gg <- ggplotly(p)
highlight(gg, on="plotly_hover", off = "plotly_deselect", color = "red", persistent=FALSE)
