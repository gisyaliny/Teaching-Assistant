## Advanced Data Programming
# R program to collect Twitter for network analysis

## Acquire API key and token from Twitter developer website
# Check https://datageneration.org/adp/twitter/ for detail

# Create token for direct authentication to access Twitter data
# Enter key and tokens from Twitter developer account 
# (Account--> Apps--> detail--> Keys and tokens)
token <- rtweet::create_token(
  app = "Your App name",
  consumer_key <- "KMbZHX9Hh61GAkwdczQv4AH3p",
  consumer_secret <- "yrbJmQ7FepQIjjJ5mAbrRqxu1gKgCxQl1TeaVtETxWFkvbfSSa",
  access_token <- "893704385972363264-2BqzLU9XWb5uRv5pPbS71FoJ8CEcnu1",
  access_secret <- "ndLy1EukugtnxZvtqsZ1dAgkMExdHjupYAzcPnabU6YYK")

## Check token

rtweet::get_token()

## Install packages need for Twitter data download

##install.packages(c("rtweet","igraph","tidyverse","ggraph","data.table"), repos = "https://cran.r-project.org")

## Load packages

library(rtweet)
library(igraph)
library(tidyverse)
library(ggraph)
library(data.table)

## search for 1000 tweets in English
rdt <- rtweet::search_tweets(q = "realDonaldTrump", n = 100, lang = "en")


## preview users data
users_data(rdt)

## Boolean search for large quantity of tweets (which could take a while)
rdt1 <- rtweet::search_tweets("Trump OR president OR potus", n = 100,
                              retryonratelimit = TRUE)


## plot time series of tweets frequency
ts_plot(rdt1, by = "mins") + theme_bw()

# Graphing retweet connections
## Warning: could take a long time to create the igraph
## Suggestion: start from a smaller data file
## Credit: Russell, Matthew. 2018. 21 Recipes for Mining Twitter Data with rtweet
## https://rud.is/books/21-recipes/visualizing-a-graph-of-retweet-relationships.html


## Create igraph object from Twitter data using user id and mentioned id.
## ggraph draws the network graph in different layouts (12). 

filter(rdt1, retweet_count > 0 ) %>% 
  select(screen_name, mentions_screen_name) %>%
  unnest(mentions_screen_name) %>% 
  filter(!is.na(mentions_screen_name)) %>% 
  graph_from_data_frame() -> rdt_g
V(rdt_g)$node_label <- unname(ifelse(degree(rdt_g)[V(rdt_g)] > 20, names(V(rdt_g)), "")) 
V(rdt_g)$node_size <- unname(ifelse(degree(rdt_g)[V(rdt_g)] > 20, degree(rdt_g), 0)) 

# ggraph layouts: 'star', 'circle', 'gem', 'dh', 'graphopt', 'grid', 'mds', 
# 'randomly', 'fr', 'kk', 'drl', 'lgl'
# Davidson-Harel algorithm
# Try also fr (fruchterman reingold)

ggraph(rdt_g, layout = 'kk') + 
  geom_edge_arc(edge_width=0.1, aes(alpha=..index..)) +
  geom_node_label(aes(label=node_label, size=node_size),
                  label.size=0, fill="#ffffff66", segment.colour="light blue",
                  color="red", repel=TRUE, family="Apple Garamond") +
  coord_fixed() +
  scale_size_area(trans="sqrt") +
  labs(title="Donald Trump Twitter Plot", subtitle="Edges=volume of retweets. Screenname size=influence") +
  theme_graph(base_family="Apple Garamond") +
  theme(legend.position="none") 
