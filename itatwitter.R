library(lubridate)
library(tidyverse)
library(rtweet)
library(leaflet)
library(devtools)
library(htmltools)
library(sf)
library(ggmap)
library(maps)
library(wordcloud2)
library(tm)
library(hrbrthemes)
library(tidytext)
library(htmlwidgets)




api_key <- "C2skGJJmzQCzNBZ3CjVE2COXG"
api_secret_key <- "amYWEq8BmiqWMoEUHzMO1Bz8HTzPVeVOSmHc4xqO7LeFqsdGTs"
access_token <- "1174663297024569344-iDi5HqnUxqDfahA7hxlUIhNhZS4D25"
access_token_secret <- "B7FHSKQwF0uXnLTnMyUYVtigVWY6VC0lB0O3stsK9NC3p"

## authenticate via token
token <- create_token(
  app = "bigdatathingy",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)
tokenb=bearer_token(token)

get_token()




t=theme(
  panel.background = element_rect(fill = "transparent", color = NA), # bg of the panel
  plot.background = element_rect(fill = "transparent", color = NA),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  panel.grid.major = element_line(colour = "transparent"),
  panel.grid.minor = element_blank(),
  axis.title=element_blank(),
  axis.text=element_blank(),
  axis.ticks=element_blank(),
  text = element_text(family= "Franklin Gothica"),
  legend.title = element_text(color = "black", size = 10, face = "bold"),
  legend.text = element_text(color = "black", size = 10, face= "bold"),
  legend.justification = "top",
  legend.key.size =  unit(10, "mm")
  ) 

senato=read.csv("twitterparlamento.csv")
senato=filter(senato, Twitter !="")

camera=read.csv(file ="full_member_info.csv") 
camera=camera[camera$c.country=="Italy",]
camera=camera[camera$lp.legislative_period_id==24,] %>% 
  select(m.name,m.uid, m.party, c.lower_chamber_name) 
  


names(camera)=c("Nome", "Twitter", "Partito", "Tipo")

for (i in 1:630){
  name=lookup_users(camera$Twitter[i], token = tokene)$screen_name
  print(i)
  
  if (length(name)!=0) {
    camera$Twitter[i]=name
  }
}
  
camera %>% group_by(m.party) %>% count()
write.csv(camera, "camera.csv", row.names = FALSE)
 



camera=read.csv(file ="camera.csv") 
senato=read.csv(file="senato.csv")
parlamento=rbind(senato,camera)

parltweet=get_timeline(parlamento$Twitter, n=100, token = tokenb, check = FALSE)

parltweet=filter(parltweet, created_at>=today()) %>% 
  filter(is_retweet==FALSE) %>% 
  filter(is_quote==FALSE)


partiticol=c("Lega"='green',
             "PD"='red',
             "Italia Viva"='#ca2c92',
             "M5S"='yellow', 
             "Ex M5S"='#c1c400', 
             "Forza Italia"='blue',
             "Coraggio Italia"='#600080',
             "CD"='gray',
             "MAIE"='darkblue',
             "Azione-+Europa"='#003399',
             "Verdi"='#008f39',
             "FDI"="#0d0099",
             "LEU"="darkred",
             "Noi con l’Italia"="gold2",
             "SVP"="black",
             "Senatore a vita"="grey",
             "UDC"="skyblue"
             )
camera %>% 
  group_by(Partito) %>% 
  summarise(n()) %>% 
  ggplot(aes(x=Partito, y= `n()`))+geom_col()+theme_ft_rc()

parltweet %>%
  filter(favorite_count > 100) %>% 
  ggplot(aes(x=favorite_count)) +
  geom_density()  +
  labs(x = "Like",
       y = "Count",
       title = "Tweets - Like Distribution ")+
  theme_classic()+
  coord_cartesian(expand = c(0,0))

#freqpoly
ggplot(parltweet, aes(as.POSIXct(created_at,"%Y-%m-%d %H:%M:%S")))+geom_freqpoly(binwidth=60*30)+
  theme_ft_rc()
  xlim(c(Sys.time()-7*60*60*24,Sys.time()-1*60*60*24))

camera$Partito=ifelse(camera$Partito=="Azione/+Europa", "Azione-+Europa", camera$Partito)

write_as_csv(parltweet, file_name = paste(today(),".csv"))

df <- merge(parltweet, senato, by.x = "screen_name", by.y = "Twitter")
df$text <- as.character(df$text)
df$text=gsub("\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)", "", df$text)
df$text <- gsub("\\$", "", df$text) 
df$text <- gsub("@\\w+", "", df$text)
df$text <- gsub("http\\w+", "", df$text)
df$text <- gsub("[[:punct:]]"," ", df$text)
df$text <- gsub('[0-9]+', '', df$text)
unique(df$Partito)




itastop=as_tibble_col(stopwords("italian"), column_name = "word")

tokens <- df %>% 
  group_by(Partito) %>% 
  unnest_tokens(word, text) %>%
  group_by(Partito) %>% 
  count(word, sort = TRUE) %>% 
  anti_join(itastop) %>% 
  ungroup()

tokens
a= tokens %>% 
  slice_max(order_by = n, n=15) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in tweets",
       subtitle = "Stop words removed from the list")
a
tokens$Partito
partiti=unique(na.omit(tokens$Partito))
partiti
for (i in partiti){
  outputfile <- paste(i,"output.html", sep="-")
  colore=partiticol[[i]]
  print(colore)
  print(names(partiticol[i]))
  filter(tokens,Partito==i) %>% 
  ungroup() %>% 
  select(word,n) %>% 
  wordcloud2(shape ='cardiod' , color = colore) %>% 
  saveWidget(outputfile,selfcontained = FALSE)
  }

    b=tokens %>% 
    select(word,n) %>% 
    wordcloud2(size= 1,color = "blue")
  b
  saveWidget(b,"b.html", selfcontained = FALSE)


blue="blue"
r=rate_limit(tokenb)
partiticol[1]
devtools::install_github("lchiffon/wordcloud2")

partiticol[["Italia Viva"]]
names(partiticol)
partiticol
