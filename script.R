library(genius)
library(tidyverse)
library(tidytext) #for text tidying

# albunes
# 9. Plastic hearts 2020
# 8. She is coming 2019 
# 7. Younger now 2017
# 6. Miley Cyrus and her dead Petz 2015
# 5. Bangerz 2013
# Hannah Montana Forever  - no tomo en cuenta
# 4. Can't be tamed 2010
# 3. The time of our lives 2009
# 2. Breakout 2008
# 1. Hannah Montana 2: Meet Miley Cyrus 2007


# 1. Hannah Montana 2: Meet Miley Cyrus 2007


t1 = genius_album(artist = "Miley Cyrus", album = "Meet Miley Cyrus")

#unique(t1$track_title)

pend_t1 = filter(t1, is.na(line)) # obtengo las canciones que no tienen letras

pend_song =  genius_tracklist(artist = "Miley Cyrus", album = "Meet Miley Cyrus")%>% 
  filter(track_title %in% pend_t1$track_title) # obtengo url de las canciones sin letras

# obtengo las letras de las canciones que faltaban uno a uno
a1_1 = genius_url(pend_song$track_url[1], info = "all")
a1_2 = genius_url(pend_song$track_url[2], info = "all")
a1_3 = genius_url(pend_song$track_url[3], info = "all")
a1_4 = genius_url(pend_song$track_url[4], info = "all")
a1_5 = genius_url(pend_song$track_url[5], info = "all")
a1_6 = genius_url(pend_song$track_url[6], info = "all")
a1_7 = genius_url(pend_song$track_url[7], info = "all")

# uno los data sets en un solo llamado como el albun
HM = bind_rows(t1[c("track_title", "lyric")], 
          a1_1[c("track_title", "lyric")], a1_2[c("track_title", "lyric")],
          a1_3[c("track_title", "lyric")], a1_4[c("track_title", "lyric")],
          a1_5[c("track_title", "lyric")], a1_6[c("track_title", "lyric")],
          a1_7[c("track_title", "lyric")])%>%
    mutate(album = "Meet Miley Cyrus")

# 2. Breakout 2008


t2 = genius_album(artist = "Miley Cyrus", album = "Breakout")
pend_t2 = filter(t2, is.na(line))

pend_song =  genius_tracklist(artist = "Miley Cyrus", album = "Breakout")%>% 
  filter(track_title %in% pend_t2$track_title)

a1_1 = genius_url(pend_song$track_url[1], info = "all")
a1_2 = genius_url(pend_song$track_url[2], info = "all")
a1_3 = genius_url(pend_song$track_url[3], info = "all")

a1_4 = genius_url(pend_song$track_url[4], info = "all")
a1_5 = genius_url(pend_song$track_url[5], info = "all")
a1_6 = genius_url(pend_song$track_url[6], info = "all")
a1_7 = genius_url(pend_song$track_url[7], info = "all")
a1_8 = genius_url(pend_song$track_url[8], info = "all")

BO = bind_rows(t2[c("track_title", "lyric")], 
               a1_1[c("track_title", "lyric")], a1_2[c("track_title", "lyric")],
               a1_3[c("track_title", "lyric")])%>%
  mutate(album = "Breakout")

# 2. Breakout 2008

#creando una funciona para los otros dicos

song_url = function(alb){
  
  let = genius_album(artist = "Miley Cyrus", album = alb)
  
  pend_let = filter(let, is.na(line)) # obtengo las canciones que no tienen letras
  
  x =  genius_tracklist(artist = "Miley Cyrus", album = alb)%>% 
    filter(track_title %in% pend_let$track_title) 
  return(x)
}


pend_song = song_url("Breakout")

a1_1 = genius_url(pend_song$track_url[1], info = "all")
a1_2 = genius_url(pend_song$track_url[2], info = "all")
a1_3 = genius_url(pend_song$track_url[3], info = "all")
a1_4 = genius_url(pend_song$track_url[4], info = "all")
a1_5 = genius_url(pend_song$track_url[5], info = "all")
a1_6 = genius_url(pend_song$track_url[6], info = "all")
a1_7 = genius_url(pend_song$track_url[7], info = "all")

BO = bind_rows(t1[c("track_title", "lyric")], 
               a1_1[c("track_title", "lyric")], a1_2[c("track_title", "lyric")],
               a1_3[c("track_title", "lyric")], a1_4[c("track_title", "lyric")],
               a1_5[c("track_title", "lyric")], a1_6[c("track_title", "lyric")],
               a1_7[c("track_title", "lyric")])%>%
  mutate(album = "Breakout")


# i = genius_url("https://genius.com/Miley-cyrus-right-here-lyrics", info = "all")




# evaluando lyrics
unique(t2$track_title)
summary(t3)




HM_df = HM %>%
  unnest_tokens(word, lyric)

BO_df = BO %>%
  unnest_tokens(word, lyric) #paso Lyric palabra por palabra a la nueva columna word




#quito la lista de stopwords a las canciones
HM_df = HM_df %>%
  anti_join(stop_words)

BO_df = BO_df %>%
  anti_join(stop_words) %>%
  filter(word != "la",word != "da", word != "sha", word != "yeah")

#contamos la frecuencia de palabras que encontramos
HM_df %>%
  count(word, sort = TRUE)

BO_df %>%
  count(word, sort = TRUE)


HM_df %>%
  count(word, sort = TRUE)%>% #algunas letras que no van como la da
  filter(n > 10, word != "la",word != "da",word != "hoo",word != "yeah")%>%
  ggplot(aes(x = reorder(word, n), y = n))+
  geom_col()+
  geom_text(aes(label = reorder(n, n)), 
            hjust = 1.2,vjust = 0.3, color = "white", 
            size = 5)+
  coord_flip()

tay_sentiment <- HM_df %>%
  inner_join(get_sentiments("bing"))%>% 
  count(album, track_title, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

bo_sentimiento =   BO_df %>%
  inner_join(get_sentiments("bing"))%>% 
  count(album, track_title, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)


### comprobando otras canciones en español ##########
#  erreway_1 = genius_tracklist(artist = "Erreway", album = "Senales")
#  RBD = genius_tracklist(artist = "RBD", album = "Rebelde")
