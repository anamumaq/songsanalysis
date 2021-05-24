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

#################################################################################################################

# 1. Hannah Montana 2: Meet Miley Cyrus 2007


t1 = genius_album(artist = "Miley Cyrus", album = "Meet Miley Cyrus")

#unique(t1$track_title)

pend_t1 = filter(t1, is.na(line)) # obtengo las canciones que no tienen letras

pend_songt1 =  genius_tracklist(artist = "Miley Cyrus", album = "Meet Miley Cyrus")%>% 
  filter(track_title %in% pend_t1$track_title) # obtengo url de las canciones sin letras

# obtengo las letras de las canciones que faltaban uno a uno
a2_1 = genius_url(pend_songt1$track_url[1], info = "all")
a2_2 = genius_url(pend_songt1$track_url[2], info = "all")
a2_3 = genius_url(pend_songt1$track_url[3], info = "all")


# uno los data sets en un solo llamado como el albun
HM = bind_rows(t1[c("track_title", "lyric")], 
               a2_1[c("track_title", "lyric")],
               a2_2[c("track_title", "lyric")],
               a2_3[c("track_title", "lyric")])%>%
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

############### 
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




##################



# i = genius_url("https://genius.com/Miley-cyrus-right-here-lyrics", info = "all")



##########################################################################################################

t = genius_album(artist = "Miley Cyrus", album = "Bangerz")

unique(t$track_title)

pend = filter(t, is.na(line)) # obtengo las canciones que no tienen letras

pend_song =  genius_tracklist(artist = "Miley Cyrus", album = "Bangerz")%>% 
  filter(track_title %in% pend$track_title) # obtengo url de las canciones sin letras

# obtengo las letras de las canciones que faltaban uno a uno
a2_1 = genius_url(pend_song$track_url[1], info = "all")
a2_2 = genius_url(pend_song$track_url[2], info = "all")
a2_3 = genius_url(pend_song$track_url[3], info = "all")
a2_4 = genius_url(pend_song$track_url[4], info = "all")
a2_5 = genius_url(pend_song$track_url[5], info = "all")
a2_6 = genius_url(pend_song$track_url[6], info = "all")
a2_7 = genius_url(pend_song$track_url[7], info = "all")


# uno los data sets en un solo llamado como el albun


bgrz = bind_rows(t[c("track_title", "lyric")], 
              a2_1[c("track_title", "lyric")],
              a2_2[c("track_title", "lyric")],
              a2_3[c("track_title", "lyric")],
              a2_4[c("track_title", "lyric")],
              a2_5[c("track_title", "lyric")],
              a2_6[c("track_title", "lyric")],
#              a2_7[c("track_title", "lyric")]
#              ,a2_8[c("track_title", "lyric")]
               )%>%
  mutate(album = "Bangerz")




#Separa las frases a palabras



HM_df = HM %>%
  unnest_tokens(word, lyric)

BO_df = BO %>%
  unnest_tokens(word, lyric) #paso Lyric palabra por palabra a la nueva columna word


TM_df = tm %>%
  unnest_tokens(word, lyric)

CNT_df = cant %>% ### arreglar
  unnest_tokens(word, lyric)

BGRZ_df = bgrz %>%
  unnest_tokens(word, lyric)

#quito la lista de stopwords a las canciones

HM_df = HM_df %>%
  anti_join(stop_words)%>%
  filter(!word %in% c("1", "4", "6", "9","a.m", "yeah","da","sha","la",'hoo'  ))

BO_df = BO_df %>%
  anti_join(stop_words) %>%
  filter(!word %in% c("1", "4", "6", "9","a.m", "yeah","da","sha","la"  ))

TM_df = TM_df %>%
  anti_join(stop_words) %>%
  filter(!word %in% c("1", "4", "3", "6", "9","a.m", "yeah","da","sha","la","u.s.a"  ))

CNT_df = CNT_df %>%
  anti_join(stop_words) %>%
  filter(!word %in% c("1", "4", "3", "6", "9","a.m", "yeah","da","sha","la",
                      "u.s.a","24","ya","yep","li","dnadon't","sh","hey"))


BGRZ_df = BGRZ_df %>%
  anti_join(stop_words) %>%
  filter(!word %in% c("1", "4", "3", "6", "9","a.m", "yeah","da","sha","la",
                      "u.s.a","24","ya","yep","li","dnadon't","sh","hey", "3d",
                      "ay", "b.i.g","bangerz","yup","yell","ya'l","woo","whoa",
                      "what'd","uh", "uhm"))



#contamos la frecuencia de palabras que encontramos
HM_df %>%
  count(word, sort = TRUE)

BO_df %>%
  count(word, sort = TRUE)

TM_df %>%
  count(word, sort = TRUE)

CNT_df %>%
  count(word, sort = TRUE)

BGRZ_df %>%
  count(word, sort = TRUE)

# graficos

CNT_df %>%
  count(word, sort = TRUE)%>% #algunas letras que no van como la da
  filter(n>5,!word %in% c("1", "4", "3", "6", "9","a.m", "yeah","da","sha","la",
                          "u.s.a","24","ya","yep","li","dnadon't","sh","hey", "3d",
                          "ay", "b.i.g","bangerz","yup","yell","ya'l","woo","whoa",
                          "what'd","uh", "uhm"),
         word != is.na(word))%>%
  ggplot(aes(x = reorder(word, n), y = n))+
  geom_col()+
  geom_text(aes(label = reorder(n, n)), 
            hjust = 1.2,vjust = 0.3, color = "white", 
            size = 5)+
  coord_flip()

# Sentimientos

HM_sentimento = HM_df %>%
  inner_join(get_sentiments("bing"))%>% 
  count(album, track_title, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

bo_sentimiento =   BO_df %>%
  inner_join(get_sentiments("bing"))%>% 
  count(album, track_title, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

TM_sentimiento =   TM_df %>%
  inner_join(get_sentiments("bing"))%>% 
  count(album, track_title, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

CNT_sentimiento =   CNT_df %>%
  inner_join(get_sentiments("bing"))%>% 
  count(album, track_title, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

BGRZ_sentimiento =   BGRZ_df %>%
  inner_join(get_sentiments("bing"))%>% 
  count(album, track_title, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

#Joins los albums

MC_alb = bind_rows(BGRZ_sentimiento, CNT_sentimiento, TM_sentimiento, bo_sentimiento,HM_sentimento)

#plot

MC_alb%>%
  filter(album %in% c("Bangerz", "Breakout"))%>%
  ggplot(aes(reorder(track_title, sentiment), sentiment, fill = album)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~album, scales = "free")+
  labs(x = NULL,
       y = "Sentiment",
       title = "Bangerz & Breakout by MC Sentimental Analysis - p1",
       caption = "                  by: Anamumaq")+
  geom_text(aes(label = sentiment), 
             hjust = 1.2,vjust = 0.3, color = "black", 
             size = 3)+
  theme_minimal()+
  
#     theme(plot.title = element_text(size = 13, hjust = 0.4, face = "bold"),
#      axis.title.y = element_text(hjust = 0.05, size = 7, color = "grey40", angle = 0),
#        axis.title.x =  element_text(size = 8, color = "grey40"),
#        axis.text.x = element_text(size = 6.5, color = "grey40"),
#        axis.text.y = element_text(size = 6.5, color = "grey40"), 
#        strip.text = element_text(size = 9, color = "grey40", face = "bold"),
#       plot.caption = element_text(size = 7.5, color = "grey40"))+
  coord_flip()
#       ggsave("sentiment.png", width = 10, height = 5.5)




### comprobando otras canciones en español ##########
#  erreway_1 = genius_tracklist(artist = "Erreway", album = "Senales")
#  RBD = genius_tracklist(artist = "RBD", album = "Rebelde")
