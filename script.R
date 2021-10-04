library(genius) #pa la canciones
library(tidyverse)
library(tidytext) #for text tidying
#library(ggthemr) la paletas no sirve xd https://github.com/Mikata-Project/ggthemr
#library(ggCyberPunk) #pa los graficos electricos medio guachafos https://github.com/delabj/ggCyberPunk
library(magick) #pa cargar imagenes https://themockup.blog/posts/2019-01-09-add-a-logo-to-your-plot/


#################################################################################################################
##CODIGO SOLO PARA EXTRAER LETRAS DE CANCIONES Y HACER ANALISIS DE SENTIMIENTO###################################
#################################################################################################################
################################# Extrae canciones ########################
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

############################ corriendo ####################
album_miley = "Hannah Montana 2: Meet Miley Cyrus"
t = genius_album(artist = "Miley Cyrus", album = album_miley)

unique(t$track_title)

pend = filter(t, is.na(line)) # obtengo las canciones que no tienen letras

pend_song =  genius_tracklist(artist = "Miley Cyrus", album = album_miley)%>% 
              filter(track_title %in% pend$track_title) # obtengo url de las canciones sin letras
# obtengo las letras de las canciones que faltaban uno a uno
a_1 = genius_url(pend_song$track_url[1], info = "all")#
a_2 = genius_url(pend_song$track_url[2], info = "all")#
a_3 = genius_url(pend_song$track_url[3], info = "all")#
a_4 = genius_url(pend_song$track_url[4], info = "all")#
#a_5 = genius_url(pend_song$track_url[5], info = "all")#
#a_6 = genius_url(pend_song$track_url[6], info = "all")#
#a_7 = genius_url(pend_song$track_url[7], info = "all")#
#a_8 = genius_url(pend_song$track_url[8], info = "all")#
#a_9 = genius_url(pend_song$track_url[9], info = "all")#
#a_10 = genius_url(pend_song$track_url[10], info = "all")
#a_11 = genius_url(pend_song$track_url[11], info = "all")

# uno los data sets en un solo llamado como el albun

################### 1. Hannah Montana###############

HM = bind_rows(t[c("track_title", "lyric")], 
                 a_1[c("track_title", "lyric")],
                 a_2[c("track_title", "lyric")],
                 a_3[c("track_title", "lyric")],
                 a_4[c("track_title", "lyric")]
                 #a_5[c("track_title", "lyric")],
                 #a_6[c("track_title", "lyric")],
                 #              a2_7[c("track_title", "lyric")]
                 #              ,a2_8[c("track_title", "lyric")]
)%>%
  mutate(album = album_miley) #se han quedado celdas vacias pero no es que falte la letra de la cancion

summary(is.na(HM))

HM%>% filter(is.na(HM$lyric)==TRUE)

#################### 1.5: Meet Miley Cyrus 2007###############

MMC = bind_rows(t[c("track_title", "lyric")], 
               a_1[c("track_title", "lyric")],
               a_2[c("track_title", "lyric")],
               a_3[c("track_title", "lyric")],
               a_4[c("track_title", "lyric")]
               #a2_5[c("track_title", "lyric")],
               #a2_6[c("track_title", "lyric")],
               #              a2_7[c("track_title", "lyric")]
               #              ,a2_8[c("track_title", "lyric")]
)%>%
  mutate(album = album_miley)

summary(is.na(MMC))

MMC%>% filter(is.na(MMC$lyric)==TRUE)


#################### 2. Breakout 2008################

BO = bind_rows(t[c("track_title", "lyric")], 
               a_1[c("track_title", "lyric")],
               a_2[c("track_title", "lyric")],
               a_3[c("track_title", "lyric")],
               a_4[c("track_title", "lyric")],
               a_5[c("track_title", "lyric")]
               #a2_6[c("track_title", "lyric")],
               #              a2_7[c("track_title", "lyric")]
               #              ,a2_8[c("track_title", "lyric")]
      )%>%
  mutate(album = album_miley)

summary(is.na(BO))

BO%>% filter(is.na(BO$lyric)==TRUE)


####################3. The time of our lives 2009################
ttool = bind_rows(t[c("track_title", "lyric")], 
               a_1[c("track_title", "lyric")],
               a_2[c("track_title", "lyric")],
               a_3[c("track_title", "lyric")],
               a_4[c("track_title", "lyric")],
               #a2_5[c("track_title", "lyric")],
               #a2_6[c("track_title", "lyric")],
               #              a2_7[c("track_title", "lyric")]
               #              ,a2_8[c("track_title", "lyric")]
)%>%
  mutate(album = album_miley)

summary(is.na(ttool))
ttool%>% filter(is.na(ttool$lyric)==TRUE)

#################### 4. Can't be tamed 2010###################
cbt = bind_rows(t[c("track_title", "lyric")], 
                  a_1[c("track_title", "lyric")],
                  a_2[c("track_title", "lyric")],
                  a_3[c("track_title", "lyric")],
                  a_4[c("track_title", "lyric")],
                  a_5[c("track_title", "lyric")],
                  #a2_6[c("track_title", "lyric")],
                  #              a2_7[c("track_title", "lyric")]
                  #              ,a2_8[c("track_title", "lyric")]
)%>%
  mutate(album = album_miley)

summary(is.na(cbt))
cbt%>% filter(is.na(cbt$lyric)==TRUE)

#################### 5. Bangerz 2013 ####################
bgrz = bind_rows(t[c("track_title", "lyric")], 
              a_1[c("track_title", "lyric")],
              a_2[c("track_title", "lyric")],
              a_3[c("track_title", "lyric")],
              a_4[c("track_title", "lyric")],
              a_5[c("track_title", "lyric")],
              a_6[c("track_title", "lyric")],
              a_7[c("track_title", "lyric")],
              a_8[c("track_title", "lyric")],
              )%>%
  mutate(album = album_miley)

summary(is.na(bgrz))
bgrz%>% filter(is.na(bgrz$lyric)==TRUE)
############## 6. Miley Cyrus and her dead Petz 2015#############
petz = bind_rows(t[c("track_title", "lyric")], 
                 a_1[c("track_title", "lyric")],
                 a_2[c("track_title", "lyric")],
                 a_3[c("track_title", "lyric")],
                 a_4[c("track_title", "lyric")],
                 a_5[c("track_title", "lyric")],
                 a_6[c("track_title", "lyric")],
                 a_7[c("track_title", "lyric")],
                 a_8[c("track_title", "lyric")],
                 a_9[c("track_title", "lyric")],
                 a_10[c("track_title", "lyric")],
                 a_11[c("track_title", "lyric")]
)%>%
  mutate(album = album_miley)


summary(is.na(petz))
petz%>% filter(is.na(petz$lyric)==TRUE)
############### 7. Younger now 2017 ############## 
yn = bind_rows(t[c("track_title", "lyric")], 
               a_1[c("track_title", "lyric")],
               a_2[c("track_title", "lyric")],
               a_3[c("track_title", "lyric")],
               a_4[c("track_title", "lyric")]
               #a_5[c("track_title", "lyric")],
               #a_6[c("track_title", "lyric")],
               #              a2_7[c("track_title", "lyric")]
               #              ,a2_8[c("track_title", "lyric")]
)%>%
  mutate(album = album_miley)

summary(is.na(yn))
yn%>% filter(is.na(yn$lyric)==TRUE)
###############  8. She is coming 2019 ############## 
sic = bind_rows(t[c("track_title", "lyric")], 
                a_1[c("track_title", "lyric")],
                a_2[c("track_title", "lyric")]
                #a2_3[c("track_title", "lyric")],
                #a2_4[c("track_title", "lyric")],
                #a2_5[c("track_title", "lyric")],
                #a2_6[c("track_title", "lyric")],
                #              a2_7[c("track_title", "lyric")]
                #              ,a2_8[c("track_title", "lyric")]
)%>%
  mutate(album = album_miley)

summary(is.na(sic))

sic%>% filter(is.na(sic$lyric)==TRUE)

###############  9. Plastic hearts 2020 ############## 
ph = bind_rows(t[c("track_title", "lyric")], 
                 a_1[c("track_title", "lyric")],
                 a_2[c("track_title", "lyric")],
                 a_3[c("track_title", "lyric")],
                 a_4[c("track_title", "lyric")],
                 a_5[c("track_title", "lyric")],
                 a_6[c("track_title", "lyric")],
                 a_7[c("track_title", "lyric")],
                 a_8[c("track_title", "lyric")],
                 a_9[c("track_title", "lyric")]
)%>%
  mutate(album = album_miley)

summary(is.na(ph))
ph%>% filter(is.na(ph$lyric)==TRUE)

######################Separa las frases a palabras##################

HM_df = HM %>%
  unnest_tokens(word, lyric)

MMC_df = MMC %>%
  unnest_tokens(word, lyric)

BO_df = BO %>%
  unnest_tokens(word, lyric) #paso Lyric palabra por palabra a la nueva columna word

ttool_df = ttool %>%
  unnest_tokens(word, lyric)

cbt_df = cbt %>%
  unnest_tokens(word, lyric)

BGRZ_df = bgrz %>%
  unnest_tokens(word, lyric)

petz_df = petz %>%
  unnest_tokens(word, lyric)

yn_df = yn %>%
  unnest_tokens(word, lyric)

sic_df = sic %>%
  unnest_tokens(word, lyric)

ph_df = ph %>%
  unnest_tokens(word, lyric)
#########################quito la lista de stopwords a las canciones###################
mis_stopwords = c("1", "4", "3", "6", "9","a.m", "yeah","da","sha","la",
                  "u.s.a","24","ya","yep","li","dnadon't","sh","hey", "3d",
                  "ay", "b.i.g","bangerz","yup","yell","ya'l","woo","whoa",
                  "what'd","uh", "uhm","til","bff","ahh","i'ma", "ooh","na","ah","15","aw","7am","8am's","7",
                  "pum","16th","500","ayy","ali","that'll","1916","1969","aah","'s","chi","tai","ups","st","karen",
                  "mu'fucker","em","goo","hmm","y'all","yah","mmm","hm","whoo")

#1 .Hannah Montana
HM_df = HM_df %>%
  anti_join(stop_words)%>%
  filter(!word %in% mis_stopwords)%>%
  group_by(album, track_title,word)%>% 
  summarise(n = n())

## 1.5: Meet Miley Cyrus ##3 no cuent

#2. Breakout

BO_df = BO_df %>%
  anti_join(stop_words) %>%
  filter(!word %in% mis_stopwords) %>%
  group_by(album, track_title,word)%>% 
  summarise(n = n())

#3. The time of our lives
ttool_df = ttool_df %>%
  anti_join(stop_words) %>%
  filter(!word %in% mis_stopwords)%>%
  group_by(album, track_title,word)%>% 
  summarise(n = n())

# 4. Can't be tamed
cbt_df = cbt_df %>%
  anti_join(stop_words) %>%
  filter(!word %in% mis_stopwords)%>%

  group_by(album, track_title,word)%>% 
  summarise(n = n())

# 5. Bangerz
BGRZ_df = BGRZ_df %>%
  anti_join(stop_words) %>%
  filter(!word %in% mis_stopwords)%>%
  group_by(album, track_title,word)%>% 
  summarise(n = n())

# 6. Miley Cyrus and her dead Petz

petz_df = petz_df %>%
  anti_join(stop_words) %>%
  filter(!word %in% mis_stopwords)%>%
  group_by(album, track_title,word)%>% 
  summarise(n = n())

# 7. Younger now 
yn_df = yn_df %>%
  anti_join(stop_words) %>%
  filter(!word %in% mis_stopwords)%>%
  group_by(album, track_title,word)%>% 
  summarise(n = n())

# 8. She is coming

sic_df = sic_df %>%
  anti_join(stop_words) %>%
  filter(!word %in% mis_stopwords)%>%
  group_by(album, track_title,word)%>% 
  summarise(n = n())

#  9. Plastic hearts

ph_df = ph_df %>%
  anti_join(stop_words) %>%
  filter(!word %in% mis_stopwords)%>%
  group_by(album, track_title,word)%>% 
  summarise(n = n())

## Unir todos en una sola base ----

word_count_MC = 
  bind_rows(HM_df,BO_df,ttool_df,cbt_df,BGRZ_df,petz_df,yn_df,sic_df,ph_df)

## Guardando el contador de palabras ----

write.csv(word_count_MC,"word_count_MC.csv")

#### graficos de barras para contar la frecuencia de palabras ##pruebas----

HM_df %>%
  count(word, sort = TRUE)%>% 
  filter(n>10,!word %in% mis_stopwords,
         word != is.na(word))%>%
  ggplot(aes(x = reorder(word, +n, sum), y = n))+
  geom_col(fill="#B38EBF")+
  geom_text(aes(label = reorder(n, n)), 
            hjust = 1.2,vjust = 0.3, color = "white", 
            size = 5)+
  theme(axis.ticks.x=element_blank(), 
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background= element_rect(fill = "white"),
        panel.grid.major =element_blank(),
        panel.grid.minor =element_blank(),
        axis.text.y = element_text(face="bold", color="#EEBE12",size=14),
        plot.title = element_text(size = rel(2)),
        plot.background = element_rect(fill = "white"))+
  ggtitle("Frecuencia de palabras disco HM", subtitle = "Apartir de palabras con frecuencia mayor a 10")+
  coord_flip()

library(wordcloud2)

HM_df %>%
  count(word, sort = TRUE)%>% 
  filter(n>3,!word %in% mis_stopwords,
         word != is.na(word))%>%
wordcloud2(size=1)



HM_sentimento %>% 
  ggplot(aes(reorder(track_title, sentiment), sentiment,label=sentiment)) +
  geom_point(stat='identity', color="#EEBE12", size=8)  +
  geom_hline(yintercept = 0, linetype="dotted", color = "#B38EBF",size=0.5)+
  geom_segment(aes(y = 0, 
                   x = track_title, 
                   yend = sentiment, 
                   xend = track_title), 
               color = "#EEBE12", ) +
  geom_text(color = "black", size = 4)+
  theme(axis.ticks.x=element_blank(), 
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background= element_rect(fill = "white"),
        panel.grid.major =element_blank(),
        panel.grid.minor =element_blank(),
        axis.text.y = element_text(face="bold", color="#B38EBF",size=9),
        plot.title = element_text(size = rel(2)),
        plot.background = element_rect(fill = "white"))+
  ylim(-16, 10) +
  coord_flip()


################ CONECTANDO LOS Sentimientos EN CADA ALBUM  ###################

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

ttool_sentimiento =   ttool_df %>%
  inner_join(get_sentiments("bing"))%>% 
  count(album, track_title, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

cbt_sentimiento =   cbt_df %>%
  inner_join(get_sentiments("bing"))%>% 
  count(album, track_title, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

BGRZ_sentimiento =   BGRZ_df %>%
  inner_join(get_sentiments("bing"))%>% 
  count(album, track_title, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)


petz_sentimiento =   petz_df %>%
  inner_join(get_sentiments("bing"))%>% 
  count(album, track_title, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)


yn_sentimiento =   yn_df %>%
  inner_join(get_sentiments("bing"))%>% 
  count(album, track_title, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)



sic_sentimiento =   sic_df %>%
  inner_join(get_sentiments("bing"))%>% 
  count(album, track_title, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)



ph_sentimiento =   ph_df %>%
  inner_join(get_sentiments("bing"))%>% 
  count(album, track_title, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)


################JOIN LOS ALBUNES SENTIMIENTO EN UNO SOLO #################

sentiment_MC = 
  bind_rows(HM_sentimento, bo_sentimiento, ttool_sentimiento,cbt_sentimiento,BGRZ_sentimiento,yn_sentimiento,sic_sentimiento,ph_sentimiento)

################ guardando dataset analisis de sentimiento --------------
write.csv(sentiment_MC,"sentiment_MC.csv")

######################### PLOT ALBUM COMPLETO COMPRATIVO ################################### no hare #####

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
       #ggsave("Bangerz & Breakout by MC Sentimental Analysis - p1", width = 10, height = 5.5)



