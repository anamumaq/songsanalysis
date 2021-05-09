library(genius)
library(tidyverse)
library(tidytext) #for text tidying

MC_Breakout_tracks = genius_tracklist("Miley Cyrus", "Hannah Montana 2")
MC_Breakout_lyrics = genius_album(artist = "Miley Cyrus")#, album = "Breakout"
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

a1 = genius_tracklist(artist = "Miley Cyrus", album = "Meet Miley Cyrus")%>%
  mutate(evento = "End of Hannah Montana", year = 2007)
a2 = genius_tracklist(artist = "Miley Cyrus", album = "Breakout")%>%
  mutate(evento = "Beak out con Nick Jonas", year = 2008)
a3 = genius_tracklist(artist = "Miley Cyrus", album = "The time of our lives")%>%
  mutate(evento = "worse celebrity influence", year = 2009)
a4 = genius_tracklist(artist = "Miley Cyrus", album = "Can't be tamed")%>%
  mutate(evento = "First independed album", year = 2010)
a5 = genius_tracklist(artist = "Miley Cyrus", album = "Bangerz")%>%
  mutate(evento = "VMA performance twerk", year = 2013)
a6 = genius_tracklist(artist = "Miley Cyrus", album = "Miley Cyrus and her dead Petz")%>%
  mutate(evento = "VMA performance transparent outfit", year = 2015)
a7 = genius_tracklist(artist = "Miley Cyrus", album = "Younger now")%>%
  mutate(evento = "Change style", year = 2017)
a8 = genius_tracklist(artist = "Miley Cyrus", album = "She is coming")%>%
  mutate(evento = "Divorce", year = 2019)
a9 = genius_tracklist(artist = "Miley Cyrus", album = "Plastic hearts")%>%
  mutate(evento = "Divorce", year = 2020)

Album =  rbind(a1, a2, a3, a4, a5, a6, a7, a8, a9)

t1 = genius_album(artist = "Miley Cyrus", album = "Meet Miley Cyrus")%>%
  mutate(album = "Taylor Swift")


# canciones sin letras :()
head(filter(miley_1, is.na(lyric)))

## para las letras que no encuentra directamente hay que obtener el url y buscarlo por ahi
flyonthewall = MC_Breakout_tracks %>% 
  filter(track_title == "Fly on the Wall")

MC_flyonthewall = genius_url(flyonthewall$track_url, info = "all") #Simple regresa la letra de la URL

  # MC_flyonthewall = genius_lyrics(artist = "Miley Cyrus", song = "Fly on the Wall") #resultado NA

MC_1 = MC_Breakout_lyrics%>%
  unnest_tokens(word, lyric) #paso Lyric palabra por palabra a la nueva columna word

#contamos la frecuencia de palabras que encontramos
MC_1 %>%
  count(word, sort = TRUE) #muchas stopwords

head(stop_words) #lista de stopwords de tidytext

#quito la lista de stopwords a las canciones
MC_2 = MC_1 %>%
  anti_join(stop_words)

MC_2 %>%
  count(word, sort = TRUE)%>% #algunas letras que no van como la da
  filter(n > 9, word != "la",word != "da" )%>%
  ggplot(aes(x = reorder(word, n), y = n))+
  geom_col()+
  geom_text(aes(label = reorder(n, n)), 
            hjust = 1.2,vjust = 0.3, color = "white", 
            size = 5)+
  coord_flip()




### comprobando otras canciones en español ##########
#  erreway_1 = genius_tracklist(artist = "Erreway", album = "Senales")
#  RBD = genius_tracklist(artist = "RBD", album = "Rebelde")
