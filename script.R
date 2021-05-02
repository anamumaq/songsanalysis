library(genius)
library(tidyverse)
library(tidytext) #for text tidying

MC_Breakout_tracks = genius_tracklist("Miley Cyrus", "Breakout")
MC_Breakout_lyrics = genius_album(artist = "Miley Cyrus", album = "Breakout")


summary(miley_1)
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
