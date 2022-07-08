# Instalar el paquete tweetbotornot (Tuvimos que subirlo a Github porque el código de origen tenía un error)
library(devtools)
install_github("elcerebrohabla/tweetbotornot")

## Especificar locale ----
Sys.setlocale("LC_ALL", "es_ES.UTF-8")

## Desabilitar notación científica.----
options(scipen = 999)
# Paquetes
library(pacman)
p_load(readxl, tidyverse, dplyr, cowplot, janitor, lmtest, ldatuning, plyr,
     sandwich, sjPlot, pander, pscl, haven, gridExtra, ggExtra, ggrepel,
     hexbin, janitor, mosaicData, scales, ggthemes, tweetbotornot, rtweet,
     lubridate, hms, tidytext, wordcloud2, tm, SnowballC, htmlTable, kableExtra,
     magick, magrittr, scales, textdata, syuzhet, visNetwork, stringi, topicmodels)

#ÃPI de Twitter
token <- create_token(
  "My Application Name",
  consumer_key = "sb0uJ9Jmc7zLdGku8VCzoiO31",
  consumer_secret = "o0eypcW3omjwqykNybpQGGFKukDVdOGyXnm3P1tLCxg1fJ8arK",
  access_token = "16505741-57TJLiUXDoaPyy8MSKBNYtOhCCMHNxZN4elVQHHQ2",
  access_secret = "FawgsbomyHBMadng9Wy6SIzy5z5X1Bce6ZCXbG6h73a45"
)

get_token()

#FUNCIONES

# Stopwords ----
custom_stop_words <- as_data_frame(tm::stopwords("es")) %>% 
  bind_rows(as_data_frame(c(
    "si","dijo","así","sólo", "dice", "pues","entonces",
    "ahí","digo","creo","que","en","la","ah","bueno", "bla","tan",
    "te", "iba", "he", "él", "t", "+", "de", "cómo", "su", "https", "t.co","account",
    "is","temporary","unavailable","media","policy","because","it","violates","learn"
  ))) %>% 
  dplyr::rename(palabra = value)

stopwords <- as.matrix(custom_stop_words)

#datos_revocacion <- read_excel("01_datos/datos_revocacion.xlsx")

#------ Proceso --------

#Bases de datos

sublevados <- read_csv("01_datos/sublevados.csv") %>% 
  mutate(cuenta = "sublevados")
lia <- read_csv("01_datos/sublevados_lia.csv") %>% 
  mutate(cuenta = "Lía Trueba")
camacho <- read_csv("01_datos/sublevados_camacho.csv") %>% 
  mutate(cuenta = "Christian Camacho")
mario <- read_csv("01_datos/sublevados_mario.csv") %>% 
  mutate(cuenta = "Mario")
emilio <- read_csv("01_datos/sublevados_emilio.csv") %>% 
  mutate(cuenta = "Emilio")
respuestas <- read_csv("01_datos/sublevados_respuestas.csv") %>% 
  mutate(cuenta = "respuestas",
         mencionados = NA)


todo <- rbind(sublevados, lia, camacho, mario, emilio, respuestas)
todas_las_cuentas <- rbind(sublevados, lia, camacho, mario, emilio)
personas <- rbind(lia, camacho, mario, emilio)
reaccion_sublevados <- rbind(sublevados, respuestas)

glimpse(todo)

todo %>% 
  dplyr::select(cuenta) %>% 
  group_by(cuenta) %>% 
  count()

todo %>% 
  dplyr::select(cuenta, like) %>% 
  group_by(cuenta) %>%
  dplyr::summarize(n = n(), likes_promedio = mean(like)) %>% 
  ggplot(aes(reorder(cuenta, n), n, label = n)) +
  geom_col() +
  geom_label()+
  labs(title = 'Total tuits',
       subtitle = 'Desde enero 2020',
       caption = "Fuente: API de Twitter",
       x = "Usuario",
       y = "Tuits")+
  #scale_y_continuous(breaks = c(1,2), limits = c(1, 2))+
  theme_economist()+
  theme(
    plot.title = element_text(size = 26, face = "bold"),
    legend.title = element_text(size = 18),
    legend.text = element_text(size=14),
    legend.position = "bottom",
    plot.subtitle = element_text(size = 16),
    plot.caption = element_text(size = 12),
    axis.text = element_text(size = 14),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size= 14))

procesado <- respuestas %>% 
  filter(is.na(mencionados)) %>% 
  separate(Datetime, into = c('date', 'time'), sep=' ', remove = FALSE) %>% 
  dplyr::select(date, time, Text, User, like, mencionados, Datetime, cuenta) %>% 
  mutate(date = as.Date(date))

procesadomenciones <- respuestas %>% 
  filter(!is.na(mencionados)) %>% 
  separate(Datetime, into = c('date', 'time'), sep=' ', remove = FALSE) %>% 
  dplyr::select(date, time, Text, User, like, mencionados, Datetime) %>% 
  mutate(date = as.Date(date)) 
  #mutate(id = row_number())



#Opcional
procesado <- procesado %>% 
  filter(User == "Sublevados_")

#--------- Likes por Tweet -----------

procesado %>% 
  filter(date > '2021-01-01') %>% 
  ggplot(aes(date, like)) +
  geom_smooth(se = F) +
  labs(title = 'Promedio likes por tweet',
       subtitle = '',
       caption = "Fuente: API de TWitter",
       x = "Fecha",
       y = "Likes")+
  #scale_y_continuous(breaks = c(1,2), limits = c(1, 2))+
  theme_economist()+
  scale_x_date(labels = date_format("%m-%Y"))+
  theme(
    plot.title = element_text(size = 26, face = "bold"),
    legend.title = element_text(size = 18),
    legend.text = element_text(size=14),
    legend.position = "bottom",
    plot.subtitle = element_text(size = 16),
    plot.caption = element_text(size = 12),
    axis.text = element_text(size = 14),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size= 14))+
  facet_grid(cols = vars(User), scales = "free")

procesado

promedio %>% 
  mutate(date = as.Date(date)) %>% 
  ggplot(aes(date, promedio_likes)) +
  geom_smooth(se = FALSE)+
  geom_point()

glimpse(promedio)  

#BIGRAMAS

limpiar <- function(texto){
  # El orden de la limpieza no es arbitrario
  # Se convierte todo el texto a minúsculas
  nuevo_texto <- tolower(texto)
  # Eliminación de páginas web (palabras que empiezan por "http." seguidas 
  # de cualquier cosa que no sea un espacio)
  nuevo_texto <- str_replace_all(nuevo_texto,"http\\S*", "")
  # Eliminación de signos de puntuación
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:punct:]]", " ")
  # Eliminación de números
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:digit:]]", " ")
  # Eliminación de espacios en blanco múltiples
  nuevo_texto <- str_replace_all(nuevo_texto,"[\\s]+", " ")
  return(nuevo_texto)
  }

bigramas <- procesado %>%
  #filter(User == "_AvilesAlvarez") %>% 
  dplyr::select(Text) %>% 
  #mutate(texto = limpiar(cuentas)) %>%
  #select(texto) %>%
  unnest_tokens(input = Text, output = "bigrama",
                token = "ngrams",n = 2, drop = TRUE) %>% 
  separate(bigrama, c("palabra1", "palabra2"),
           sep = " ") %>% 
  filter(!palabra1 %in% stopwords) %>%
  filter(!palabra2 %in% stopwords) %>% 
  unite(palabra1, palabra2,col="bigrama",sep=" ") %>% 
  group_by(bigrama) %>% 
  count() %>% 
  arrange(desc(freq)) 

bigramas %>% 
  wordcloud2(size=2.5, color='random-dark')

bigramas %>% 
  filter(bigrama != "NA NA" & bigrama != "cada vez" & bigrama != "debe ser"
         & bigrama != "00 pm" & bigrama != "20 00") %>% 
  top_n(20) %>% 
  kbl() %>%
  kable_styling()

bigramas %>% 
  filter(!str_detect(bigrama, "sublevados_")) %>% 
  filter(bigrama != "NA NA" & bigrama != "cada vez" & bigrama != "puede ser"
         & bigrama != "00 pm" & bigrama != "20 00" & bigrama != "8 00"
         & bigrama != "debe ser" & bigrama != "8 00" & bigrama != "hace falta" & 
           bigrama != "cada día" & bigrama != "tal vez") %>% 
  top_n(20) %>% 
  ggplot(aes(reorder(bigrama, freq), freq, fill="salmon")) +
  geom_col() +
  coord_flip() +
  labs(title = 'Bigramas más mencionados\nen respuestas a Sublevados',
       subtitle = '',
       caption = "Fuente: API de TWitter",
       x = "Términos",
       y = "Frecuencia")+
  #scale_y_continuous(breaks = c(1,2), limits = c(1, 2))+
  theme_economist()+
  theme(
    plot.title = element_text(size = 26, face = "bold"),
    legend.title = element_text(size = 18),
    legend.text = element_text(size=14),
    legend.position = "bottom",
    plot.subtitle = element_text(size = 16),
    plot.caption = element_text(size = 12),
    axis.text = element_text(size = 14),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size= 14))


#Wordcloud
procesado %>% 
  filter(User == "salomonj") %>% 
  dplyr::select(Text) %>% 
  unnest_tokens(input = Text, output = "text",
                token = "ngrams",n = 1, drop = TRUE) %>% 
  group_by(text) %>% 
  count() %>% 
  ungroup() %>% 
  filter(!text %in% stopwords) %>% 
  arrange(desc(n)) %>% 
  wordcloud2(size=2.5, color='random-dark')

cuentas %>% 
  separate(Datetime, into = c('date', 'time'), sep=' ', remove = FALSE)  %>% 
  mutate(date = as.Date(date)) %>% 
  group_by(date) %>% 
  count() %>% 
  ggplot(aes(date, n)) +
  geom_line()+
  geom_point()

total <- cuentas %>% 
  group_by(User) %>% 
  count() %>% 
  arrange(desc(n))

cuentas

mas_populares <- cuentas %>% 
  arrange(desc(like)) %>% 
  dplyr::select(Text, User, like) %>% 
  top_n(100) %>% 
  kbl() %>%
  kable_styling()


#------------Sentimientos--------------

download.file("https://raw.githubusercontent.com/jboscomendoza/rpubs/master/sentimientos_afinn/lexico_afinn.en.es.csv",
              "lexico_afinn.en.es.csv")

afinn <- read.csv("lexico_afinn.en.es.csv", stringsAsFactors = F, fileEncoding = "latin1") %>% 
  tbl_df()

sentimientos <- 
  procesado %>%
  separate(date, into = c("Periodo", "Mes", "Día"), sep = "-",
           remove = FALSE)

sentimientos_afinn <- 
  sentimientos %>%
  unnest_tokens(input = "Text", output = "Palabra") %>%
  inner_join(afinn, ., by = "Palabra") %>%
  mutate(Tipo = ifelse(Puntuacion > 0, "Positiva", "Negativa")) 

sentimientos_afinn <-
  sentimientos_afinn %>%
  filter(Palabra != "no") 
  
glimpse(sentimientos_afinn)

sentimientos_afinn_fecha <-
  sentimientos_afinn %>%
  rename(Fecha = date) %>% 
  group_by(id) %>%
  mutate(Suma = mean(Puntuacion)) %>%
  group_by(cuenta, Fecha) %>%
  summarise(Media = mean(Puntuacion))

sentimientos_afinn_fecha %>%
  ggplot() +
  aes(Fecha, Media, color = cuenta) +
  geom_smooth(se = F) + 
  labs(title = 'Sentimientos de los tuits',
       subtitle = '',
       caption = "Fuente: API de TWitter",
       x = "Fecha",
       y = "<- Negativo - Positivo ->")+
  #scale_y_continuous(breaks = c(1,2), limits = c(1, 2))+
  theme_economist()+
  scale_x_date(labels = date_format("%m-%Y"))+
  theme(
    plot.title = element_text(size = 26, face = "bold"),
    legend.title = element_text(size = 18),
    legend.text = element_text(size=14),
    legend.position = "bottom",
    plot.subtitle = element_text(size = 16),
    plot.caption + 
  labs(title = 'Sentimientos de los tuits',
       subtitle = '',
       caption = "Fuente: API de TWitter",
       x = "Fecha",
       y = "<- Negativo - Positivo ->")+
  #scale_y_continuous(breaks = c(1,2), limits = c(1, 2))+
  theme_economist()+
  scale_x_date(labels = date_format("%m-%Y"))+
  theme(
    plot.title = element_text(size = 26, face = "bold"),
    legend.title = element_text(size = 18),
    legend.text = element_text(size=14),
    legend.position = "bottom",
    plot.subtitle = element_text(size = 16),
    plot.caption = element_text(size = 12),
    axis.text = element_text(size = 14),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size= 14))
= element_text(size = 12),
    axis.text = element_text(size = 14),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size= 14))

#-------Relación de palabras---------------

religión <- c("dios","católico","catolicismo","protestante","iglesia","rezar","navidad","orar")
familia <- c("familia","padre","madres","esposos","matrimonio")
derechos_personales =	c("ciudadana",	"ciudadano",	"ciudadanía",	"contratos",	"democracia",	"derechos",	"elecciones",	"electoral",	"negocios",	"participación",	"propiedad")
libertad_personal =	c("adolescente",	"burocracia",	"corrupción",	"embarazo",	"informal",	"informalidad",	"jóvenes",	"juventud",	"juventudes",	"laboral",	"ninis",	"transporte",
                      "libertad","justicia")
inclusión =	c("discapacidad",	"discapacidades",	"discriminación",	"diversidad",	"homofobia",	"inclusión",	"indígena",	"indígenas",	"paridad",
              "equidad","igualdad","cuotas","género")
luchas <- c("derechos", "igualdad", "equidad", "desigualdad", "feminismo", "aborto","racismo")
protestas <- c("antifeministas","vidrios","queman","vallas","grafiti","protestas",
              "policía","monumento","marcha")
adversarios <- c("feminazi","feminazis", "progresistas","progres","woke","cancelación","corrección política",
                 "lgbt", "feministas")
seguridad <- c("seguridad","crimen", "armas", "violencia", "defensa")
regimen <- c("AMLO","López Obrador", "4T", "MORENA", "Scheinbaum","régimen",
             "Peje")
oposición <- c("PRI","PAN", "oposición", "MC", "Movimiento Ciudadano")
pandemia <- c("pandemia","covid","Covid","vacunas","vacuna")
populistas <- c("Trump","Orban", "Bolsonaro", "Vox", "Abascal")

tempo <- procesado %>% 
  mutate(religión= str_detect(string = Text, pattern = paste(religión, collapse = "|")),
         familia = str_detect(string = Text, pattern = paste(familia, collapse = "|")),
         derechos_personales = str_detect(string = Text, pattern = paste(derechos_personales, collapse = "|")),
         libertad_personal = str_detect(string = Text, pattern = paste(libertad_personal, collapse = "|")),
         inclusión = str_detect(string = Text, pattern = paste(inclusión, collapse = "|")),
         luchas = str_detect(string = Text, pattern = paste(luchas, collapse = "|")),
         protestas = str_detect(string = Text, pattern = paste(protestas, collapse = "|")),
         adversarios = str_detect(string = Text, pattern = paste(adversarios, collapse = "|")),
         seguridad = str_detect(string = Text, pattern = paste(seguridad, collapse = "|")),
         regimen = str_detect(string = Text, pattern = paste(regimen, collapse = "|")),
         populistas = str_detect(string = Text, pattern = paste(populistas, collapse = "|")),
         pandemia = str_detect(string = Text, pattern = paste(pandemia, collapse = "|"))
         ) 
tempo %>% 
  select(pandemia)

bigramas <- tempo %>%
  filter(pandemia == TRUE) %>% 
  dplyr::select(Text) %>% 
  #mutate(texto = limpiar(cuentas)) %>%
  #select(texto) %>%
  unnest_tokens(input = Text, output = "bigrama",
                token = "ngrams",n = 2, drop = TRUE) %>% 
  separate(bigrama, c("palabra1", "palabra2"),
           sep = " ") %>% 
  filter(!palabra1 %in% stopwords) %>%
  filter(!palabra2 %in% stopwords) %>% 
  filter(palabra1 != "sublevados_") %>% 
  unite(palabra1, palabra2,col="bigrama",sep=" ") %>% 
  group_by(bigrama) %>% 
  count() %>% 
  arrange(desc(freq)) 

bigramas

bigramas %>% 
  wordcloud2(size=2.5, color='random-dark')

bigramas %>% 
  top_n(20) %>% 
  kbl() %>%
  kable_styling()

#-------Tuits más populares ---------------

procesado %>%
  filter(User == "Sublevados_") %>% 
  arrange(desc(like)) %>% 
  select(Text, like)%>% 
  kbl() %>%
  kable_styling()
#-------Cuentas más populares ---------------

procesadomenciones %>% 
  select(mencionados)

glimpse(procesadomenciones)

mencionesfinal <- procesadomenciones %>% 
  dplyr::select(mencionados) %>% 
  mutate(usuarios = stri_extract(mencionados, regex = "username='.*?',")) %>% 
  dplyr::select(usuarios) %>% 
  mutate(usuarios = stri_extract(usuarios, regex = "'.*?',")) %>% 
  group_by(usuarios) %>% 
  count() %>% 
  arrange(desc(freq)) 

mencionesfinal

mencionesfinal %>% 
  top_n(20) %>% 
  ggplot(aes(reorder(usuarios, freq), freq, fill="salmon")) +
  geom_col() +
  coord_flip() +
  labs(title = 'Cuentas más mencionadas\npor líderes de Sublevados',
       subtitle = '',
       caption = "Fuente: API de TWitter",
       x = "Términos",
       y = "Frecuencia")+
  #scale_y_continuous(breaks = c(1,2), limits = c(1, 2))+
  theme_economist()+
  theme(
    plot.title = element_text(size = 26, face = "bold"),
    legend.title = element_text(size = 18),
    legend.text = element_text(size=14),
    legend.position = "none",
    plot.subtitle = element_text(size = 16),
    plot.caption = element_text(size = 12),
    axis.text = element_text(size = 14),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size= 14))


#-------SENTIMIENTO POR TEMA ----------------

temas <- procesado %>% 
  mutate(Trump = case_when(str_detect(Text, paste0(c("Trump","realDonaldTrump"),collapse = '|'), negate = F) ~ T),
         LePen = case_when(str_detect(Text, paste0(c("Le Pen","LePen","@MLP_officiel"),collapse = '|'), negate = F) ~ T),
         Orban = case_when(str_detect(Text, paste0(c("Orban, Orbán"),collapse = '|'), negate = F) ~ T),
         Abascal = case_when(str_detect(Text, paste0(c("Abascal","Santi_ABASCAL"),collapse = '|'), negate = F) ~ T),
         Bolsonaro = case_when(str_detect(Text, paste0(c("Bolsonaro","jairbolsonaro"),collapse = '|'), negate = F) ~ T),
         Milei = case_when(str_detect(Text, paste0(c("Milei","JMilei"),collapse = '|'), negate = F) ~ T), 
         AMLO = case_when(str_detect(Text, paste0(c("AMLO","López Obrador,lopezobrador_"),collapse = '|'), negate = F) ~ T),
         Opositores = case_when(str_detect(Text, paste0(c("oposición","Oposición"),collapse = '|'), negate = F) ~ T),
         PAN = case_when(str_detect(Text, paste0(c("PAN"),collapse = '|'), negate = F) ~ T),
         ) %>% 
  pivot_longer(Trump:PAN, "tópico","Nombre") %>% 
  filter(value == TRUE)

temas <- procesado %>% 
  mutate(Género = case_when(str_detect(Text, paste0(c("Género","género"),collapse = '|'), negate = F) ~ T),
         LGBT = case_when(str_detect(Text, paste0(c("LGBT"),collapse = '|'), negate = F) ~ T),
         Denigra_Gay = case_when(str_detect(Text, paste0(c("Joto","joto","jotos","maricón","Maricón","Putos"),collapse = '|'), negate = F) ~ T),
         Democracia = case_when(str_detect(Text, paste0(c("Democracia","democracia"),collapse = '|'), negate = F) ~ T),
         Iglesia = case_when(str_detect(Text, paste0("Iglesia",collapse = '|'), negate = F) ~ T),
         a_Conservador = case_when(str_detect(Text, paste0(c("Conservador","Conservadurismo","conservador","conservadurismo"),collapse = '|'), negate = F) ~ T), 
         a_Progresista = case_when(str_detect(Text, paste0(c("Progresista","Progre","progresista","progre"),collapse = '|'), negate = F) ~ T),
         b_Aborto = case_when(str_detect(Text, paste0(c("Aborto","aborto"),collapse = '|'), negate = F) ~ T),
         b_Provida = case_when(str_detect(Text, paste0(c("Provida","provida"),collapse = '|'), negate = F) ~ T),
         c_Globalismo = case_when(str_detect(Text, paste0(c("Globalismo","globalismo","globalista"),collapse = '|'), negate = F) ~ T),
         c_Nacionalismo = case_when(str_detect(Text, paste0(c("Nacionalismo","nacionalismo","nacionalista"),collapse = '|'), negate = F) ~ T),
         Feminazi = case_when(str_detect(Text, paste0(c("Feminazi","Feminazis","feminazi","feminazis"),collapse = '|'), negate = F) ~ T),
         Corrección_Política = case_when(str_detect(Text, paste0(c("Corrección política","corrección política"),collapse = '|'), negate = F) ~ T),
         Lenguaje_inclusivo = case_when(str_detect(Text, paste0(c("Lenguaje inclusivo","lenguaje inclusivo"),collapse = '|'), negate = F) ~ T)
  ) %>% 
  pivot_longer(Género:Lenguaje_inclusivo, "tópico","Nombre") %>% 
  filter(value == TRUE)

glimpse(temas)

temas %>%
  dplyr::select(tópico) %>% 
  group_by(tópico) %>% 
  count() %>% 
  ggplot(aes(x=reorder(tópico, freq), y=freq))+
  geom_col()+
  coord_flip()+ 
  labs(title = 'Frecuencia de políticos',
       subtitle = '',
       caption = "Fuente: API de TWitter",
       x = "",
       y = "")+
  #scale_y_continuous(breaks = c(1,2), limits = c(1, 2))+
  theme_economist()+
  theme(
    plot.title = element_text(size = 26, face = "bold"),
    legend.title = element_text(size = 18),
    legend.text = element_text(size=14),
    legend.position = "bottom",
    plot.subtitle = element_text(size = 16),
    plot.caption = element_text(size = 12),
    axis.text = element_text(size = 14),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size= 14))


sentimientos_topic <- 
  temas %>%
  separate(date, into = c("Periodo", "Mes", "Día"), sep = "-",
           remove = FALSE)

sentimientos_afinn_topic <- 
  sentimientos_topic %>%
  unnest_tokens(input = "Text", output = "Palabra") %>%
  inner_join(afinn, ., by = "Palabra") %>%
  mutate(Tipo = ifelse(Puntuacion > 0, "Positiva", "Negativa")) 

sentimientos_afinn_topic <-
  sentimientos_afinn_topic %>%
  filter(Palabra != "no") 

sentimientos_topicos <-
  sentimientos_afinn_topic %>%
  rename(Fecha = date) %>% 
  #group_by(tópico) %>%
  #mutate(Suma = mean(Puntuacion)) %>%
  group_by(tópico) %>%
  summarise(Media = mean(Puntuacion))

sentimientos_afinn_topic %>% 
  ggplot(aes(tópico, Puntuacion))+
  geom_boxplot()+
  stat_summary(fun.y=mean, geom="point", shape=20, size=7, color="red", fill="red") +
  geom_jitter(alpha = 0.05) + 
  labs(title = 'Sentimientos de los tuits personajes',
       subtitle = '',
       caption = "Fuente: API de TWitter",
       x = "Fecha",
       y = "<- Negativo - Positivo ->")+
  #scale_y_continuous(breaks = c(1,2), limits = c(1, 2))+
  theme_economist()+
  theme(
    plot.title = element_text(size = 26, face = "bold"),
    legend.title = element_text(size = 18),
    legend.text = element_text(size=14),
    legend.position = "bottom",
    plot.subtitle = element_text(size = 16),
    plot.caption = element_text(size = 12),
    axis.text = element_text(size = 14),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size= 14))
#------- RECURRENCIA DE TEMAS ----------------

temas <- procesado %>% 
  mutate(Ideología_de_género = case_when(str_detect(Text, paste0(c("Ideología de género","ideologia de género"),collapse = '|')) ~ T),
         Feminazi = case_when(str_detect(Text, paste0(c("Feminazi","Feminazis","feminazi","feminazis"),collapse = '|')) ~ T),
         Religión = case_when(str_detect(Text, paste0(c("Religión","religión"),collapse = '|')) ~ T),
         Donald_Trump = case_when(str_detect(Text, paste0("Trump",collapse = '|')) ~ T)
  ) %>% 
  pivot_longer(Ideología_de_género:Donald_Trump, "tópico","Nombre") %>% 
  filter(value == TRUE)

view(temas)

#------- SEGUIDORES DE LOS FOLLOWERS --------------------

seguidores_sublevados <- get_followers("sublevados_")

seguidores_sublevados

followers <- as.matrix(seguidores_sublevados)

followers_final <- lookup_users(followers)


followers_final_b <- followers_final %>% 
  filter(!is.na(created_at)) %>% 
  select(screen_name) 

sample <- sample_n(followers_final_b,150)

matriz <- as.vector(sample)

getAllFollowers <- function (name) { 
  user_info <- lookup_users(name) 
  user_follower <- get_friends(name, n=5000, retryonratelimit = T)
  Sys.sleep(2) #sleep for 2 seconds
  return(user_follower) }

out <- lapply(X = matriz, FUN = getAllFollowers)

out

out2 <- out %>% 
  utils::select.list(user_id)

out2 %>% 
  select(screen_name.user_id)

depurado <- ldply(out2) %>% 
  select(user_id)

depurado_vector <- as.matrix(depurado)

#final <- lookup_users(depurado_vector)



#write.csv(out,"01_datos/followers_sublevados.csv")



lookup_many_users <- function(users, retry_limit = 5){
  
  breaks <- seq(1, length(users), 89999)
  
  if(breaks[length(breaks)] != length(users)){
    breaks <- c(breaks, length(users))
  }
  
  user_details <- NULL
  
  for(i in 1:(length(breaks) -1)){
    
    attempt <- 0
    
    while(is.null(user_details) && attempt <= retry_limit){
      
      attempt <- attempt + 1
      
      try({
        user_details <- lookup_users(users[breaks[i]:breaks[i+1]])
        
        Sys.sleep(15 * 60) #wait 15 minutes for rate limit to reset before proceeding
      })
    }
    
    if(is.null(user_details)){
      stop("failed to get users")
    }    
    
    if(i == 1){
      all_user_details <- user_details
    } else {
      all_user_details <- rbind(all_user_details, user_details)
    }
    
    user_details <- NULL
  }
  
  return(all_user_details)
  
}

data <- lookup_many_users(depurado_vector)

final %>% 
  select(screen_name) %>% 
  count() %>% 
  arrange(desc(freq))

#-------LDA----------------------------------

vector <- as.vector(procesado$Text)

corpus <- Corpus(VectorSource(vector))

corpus

processedCorpus <- tm_map(corpus, content_transformer(tolower))
processedCorpus <- tm_map(processedCorpus, removeWords, english_stopwords)
processedCorpus <- tm_map(processedCorpus, removePunctuation, preserve_intra_word_dashes = TRUE)
processedCorpus <- tm_map(processedCorpus, removeNumbers)
processedCorpus <- tm_map(processedCorpus, stemDocument, language = "es")
processedCorpus <- tm_map(processedCorpus, stripWhitespace)

minimumFrequency <- 5
DTM <- DocumentTermMatrix(processedCorpus, control = list(bounds = list(global = c(minimumFrequency, Inf))))
dim(DTM)

sel_idx <- slam::row_sums(DTM) > 0
DTM <- DTM[sel_idx, ]
textdata <- procesado[sel_idx, ]

result <- ldatuning::FindTopicsNumber(
  DTM,
  topics = seq(from = 2, to = 20, by = 1),
  metrics = c("CaoJuan2009",  "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  verbose = TRUE
)

write.csv(result, "01_datos/topicsnumber.csv")

FindTopicsNumber_plot(result)

# number of topics
K <- 7
# set random number generator seed
set.seed(9161)
# compute the LDA model, inference via 1000 iterations of Gibbs sampling
topicModel <- LDA(DTM, K, method="Gibbs", control=list(iter = 500, verbose = 25))

# have a look a some of the results (posterior distributions)
tmResult <- posterior(topicModel)
# format of the resulting object
attributes(tmResult)

nTerms(DTM)

# topics are probability distribtions over the entire vocabulary
beta <- tmResult$terms   # get beta from results
dim(beta) 
rowSums(beta)
nDocs(DTM)

# for every document we have a probaility distribution of its contained topics
theta <- tmResult$topics 
dim(theta)               # nDocs(DTM) distributions over K topics
rowSums(theta)[1:10]     # rows in theta sum to 1

terms(topicModel, 20)
