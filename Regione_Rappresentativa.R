library(tidyverse)
library(janitor)
library(data.table)

# Politiche 2022

pol_22 = as.data.frame(fread("Input/Camera/camera-20220925/Camera_Italia_LivComune.txt"))%>%
  clean_names() %>%
  mutate(regione = trimws(gsub('[0-9]+', '', circ_reg)))%>%
  group_by(regione, descrlista)%>%
  summarise(voti = sum(votilista))%>%
  rename(lista = descrlista)%>%
  mutate(anno = 2022)%>%
  select(regione, lista, voti, anno)


pol_22_vda = as.data.frame(fread("Input/Camera/camera-20220925/Camera_VAosta_LivComune.txt"))%>%
  clean_names()%>%
  select(regione, contrassegno, totvoti)%>%
  rename(lista = contrassegno)%>%
  group_by(regione, lista)%>%
  summarise(voti = sum(totvoti))%>%
  mutate(anno = 2022)%>%
  select(regione, lista, voti, anno)


# Politiche 2018

pol_18 = as.data.frame(fread("Input/Camera/camera-20180304/Camera2018_livComune.txt"))%>%
  clean_names() %>%
  mutate(regione = trimws(gsub('[0-9]+', '', circoscrizione)))%>%
  group_by(regione, lista)%>%
  summarise(voti = sum(voti_lista, na.rm = TRUE))%>%
  mutate(anno = 2018)%>%
  select(regione, lista, voti, anno)

pol_18_vda = as.data.frame(fread("Input/Camera/camera-20180304/Camera2018_VAosta_CandidCollUninom.txt"))%>%
  clean_names()%>%
  mutate(regione = "VALLE D'AOSTA")%>%
  select(regione, lista, totvoti)%>%
  group_by(regione, lista)%>%
  summarise(voti = sum(totvoti))%>%
  mutate(anno = 2018)%>%
  select(regione, lista, voti, anno)


# Politiche 2013

pol_13 = as.data.frame(fread("Input/Camera/camera-20130224/camera_italia-20130224.txt"))%>%
  clean_names() %>%
  mutate(regione = trimws(gsub('[0-9]+', '', circoscrizione)))%>%
  group_by(regione, lista)%>%
  summarise(voti = sum(voti_lista, na.rm = TRUE))%>%
  mutate(anno = 2013)%>%
  select(regione, lista, voti, anno)

pol_13_vda = as.data.frame(fread("Input/Camera/camera-20130224/camera_vaosta-20130224.txt"))%>%
  clean_names()%>%
  mutate(voti_lista = as.numeric(trimws(gsub(',', '.', voti_lista))))%>%
  mutate(regione = "VALLE D'AOSTA")%>%
  select(regione, lista, voti_lista)%>%
  group_by(regione, lista)%>%
  summarise(voti = sum(voti_lista))%>%
  mutate(anno = 2013)%>%
  select(regione, lista, voti, anno)

# Politiche 2008

pol_08 = as.data.frame(fread("Input/Camera/camera-20080413/camera_italia-20080413.txt"))%>%
  clean_names()%>%
  mutate(regione = trimws(gsub('[0-9]+', '', circoscrizione)))%>%
  group_by(regione, lista)%>%
  summarise(voti = sum(voti_lista, na.rm = TRUE))%>%
  mutate(anno = 2008)%>%
  select(regione, lista, voti, anno)

pol_08_vda = as.data.frame(fread("Input/Camera/camera-20080413/camera_vaosta-20080413.txt"))%>%
  clean_names()%>%
  mutate(voti_lista = as.numeric(trimws(gsub(',', '.', voti_lista))))%>%
  mutate(regione = "VALLE D'AOSTA")%>%
  select(regione, lista, voti_lista)%>%
  group_by(regione, lista)%>%
  summarise(voti = sum(voti_lista))%>%
  mutate(anno = 2008)%>%
  select(regione, lista, voti, anno)


# Politiche 2006

pol_06 = as.data.frame(fread("Input/Camera/camera-20060409/camera_italia-20060409.txt"))%>%
  clean_names()  %>%
  mutate(regione = trimws(gsub('[0-9]+', '', circoscrizione)))%>%
  group_by(regione, lista)%>%
  summarise(voti = sum(votilista, na.rm = TRUE))%>%
  mutate(anno = 2006)%>%
  select(regione, lista, voti, anno)

pol_06_vda = as.data.frame(fread("Input/Camera/camera-20060409/camera_vaosta-20060409.txt"))%>%
  clean_names()%>%
  mutate(voti_lista = as.numeric(trimws(gsub(',', '.', voti_lista))))%>%
  mutate(regione = "VALLE D'AOSTA")%>%
  select(regione, lista, voti_lista)%>%
  group_by(regione, lista)%>%
  summarise(voti = sum(voti_lista))%>%
  mutate(anno = 2006)%>%
  select(regione, lista, voti, anno)


# Politiche 2001

pol_01 = as.data.frame(fread("Input/Camera/camera-20010513/camera-20010513_Proporzionale.txt"))%>%
  clean_names() %>%
  mutate(regione = trimws(gsub('[0-9]+', '', circoscrizione)))%>%
  group_by(regione, lista)%>%
  summarise(voti = sum(voti_lista, na.rm = TRUE))%>%
  mutate(anno = 2001)%>%
  arrange(desc(voti))%>%
  select(regione, lista, voti, anno)


# Pre 2000

pol_96 = as.data.frame(fread("Input/Camera/camera-19960421/camera-19960421_Proporzionale.txt"))%>%
  clean_names() %>%
  mutate(regione = trimws(gsub('[0-9]+', '', circoscrizione)))%>%
  group_by(regione, lista)%>%
  summarise(voti = sum(voti_lista, na.rm = TRUE))%>%
  mutate(anno = 1996)%>%
  arrange(desc(voti))%>%
  select(regione, lista, voti, anno)

pol_94 = as.data.frame(fread("Input/Camera/camera-19940327/camera-19940327_Proporzionale.txt"))%>%
  clean_names() %>%
  mutate(regione = trimws(gsub('[0-9]+', '', circoscrizione)))%>%
  group_by(regione, lista)%>%
  summarise(voti = sum(voti_lista, na.rm = TRUE))%>%
  mutate(anno = 1994)%>%
  arrange(desc(voti))%>%
  select(regione, lista, voti, anno)

pol_92 = as.data.frame(fread("Input/Camera/camera-19920405/camera-19920405.txt"))%>%
  clean_names() %>%
  mutate(regione = trimws(gsub('[0-9]+', '', circoscrizione)))%>%
  group_by(regione, lista)%>%
  summarise(voti = sum(voti_lista, na.rm = TRUE))%>%
  mutate(anno = 1992)%>%
  arrange(desc(voti))%>%
  select(regione, lista, voti, anno)

pol_87 = as.data.frame(fread("Input/Camera/camera-19870614/camera-19870614.txt"))%>%
  clean_names() %>%
  mutate(regione = trimws(gsub('[0-9]+', '', circoscrizione)))%>%
  group_by(regione, lista)%>%
  summarise(voti = sum(voti_lista, na.rm = TRUE))%>%
  mutate(anno = 1987)%>%
  arrange(desc(voti))%>%
  select(regione, lista, voti, anno)


pol_83 = as.data.frame(fread("Input/Camera/camera-19830626/camera-19830626.txt"))%>%
  clean_names() %>%
  mutate(regione = trimws(gsub('[0-9]+', '', circoscrizione)))%>%
  group_by(regione, lista)%>%
  summarise(voti = sum(voti_lista, na.rm = TRUE))%>%
  mutate(anno = 1983)%>%
  arrange(desc(voti))%>%
  select(regione, lista, voti, anno)


pol_79 = as.data.frame(fread("Input/Camera/camera-19790603/camera-19790603.txt"))%>%
  clean_names() %>%
  mutate(regione = trimws(gsub('[0-9]+', '', circoscrizione)))%>%
  group_by(regione, lista)%>%
  summarise(voti = sum(voti_lista, na.rm = TRUE))%>%
  mutate(anno = 1979)%>%
  arrange(desc(voti))%>%
  select(regione, lista, voti, anno)

pol_76 = as.data.frame(fread("Input/Camera/camera-19760620/camera-19760620.txt"))%>%
  clean_names() %>%
  mutate(regione = trimws(gsub('[0-9]+', '', circoscrizione)))%>%
  group_by(regione, lista)%>%
  summarise(voti = sum(voti_lista, na.rm = TRUE))%>%
  mutate(anno = 1976)%>%
  arrange(desc(voti))%>%
  select(regione, lista, voti, anno)


pol_72 = as.data.frame(fread("Input/Camera/camera-19720507/camera-19720507.txt"))%>%
  clean_names() %>%
  mutate(regione = trimws(gsub('[0-9]+', '', circoscrizione)))%>%
  group_by(regione, lista)%>%
  summarise(voti = sum(voti_lista, na.rm = TRUE))%>%
  mutate(anno = 1972)%>%
  arrange(desc(voti))%>%
  select(regione, lista, voti, anno)


pol_68 = as.data.frame(fread("Input/Camera/camera-19680519/camera-19680519.txt"))%>%
  clean_names() %>%
  mutate(regione = trimws(gsub('[0-9]+', '', circoscrizione)))%>%
  group_by(regione, lista)%>%
  summarise(voti = sum(voti_lista, na.rm = TRUE))%>%
  mutate(anno = 1968)%>%
  arrange(desc(voti))%>%
  select(regione, lista, voti, anno)


pol_63 = as.data.frame(fread("Input/Camera/camera-19630428/camera-19630428.txt"))%>%
  clean_names() %>%
  mutate(regione = trimws(gsub('[0-9]+', '', circoscrizione)))%>%
  group_by(regione, lista)%>%
  summarise(voti = sum(voti_lista, na.rm = TRUE))%>%
  mutate(anno = 1963)%>%
  arrange(desc(voti))%>%
  select(regione, lista, voti, anno)


pol_58 = as.data.frame(fread("Input/Camera/camera-19580525/camera-19580525.txt"))%>%
  clean_names() %>%
  mutate(regione = trimws(gsub('[0-9]+', '', circoscrizione)))%>%
  group_by(regione, lista)%>%
  summarise(voti = sum(voti_lista, na.rm = TRUE))%>%
  mutate(anno = 1958)%>%
  arrange(desc(voti))%>%
  select(regione, lista, voti, anno)


pol_53 = as.data.frame(fread("Input/Camera/camera-19530607/camera-19530607.txt"))%>%
  clean_names() %>%
  mutate(regione = trimws(gsub('[0-9]+', '', circoscrizione)))%>%
  group_by(regione, lista)%>%
  summarise(voti = sum(voti_lista, na.rm = TRUE))%>%
  mutate(anno = 1953)%>%
  arrange(desc(voti))%>%
  select(regione, lista, voti, anno)


pol_48 = as.data.frame(fread("Input/Camera/camera-19480418/camera-19480418.txt"))%>%
  clean_names() %>%
  mutate(regione = trimws(gsub('[0-9]+', '', circoscrizione)))%>%
  group_by(regione, lista)%>%
  summarise(voti = sum(voti_lista, na.rm = TRUE))%>%
  mutate(anno = 1948)%>%
  arrange(desc(voti))%>%
  select(regione, lista, voti, anno)






# Unisci tutto

data = bind_rows( pol_48,
                  pol_53,
                  pol_58,
                  pol_63,
                  pol_68,
                  pol_72,
                  pol_76,
                  pol_83,
                  pol_87,
                  pol_92,
                  pol_94,
                  pol_96,
                  pol_01,
                  pol_06,
                  pol_06_vda,
                  pol_08,
                  pol_08_vda,
                  pol_13,
                  pol_13_vda,
                  pol_18,
                  pol_18_vda,
                  pol_22,
                  pol_22_vda
)%>%
  mutate(regione = toupper(regione))%>%
  mutate(regione = case_when(regione %in% c("EMILIA-ROMAGNA",
                                            "EMILIA ROMAGNA") ~ "EMILIA-ROMAGNA",
                             regione %in% c("FRIULI VENEZIA GIULIA" ,
                                            "FRIULI-VENEZIA GIULIA") ~ "FRIULI-VENEZIA GIULIA",
                             regione %in% c("TRENTINO-ALTO ADIGE", "TRENTINO ALTO ADIGE",
                                            "TRENTINO-ALTO ADIGE/SUDTIROL", "TRENTINO-ALTO ADIGE/S_DTIROL") ~ "TRENTINO-ALTO ADIGE",
                             regione == 'MILANO-PAVIA' ~ 'LOMBARDIA',
                             regione == 'ROMA-VITERBO-LATINA-FROSINONE' ~ 'LAZIO',
                             regione == 'VERONA-PADOVA-VICENZA-ROVIGO' ~ 'VENETO',
                             regione == 'TORINO-NOVARA-VERCELLI' ~ 'PIEMONTE',
                             regione == 'NAPOLI-CASERTA' ~ 'CAMPANIA',
                             regione == 'BOLOGNA-FERRARA-RAVENNA-FORLÌ' ~ 'EMILIA-ROMAGNA',
                             regione == 'CATANIA-MESSINA-SIRACUSA-RAGUSA-ENNA' ~ 'SICILIA',
                             regione == 'BRESCIA-BERGAMO' ~ 'LOMBARDIA',
                             regione == "PARMA-MODENA-PIACENZA-REGGIO NELL'EMILIA" ~ 'EMILIA-ROMAGNA',
                             regione == 'PALERMO-TRAPANI-AGRIGENTO-CALTANISSETTA' ~ 'SICILIA',
                             regione == 'GENOVA-IMPERIA-LA SPEZIA-SAVONA' ~ 'LIGURIA',
                             regione == 'CATANZARO-COSENZA-REGGIO DI CALABRIA' ~ 'CALABRIA',
                             regione == 'CUNEO-ALESSANDRIA-ASTI' ~ 'PIEMONTE',
                             regione == 'BARI-FOGGIA' ~ 'PUGLIA',
                             regione == 'VENEZIA-TREVISO' ~ 'VENETO',
                             regione == 'COMO-SONDRIO-VARESE' ~ 'LOMBARDIA',
                             regione == 'BENEVENTO-AVELLINO-SALERNO' ~ 'CAMPANIA',
                             regione == 'ANCONA-PESARO-MACERATA-ASCOLI PICENO' ~ 'MARCHE',
                             regione == "L'AQUILA-PESCARA-CHIETI-TERAMO" ~ 'ABRUZZO',
                             regione == 'FIRENZE-PISTOIA' ~ 'TOSCANA',
                             regione == 'UDINE-BELLUNO-GORIZIA' ~ 'FRIULI-VENEZIA GIULIA',
                             regione == 'LECCE-BRINDISI-TARANTO' ~ 'PUGLIA',
                             regione == 'PISA-LIVORNO-LUCCA-MASSA E CARRARA' ~ 'TOSCANA',
                             regione == 'CAGLIARI-SASSARI-NUORO' ~ 'SARDEGNA',
                             regione == 'SIENA-AREZZO-GROSSETO' ~ 'TOSCANA',
                             regione == 'PERUGIA-TERNI-RIETI' ~ 'UMBRIA',
                             regione == 'MANTOVA-CREMONA' ~ 'LOMBARDIA',
                             regione == 'TRENTO-BOLZANO' ~ 'TRENTINO-ALTO ADIGE',
                             regione == 'POTENZA-MATERA' ~ 'BASILICATA',
                             regione == 'CAMPOBASSO' ~ 'MOLISE',
                             regione == "VALLE D'AOSTA" ~ "VALLE D'AOSTA",
                             regione == 'MILANO-PAVIA' ~ 'LOMBARDIA',
                             regione == 'VERONA-PADOVA-VICENZA-ROVIGO' ~ 'VENETO',
                             regione == 'ROMA-VITERBO-LATINA-FROSINONE' ~ 'LAZIO',
                             regione == 'TORINO-NOVARA-VERCELLI' ~ 'PIEMONTE',
                             regione == 'BRESCIA-BERGAMO' ~ 'LOMBARDIA',
                             regione == 'NAPOLI-CASERTA' ~ 'CAMPANIA',
                             regione == 'BOLOGNA-FERRARA-RAVENNA-FORLI' ~ 'EMILIA-ROMAGNA',
                             regione == 'CATANIA-MESSINA-SIRACUSA-RAGUSA-ENNA' ~ 'SICILIA',
                             regione == 'PALERMO-TRAPANI-AGRIGENTO-CALTANISSETTA' ~ 'SICILIA',
                             regione == 'GENOVA-IMPERIA-LA SPEZIA-SAVONA' ~ 'LIGURIA',
                             regione == 'COMO-SONDRIO-VARESE' ~ 'LOMBARDIA',
                             regione == 'CATANZARO-COSENZA-REGGIO CALABRIA' ~ 'CALABRIA',
                             regione == 'VENEZIA-TREVISO' ~ 'VENETO',
                             regione == 'CUNEO-ALESSANDRIA-ASTI' ~ 'PIEMONTE',
                             regione == 'PARMA-MODENA-PIACENZA-REGGIO EMILIA' ~ 'EMILIA-ROMAGNA',
                             regione == 'BARI-FOGGIA' ~ 'PUGLIA',
                             regione == 'UDINE-BELLUNO-GORIZIA-PORDENONE' ~ 'FRIULI-VENEZIA GIULIA',
                             regione == 'ANCONA-PESARO-MACERATA-ASCOLI PICENO' ~ 'MARCHE',
                             regione == 'BENEVENTO-AVELLINO-SALERNO' ~ 'CAMPANIA',
                             regione == "L'AQUILA-PESCARA-CHIETI-TERAMO" ~ 'ABRUZZO',
                             regione == 'FIRENZE-PISTOIA' ~ 'TOSCANA',
                             regione == 'PISA-LIVORNO-LUCCA-MASSA CARRARA' ~ 'TOSCANA',
                             regione == 'LECCE-BRINDISI-TARANTO' ~ 'PUGLIA',
                             regione == 'CAGLIARI-SASSARI-NUORO-ORISTANO' ~ 'SARDEGNA',
                             regione == 'SIENA-AREZZO-GROSSETO' ~ 'TOSCANA',
                             regione == 'MANTOVA-CREMONA' ~ 'LOMBARDIA',
                             regione == 'PERUGIA-TERNI-RIETI' ~ 'UMBRIA',
                             regione == 'POTENZA-MATERA' ~ 'BASILICATA',
                             regione == 'CAMPOBASSO-ISERNIA' ~ 'MOLISE',
                             regione == 'AOSTA' ~ "VALLE D'AOSTA",
                             regione == 'PALERMO-TRAPANI-AGRIGENTO-CALTANISETTA' ~ 'SICILIA',
                             regione == 'UDINE-BELLUNO-GORIZIA' ~ 'FRIULI-VENEZIA GIULIA',
                             regione == 'CAGLIARI-SASSARI-NUORO' ~ 'SARDEGNA',
                             regione == 'CAMPOBASSO' ~ 'MOLISE',
                             regione == 'TRIESTE' ~ 'FRIULI-VENEZIA GIULIA',
                             regione == "BOLOGNA-FERRARA-RAVENNA-FORLI'" ~ 'EMILIA-ROMAGNA',
                             regione == "VALLE D'AOSTA" ~ "VALLE D'AOSTA",
                             regione %in% c("ABRUZZI") ~ "ABRUZZO",
                             TRUE ~ regione))%>%
  filter(regione != "NA")%>%
  group_by(regione,lista,  anno)%>%
  summarise(voti = sum(voti))


length(unique(data$regione)) == 20

# Finale

data_naz = data %>%
  group_by(anno, lista)%>%
  summarise(voti_nazionale = sum(voti))%>%
  group_by(anno)%>%
  mutate(perc_naz = voti_nazionale / sum(voti_nazionale) * 100)


data_reg = inner_join(data_naz, data, by=c("anno", "lista"),
                      multiple = "all")%>%
  group_by(regione, anno)%>%
  mutate(perc_reg = voti / sum(voti) * 100)%>%
  mutate(diff = (perc_naz - perc_reg)^2)%>%
  group_by(regione, anno)%>%
  summarise(diff_somma = sum(diff))%>%
  mutate(radice = sqrt(diff_somma))%>%
  group_by(anno)%>%
  mutate(indice = rank(radice))%>%
  group_by(regione)%>%
  summarise(media_rank = mean(indice),
            media_radice = mean(radice))%>%
  mutate(indice = rank(media_radice),
         radice = round(media_radice, 2))


write.csv(data_reg, file="Output/camera_media.csv")



data_reg = inner_join(data_naz, data, by=c("anno", "lista"),
                      multiple = "all")%>%
  group_by(regione, anno)%>%
  mutate(perc_reg = voti / sum(voti) * 100)%>%
  mutate(diff = (perc_naz - perc_reg)^2)%>%
  group_by(regione, anno)%>%
  summarise(diff_somma = sum(diff))%>%
  mutate(radice = sqrt(diff_somma))%>%
  group_by(anno)%>%
  mutate(indice = rank(radice),
         radice = round(radice, 2))

write.csv(data_reg,
          file="Output/camera_anni.csv")




###########################

## EUROPEE ##


eu_19 = as.data.frame(fread("Input/Europee/europee-20190526/europee-20190526.txt"))%>%
  clean_names()%>%
  select(regione, lista, voti_lista)%>%
  mutate(anno = 2019)

eu_14 = as.data.frame(fread("Input/Europee/europee-20140525/europee-20140525.txt"))%>%
  clean_names()%>%
  select(regione, lista, voti_lista)%>%
  mutate(anno = 2014)

eu_09 = as.data.frame(fread("Input/Europee/europee-20090607/europee-20090607.txt"))%>%
  clean_names()%>%
  select(regione, lista, voti_lista)%>%
  mutate(anno = 2009)

eu_04 = as.data.frame(fread("Input/Europee/europee-20040612/europee-20040612.txt"))%>%
  clean_names()%>%
  select(regione, lista, votilista)%>%
  rename(voti_lista = votilista)%>%
  mutate(anno = 2004)

eu_99 = as.data.frame(fread("Input/Europee/europee-19990613/europee-19990613.txt"))%>%
  clean_names()%>%
  select(descrreg, lista, voti_lista)%>%
  rename(regione = descrreg)%>%
  mutate(anno = 1999)

eu_94 = as.data.frame(fread("Input/Europee/europee-19940612/europee-19940612.txt"))%>%
  clean_names()%>%
  select(regione, lista, voti_lista)%>%
  mutate(anno = 1994)

eu_89 = as.data.frame(fread("Input/Europee/europee-19890618/europee-19890618.txt"))%>%
  clean_names()%>%
  select(regione, lista, voti_lista)%>%
  mutate(anno = 1989)

eu_84 = as.data.frame(fread("Input/Europee/europee-19840617/europee-19840617.txt"))%>%
  clean_names()%>%
  select(regione, lista, voti_lista)%>%
  mutate(anno = 1984)

eu_79 = as.data.frame(fread("Input/Europee/europee-19790610/europee-19790610.txt"))%>%
  clean_names()%>%
  select(regione, lista, voti_lista)%>%
  mutate(anno = 1979)



# Unisci tutto

data = bind_rows(
  eu_19,
  eu_14,
  eu_09,
  eu_04,
  eu_99,
  eu_94,
  eu_89,
  eu_84,
  eu_79
)%>%
  mutate(regione = trimws(toupper(regione)))%>%
  mutate(regione = case_when(regione %in% c("EMILIA-ROMAGNA",
                                            "EMILIA ROMAGNA") ~ "EMILIA ROMAGNA",
                             regione %in% c("FRIULI VENEZIA GIULIA" ,
                                            "FRIULI-VENEZIA GIULIA") ~ "FRIULI-VENEZIA GIULIA",
                             regione %in% c("TRENTINO-ALTO ADIGE", "TRENTINO ALTO ADIGE",
                                            "TRENTINO-ALTO ADIGE/SUDTIROL", "TRENTINO-ALTO ADIGE/S_DTIROL") ~ "TRENTINO-ALTO ADIGE",
                             regione %in% c("ABRUZZI") ~ "ABRUZZO",
                             TRUE ~ regione))%>%
  rename(voti = voti_lista)%>%
  group_by(regione,lista,  anno)%>%
  summarise(voti = sum(voti))


length(unique(data$regione)) == 20

# Finale

data_naz = data %>%
  group_by(anno, lista)%>%
  summarise(voti_nazionale = sum(voti))%>%
  group_by(anno)%>%
  mutate(perc_naz = voti_nazionale / sum(voti_nazionale) * 100)


data_reg = inner_join(data_naz, data, by=c("anno", "lista"),
                      multiple = "all")%>%
  group_by(regione, anno)%>%
  mutate(perc_reg = voti / sum(voti) * 100)%>%
  mutate(diff = (perc_naz - perc_reg)^2)%>%
  group_by(regione, anno)%>%
  summarise(diff_somma = sum(diff))%>%
  mutate(radice = sqrt(diff_somma))%>%
  group_by(anno)%>%
  mutate(indice = rank(radice))%>%
  group_by(regione)%>%
  summarise(media_rank = mean(indice),
            media_radice = mean(radice))%>%
  mutate(indice = rank(media_radice))


write.csv(data_reg, file="Output/europee_media.csv")



data_reg = inner_join(data_naz, data, by=c("anno", "lista"),
                      multiple = "all")%>%
  group_by(regione, anno)%>%
  mutate(perc_reg = voti / sum(voti) * 100)%>%
  mutate(diff = (perc_naz - perc_reg)^2)%>%
  group_by(regione, anno)%>%
  summarise(diff_somma = sum(diff))%>%
  mutate(radice = sqrt(diff_somma))%>%
  group_by(anno)%>%
  mutate(indice = rank(radice),
         radice = round(radice, 2))

write.csv(data_reg,
        file="Output/europee_anni.csv")




