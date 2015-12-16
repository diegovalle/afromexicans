library(readr)
library(dplyr)
library(stringr)
library(downloader)
library(rgeos)
library(rgdal) 
library(ggplot2)
library(viridis)
library("raster")
library(leaflet)
library(htmltools)
library(htmlTable)
library(scales)
options(stringsAsFactors=FALSE)

#http://www3.inegi.org.mx/sistemas/microdatos/Descargas.aspx?sr=microdatos_archivos/encuesta_intercensal/2015/eic2015_27_csv.zip&ht=02
#http://www3.inegi.org.mx/sistemas/microdatos/Descargas.aspx?sr=microdatos_archivos/encuesta_intercensal/2015/eic2015_32_csv.zip&ht=02
#("ags" "bc" "bcs" "camp" "coah" "col" "chis" "chih" 
#"df" "dgo" "gto" "gro" "hgo" "jal" "mex" "mich" "mor" "nay" "nl" "oax" 
#"pue" "qro" "qroo" "slp" "sin" "son" "tab" "tamps" "tlax" "ver" "yuc" 
#"zac");

# summary(df$FACTOR)
# sum(df$FACTOR)
# nrow(df)
URL <- "http://www3.inegi.org.mx/sistemas/microdatos/Descargas.aspx?sr=microdatos_archivos/encuesta_intercensal/2015/eic2015_"


for(i in 1:32) {
  download(str_c(URL, str_pad(i, 2, "left", "0"), "_csv.zip&ht=02"),
           str_c("data/", i), mode = "wb")
  unzip(str_c("data/", i), exdir = "data")
  file.remove(str_c("data/", i))
}

afros <- data.frame()
for(i in 1:32) {
  print(i)
  df <- read_csv(str_c("data/TR_PERSONA",  str_pad(i, 2, "left", "0"), ".CSV"))
#   df%>%
#     group_by(AFRODES) %>%
#     summarise(count = sum(FACTOR))
  
  afros_state <- df %>%
    group_by(ENT, NOM_ENT, MUN, NOM_MUN) %>%
    summarise(total = sum(FACTOR)) %>%
    left_join(
      df%>%
        group_by(ENT, NOM_ENT, MUN, NOM_MUN) %>%
        # 1 == Sí, 2 == Sí, en parte 
        filter(AFRODES %in% c(1)) %>%
        summarise(afro = sum(FACTOR))
    ) %>%
    left_join(
      df%>%
        group_by(ENT, NOM_ENT, MUN, NOM_MUN) %>%
        # 1 == Sí, 2 == Sí, en parte 
        filter(AFRODES %in% c(2)) %>%
        summarise(afro_part = sum(FACTOR))
    ) %>%
    left_join(
      df%>%
        group_by(ENT, NOM_ENT, MUN, NOM_MUN) %>%
        # 1 == Sí, 2 == Sí, en parte 
        filter(PERTE_INDIGENA %in% c(1)) %>%
        summarise(ind = sum(FACTOR))
    ) %>%
    left_join(
      df%>%
        group_by(ENT, NOM_ENT, MUN, NOM_MUN) %>%
        # 1 == Sí, 2 == Sí, en parte 
        filter(PERTE_INDIGENA %in% c(2)) %>%
        summarise(ind_part = sum(FACTOR))
    ) %>%
    mutate(per = (afro + afro_part) / total) %>%
    arrange(desc(per))
  afros <- rbind(afros, afros_state)
  rm(df)
  gc()
}
afros$id <- str_c(afros$ENT, afros$MUN)
afros$per <- (afros$afro + afros$afro_part) / afros$total
afros$per_ind <- (afros$ind) / afros$total
afros[is.na(afros$per), "per"] <- 0
afros[is.na(afros$per), "afro"] <- 0
afros[is.na(afros$per_ind), "per_ind"] <- 0
afros[is.na(afros$per_ind), "ind"] <- 0

round((sum(afros$afro, na.rm = TRUE)+sum(afros$afro_part, na.rm = TRUE))/sum(afros$total, na.rm = TRUE),3)


ggplot(afros, aes(afro, afro_part)) +
  geom_point() +
  geom_smooth(method = lm)


read <- data.frame()
for(i in c(12,20,30)) {
  print(i)
  df <- read_csv(str_c("data/TR_PERSONA",  str_pad(i, 2, "left", "0"), ".CSV"))
  
  read_mun <- df %>%
    group_by(ENT, NOM_ENT,  AFRODES, PERTE_INDIGENA) %>%
    # 
    # 1 == Sí, 2 == Sí, en parte 3 ==No
    # 5 == Sí, 7 == No
    filter((ALFABET == 5 | ALFABET == 7) & 
             (AFRODES == 1 | AFRODES == 2 | AFRODES == 3) &
             (PERTE_INDIGENA == 1 | PERTE_INDIGENA == 2 | PERTE_INDIGENA == 3)) %>%
    summarise(total = sum(FACTOR)) %>%
    left_join(
      df%>%
        group_by(ENT, NOM_ENT,  AFRODES, ALFABET, PERTE_INDIGENA) %>%
        # 1 == Sí, 2 == Sí, en parte 
        filter((ALFABET == 5 | ALFABET == 7) & 
                 (AFRODES == 1 | AFRODES == 2 | AFRODES == 3) &
                 (PERTE_INDIGENA == 1 | PERTE_INDIGENA == 2 | PERTE_INDIGENA == 3)) %>%
        summarise(alfabet = sum(FACTOR))
    )
  read <- rbind(read, read_mun)
  rm(df)
  gc()
}
read$category <- NA
read[which(read$AFRODES == 1 & 
             read$PERTE_INDIGENA == 1 & 
             read$ALFABET == 5), "category" ] <- "Afro-Mexican & Indigenous"
read[which(read$AFRODES == 2 & 
             read$PERTE_INDIGENA == 1 & 
             read$ALFABET == 5), "category" ] <- "Part Afro-Mexican & Indigenous"
read[which(read$AFRODES == 3 & 
             read$PERTE_INDIGENA == 1 & 
             read$ALFABET == 5), "category" ] <- "Indigenous"

read[which(read$AFRODES == 1 & 
             read$PERTE_INDIGENA == 2 & 
             read$ALFABET == 5), "category" ] <- "Afro-Mexican & Part Indigenous"
read[which(read$AFRODES == 2 & 
             read$PERTE_INDIGENA == 2 & 
             read$ALFABET == 5), "category" ] <- "Part Afro-Mexican & Part Indigenous"
read[which(read$AFRODES == 3 & 
             read$PERTE_INDIGENA == 2 & 
             read$ALFABET == 5), "category" ] <- "Part Indigenous"

read[which(read$AFRODES == 1 & 
             read$PERTE_INDIGENA == 3 & 
             read$ALFABET == 5), "category" ] <- "Afro-Mexican"
read[which(read$AFRODES == 2 & 
             read$PERTE_INDIGENA == 3 & 
             read$ALFABET == 5), "category" ] <- "Part Afro-Mexican"
read[which(read$AFRODES == 3 & 
             read$PERTE_INDIGENA == 3 & 
             read$ALFABET == 5), "category" ] <- "Non Afro-Mexican & Non Indigenous"

read[which(read$AFRODES == 1 & 
             read$PERTE_INDIGENA == 1 & 
             read$ALFABET == 7), "category" ] <- "Afro-Mexican & Indigenous"
read[which(read$AFRODES == 2 & 
             read$PERTE_INDIGENA == 1 & 
             read$ALFABET == 7), "category" ] <- "Part Afro-Mexican & Indigenous"
read[which(read$AFRODES == 3 & 
             read$PERTE_INDIGENA == 1 & 
             read$ALFABET == 7), "category" ] <- "Indigenous"

read[which(read$AFRODES == 1 & 
             read$PERTE_INDIGENA == 2 & 
             read$ALFABET == 7), "category" ] <- "Afro-Mexican & Part Indigenous"
read[which(read$AFRODES == 2 & 
             read$PERTE_INDIGENA == 2 & 
             read$ALFABET ==7), "category" ] <- "Part Afro-Mexican & Part Indigenous"
read[which(read$AFRODES == 3 & 
             read$PERTE_INDIGENA == 2 & 
             read$ALFABET == 7), "category" ] <- "Part Indigenous"

read[which(read$AFRODES == 1 & 
             read$PERTE_INDIGENA == 3 & 
             read$ALFABET == 7), "category" ] <- "Afro-Mexican"
read[which(read$AFRODES == 2 & 
             read$PERTE_INDIGENA == 3 & 
             read$ALFABET == 7), "category" ] <- "Part Afro-Mexican"
read[which(read$AFRODES == 3 & 
             read$PERTE_INDIGENA == 3 & 
             read$ALFABET == 7), "category" ] <- "Non Afro-Mexican & Non Indigenous"

read$per <-  read$alfabet / read$total 

#read %>% filter(ALFABET == 5) %>% group_by(NOM_ENT, AFRODES) %>% summarise(total = sum(total))

ggplot(filter(read, ALFABET == 5), aes(reorder(category, total), total)) +
  geom_bar(stat = "identity") +
  facet_wrap(~NOM_ENT) +
  coord_flip() +
  ylab("ethnic group/race") +
  xlab("population") +
  scale_y_continuous(labels = comma) +
  ggtitle("Total population in Guerrero, Oaxaca and Veracruz,\nby Afro-Mexican and Indigenous self-identification\n(excludes people who didn't know or did not answer)")
ggsave("graphs/total_race.png",  dpi = 100, width = 9.6, height = 7)

afros %>%
  group_by(NOM_ENT, ENT) %>%
  summarise(total = sum(total),
            afro = sum(afro)) %>%
  mutate(per = afro / total) %>%
  arrange(desc(per))%>%
  filter(NOM_ENT %in% c("Guerrero", "Veracruz de Ignacio de la Llave",
                        "Oaxaca"))

#afros=read.csv("data/afros.csv", fileEncoding="cp1252")
write_csv(afros, "data/afros.csv")

muns = readOGR("map/mgm2013v6_2.shp", "mgm2013v6_2")
#
bb <- bbox(as(extent(muns) , "SpatialPolygons" ) )
muns@data$id = muns@data$concat
muns@data <- plyr::join(muns@data, afros, by = "id")

muns_df <- fortify(muns,region = "id")

muns_df <- merge(muns_df, afros, by="id")

## Theme for maps


theme_bare <-theme(axis.line=element_blank(),
                   axis.text.x=element_blank(),
                   axis.text.y=element_blank(),
                   axis.ticks=element_blank(),
                   axis.title.x=element_blank(),
                   axis.title.y=element_blank(),
                   panel.background=element_blank(),
                   panel.border=element_blank(),
                   panel.grid.major=element_blank(),
                   panel.grid.minor=element_blank(),
                   plot.background=element_blank())

gg <- ggplot()+ 
  geom_map(data=muns_df, map=muns_df,
                    aes(map_id=id, x=long, y=lat, group=group, fill=per),
                    color="white", size=0.04)+  
  scale_fill_viridis("percentage", trans = "log1p")+
  coord_map() +
  labs(x="", y="", title="Percentage of the population that identifies as Afro-Mexican\nor partially Afro-Mexican according to the 2015 Encuesta Intercensal")+
  coord_map("albers", lat0 = bb[ 2 , 1 ] , lat1 = bb[ 2 , 2 ] ) +
  theme_bw() + 
  theme(legend.key = element_rect( fill = NA)) +
  theme_bare
gg
ggsave("graphs/afros.png", plot = gg, dpi = 100, width = 9.6, height = 7)

gg <- ggplot()+ 
  geom_map(data=muns_df, map=muns_df,
           aes(map_id=id, x=long, y=lat, group=group, fill=per_ind),
           color="white", size=0.04)+  
  scale_fill_viridis("percentage", trans = "log1p", option="magma")+
  coord_map() +
  labs(x="", y="", title="Percentage of the population that identifies as indigenous\naccording to the 2015 Encuesta Intercensal")+
  coord_map("albers", lat0 = bb[ 2 , 1 ] , lat1 = bb[ 2 , 2 ] ) +
  theme_bw() + 
  theme(legend.key = element_rect( fill = NA)) +
  theme_bare
gg
ggsave("graphs/indigenous.png", plot = gg, dpi = 100, width = 9.6, height = 7)

muns$logper <- log1p(muns$per)
#muns$text <- str_c(muns$NOM_ENT, ", ", muns$NOM_MUN, " - ", muns$per)
pal <- colorNumeric(viridis(15), muns$logper)
leaflet(muns) %>% addTiles() %>%
  addPolygons(
    stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,
    color = ~pal(logper),
    popup = ~htmlEscape(str_c(NOM_MUN, " - Percent: ", round(per, 1)))
  ) %>%
  addLegend("bottomright", pal = pal, values = ~logper,
            labels = as.character(expm1(seq(0,4,.5))),
            title = "Percent Afro-Mexican<br/>(Totally or Partially)",
            labFormat = labelFormat(suffix = "%",
                                    transform = function(x) round(expm1(x))),
            opacity = 1
  )

ggplot(afros, aes(per)) +
  geom_density()


top10 <- afros[order(-afros$per),]
top10$per  <- str_c(round(top10$per, 1), "%")
htmlTable(top10[1:10, c("NOM_ENT", "NOM_MUN", "total", "afro", "per")],
          align="llcc|c",
          rnames=FALSE,
          col.rgroup = c("none", "#F7F7F7"),
          header = c("State", "Municipio", "Total Population", 
                     "Afro-Mexicans\n(totally or partially)", "Percent"))
ggplot(head(top10, 10), aes(NOM_MUN, per)) +
  geom_bar(stat="identity") +
  coord_flip()


top10 <- afros[order(-afros$afro),]
top10$per  <- str_c(round(top10$per, 1), "%")
htmlTable(top10[1:10, c("NOM_ENT", "NOM_MUN", "total", "afro", "per")],
          align="llc|cc",
          rnames=FALSE,
          col.rgroup = c("none", "#F7F7F7"),
          header = c("State", "Municipio", "Total Population", 
                     "Afro-Mexicans\n(totally or partially)", "Percent"))
