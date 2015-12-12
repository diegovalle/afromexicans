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
        filter(AFRODES %in% c(1, 2)) %>%
        summarise(afro = sum(FACTOR))
    ) %>%
    mutate(per = afro/total * 100) %>%
    arrange(desc(per))
  afros <- rbind(afros, afros_state)
  rm(df)
  gc()
}
afros$id <- str_c(afros$ENT, afros$MUN)
afros[is.na(afros$per), "per"] <- 0
afros[is.na(afros$per), "afro"] <- 0
sum(afros$afro, na.rm = TRUE)

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
                    aes(map_id=id, x=long, y=lat, group=group, fill=log1p(per)),
                    color="white", size=0.05)+  
  scale_fill_viridis("log(1+x) percentage")+
  coord_map() +
  labs(x="", y="", title="Percentage of the population that identifies as Afro-Mexican\naccording to the 2015 Encuesta Intercensal")+
  coord_map("albers", lat0 = bb[ 2 , 1 ] , lat1 = bb[ 2 , 2 ] ) +
  theme_bw() + 
  theme(legend.key = element_rect( fill = NA)) +
  theme_bare
gg
ggsave("graphs/afros.png", plot = gg, dpi = 100, width = 9.6, height = 8)

#muns$logper <- log1p(muns$per)
#muns$text <- str_c(muns$NOM_ENT, ", ", muns$NOM_MUN, " - ", muns$per)
pal <- colorNumeric(viridis(15), muns$per)
leaflet(muns) %>% addTiles() %>%
  addPolygons(
    stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,
    color = ~pal(per),
    popup = ~htmlEscape(str_c(NOM_MUN, "= Percent: ", round(per, 1)))
  ) %>%
  addLegend("bottomright", pal = pal, values = ~per,
            title = "Percent Afro-Mexican",
            opacity = 1
  )
