
muns = readOGR("map/mgm2013v6_2.shp", "mgm2013v6_2")

states <- readOGR("map/mge2013v6_2.shp", "mge2013v6_2")
states_df <- fortify(states)
#
bb <- bbox(as(extent(muns) , "SpatialPolygons" ) )
muns@data$id = as.numeric(muns@data$concat)
muns@data <- plyr::join(muns@data, afros, by = "id")

muns_df <- fortify(muns,region = "id")

muns_df <- plyr::join(muns_df, afros, by="id")

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

gg_a <- ggplot()+ 
  geom_map(data=muns_df, map=muns_df,
           aes(map_id=id, x=long, y=lat, group=group, fill=per),
           color="white", size=0.04) + 
  geom_polygon(data = states_df,
               aes(long, lat, group = group),
               color = "#aaaaaa", fill = NA, size = 0.3) +
  scale_fill_viridis("percentage", trans = "sqrt", labels = percent)+
  coord_map() +
  labs(x="", y="", title="Percentage of the population that self-identifies as Afro-Mexican\nor partially Afro-Mexican according to the 2015 encuesta intercensal")+
  coord_map("albers", lat0 = bb[ 2 , 1 ] , lat1 = bb[ 2 , 2 ] ) +
  theme_bw() + 
  theme(legend.key = element_rect( fill = NA)) +
  theme_bare
ggsave("graphs/afromexicanos.png", plot = gg_a, dpi = 100, width = 9.6, height = 7)

gg_i <- ggplot()+ 
  geom_map(data=muns_df, map=muns_df,
           aes(map_id=id, x=long, y=lat, group=group, fill=per_ind),
           color="white", size=0.04) + 
  geom_polygon(data = states_df,
               aes(long, lat, group = group),
               color = "#aaaaaa", fill = NA, size = 0.3) +  
  scale_fill_viridis("percentage",  option="magma", labels = percent)+
  coord_map() +
  labs(x="", y="", title="Percentage of the population that self-identifies as indigenous\naccording to the 2015 encuesta intercensal")+
  coord_map("albers", lat0 = bb[ 2 , 1 ] , lat1 = bb[ 2 , 2 ] ) +
  theme_bw() + 
  theme(legend.key = element_rect( fill = NA)) +
  theme_bare
ggsave("graphs/indigenous.png", plot = gg_i, dpi = 100, width = 9.6, height = 7)



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
