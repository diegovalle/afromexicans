
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

#Download the encuesta intercensal microdata
for(i in 1:32) {
  download(str_c(URL, str_pad(i, 2, "left", "0"), "_csv.zip&ht=02"),
           str_c("data/", i), mode = "wb")
  unzip(str_c("data/", i), exdir = "data")
  file.remove(str_c("data/", i))
}

# Read the census file for each state
# compute the number of Afro-Mexicans and part Afro-Mexicans along
# with population total
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
# For Afro-Mexicans compute the percentage with partials
afros$per <- (afros$afro + afros$afro_part) / afros$total
# Ignore people who are part indigenous since it is a very small percentage
afros$per_ind <- (afros$ind) / afros$total
# Municipios with NA really should be 0
afros[is.na(afros$per), "per"] <- 0
afros[is.na(afros$per), "afro"] <- 0
afros[is.na(afros$per_ind), "per_ind"] <- 0
afros[is.na(afros$per_ind), "ind"] <- 0

round((sum(afros$afro, na.rm = TRUE)+sum(afros$afro_part, na.rm = TRUE))/sum(afros$total, na.rm = TRUE),3)

#afros=read.csv("data/afros.csv", fileEncoding="cp1252")
write_csv(afros, "data/afros.csv")

# Correlation between people who are Afro-Mexican and part Afro-Mexican
ggplot(afros, aes(afro, afro_part)) +
  geom_point() +
  geom_smooth(method = lm)

# Total Afro-Mexicans by state
totals=afros %>%
  group_by(NOM_ENT, ENT) %>%
  summarise(total = sum(total, na.rm = TRUE),
            afro = sum(afro + afro_part, na.rm = TRUE)) %>%
  mutate(per = afro / total) %>%
  arrange(desc(per))#%>%
#   filter(NOM_ENT %in% c("Guerrero", "Veracruz de Ignacio de la Llave",
#                         "Oaxaca"))


#Afro-Mexicans are concentrated in a few municipios
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
