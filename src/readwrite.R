
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
             (PERTE_INDIGENA == 1 | PERTE_INDIGENA == 2 | PERTE_INDIGENA == 3) &
             (EDAD > 15 & EDAD < 111)) %>%
    summarise(total = sum(FACTOR)) %>%
    left_join(
      df%>%
        group_by(ENT, NOM_ENT,  AFRODES, ALFABET, PERTE_INDIGENA) %>%
        # 1 == Sí, 2 == Sí, en parte 
        filter((ALFABET == 5 | ALFABET == 7) & 
                 (AFRODES == 1 | AFRODES == 2 | AFRODES == 3) &
                 (PERTE_INDIGENA == 1 | PERTE_INDIGENA == 2 | PERTE_INDIGENA == 3) &
                 (EDAD > 15 & EDAD < 111)) %>%
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
  xlab("ethnic group") +
  ylab("population") +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))+
  ggtitle("Total population in Guerrero, Oaxaca and Veracruz,\nby Afro-Mexican and Indigenous self-identification\n(excludes people who didn't know or did not answer)")
ggsave("graphs/total_race.png",  dpi = 100, width = 9.6, height = 7)

read$indigenous <- ifelse(read$category %in% c("Indigenous",
                                               "Afro-Mexican & Indigenous",
                                               "Part Afro-Mexican & Indigenous"), TRUE, FALSE)
ggplot(filter(read, ALFABET == 5), aes(reorder(category, per), per)) +
  geom_point(stat = "identity", aes(size = total, color = indigenous)) +
  facet_wrap(~NOM_ENT) +
  coord_flip() +
  scale_y_continuous(labels = percent) +
  scale_size("population", labels=comma, range = c(2, 10)) +
  ylab("percent who can read and write a simple statement") +
  xlab("ethnic group") +
  theme_bw() +
  ggtitle("Percentage of the population age 15 and above who can, with understanding,\nread and write a short, simple statement")
ggsave("graphs/read_race.svg",  dpi = 100, width = 9.9, height = 7)
