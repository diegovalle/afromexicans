# afros$per_part_afro <- (afros$afro_part) / afros$total
# afros$per_afro <- (afros$afro) / afros$total

states <- afros %>%
  group_by(NOM_ENT) %>%
  summarise(count_afro = sum(afro, na.rm = TRUE), 
            count_part_afro = sum(afro_part, na.rm = TRUE), 
            pop = sum(total, na.rm = TRUE)) %>%
  mutate(per_afro = count_afro / pop,
         per_part_afro = count_part_afro / pop)

states <- gather(states[,c("NOM_ENT", "per_afro", "per_part_afro")], 
                type, percentage, per_afro:per_part_afro)

ggplot(states, aes(reorder(NOM_ENT, percentage), percentage, fill = type)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(labels = percent) +
  xlab("state") +
  ylab("percentage of the population") +
  scale_fill_manual("type", values = c("black", "#555555"),
                     labels = c("Afro-Mexican", "Part Afro-Mexican")) +
  ggtitle("Percentage of the population that is Afro-Mexican or partially Afro-Mexican, by state")
ggsave("graphs/states_afro.png",  dpi = 100, width = 9.6, height = 7)

