

afros$ENT <- as.integer(afros$ENT)
afros$MUN <- as.integer(afros$MUN)
# afros$per <- (afros$afro + afros$afro_part) / afros$total

corr_chart <- function(years, years_str, filename) {
  hom <- injury.intent %>%
    filter(state_reg %in% c(12,20,30) & intent == "Homicide" & year_reg %in% c(years)) %>%
    group_by(state_reg, mun_reg) %>%
    summarise(homicides = n()) %>%
    left_join(afros, by = c('state_reg' = 'ENT', 'mun_reg' = 'MUN')) %>%
    mutate(rate = homicides / (total * (max(years) - min(years))) * 10^5) %>%
    mutate(NOM_ENT = str_replace(NOM_ENT, " de Ignacio de la Llave", ""))
  
  dfcoef <- hom %>%
    group_by(NOM_ENT) %>%
    do(fit = lm(rate ~ per,data = .))
  print(tidy(dfcoef, fit))
  
  gg <- ggplot(hom, aes(per, rate, group = NOM_ENT, color = NOM_ENT)) +
    geom_point(alpha = .5) +
    geom_smooth(method = lm) +
    scale_color_discrete("State") +
    scale_x_continuous(labels = percent) +
    xlab("percentage who self-identifies as Afro-Mexican or partially Afro-Mexican") +
    ylab(str_c("homicide rate ", years_str)) +
    ggtitle("Correlations between homicide rates and percentage Afro-Mexican\nat the municipio level, by state")
  ggsave(filename, plot = gg, dpi = 100, width = 9, height = 5)
}
corr_chart(2008:2014, "2008-2014", "graphs/homicides_drugwar.svg")
corr_chart(2004:2007, "2004-2007", "graphs/homicides_before.svg")
