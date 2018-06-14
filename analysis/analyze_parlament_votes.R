library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)
library(rMMLEDirBer)

# Load dataset and rename parliamentart groups for readability
# UPyD labelled as Ciudadanos
# GIP (Izquierda Plural) labelled as UP (Unidos Podemos)
# In the dataset, CiU + DL members are never considered into Grupo Mixto
# because I want to explicitly analyze their behavior
devtools::load_all()
data("ESvotes_calls")
ESvotes_calls$grupo[ESvotes_calls$grupo == 'GP']         <-  "PP"
ESvotes_calls$grupo[ESvotes_calls$grupo == 'GCs']        <-  "Cs"
ESvotes_calls$grupo[ESvotes_calls$grupo == 'GUPyD']      <-  "Cs"
ESvotes_calls$grupo[ESvotes_calls$grupo == 'GV (EAJ-PNV)'] <-"PNV"
ESvotes_calls$grupo[ESvotes_calls$grupo == 'GC-CiU']     <-  "CiU"
ESvotes_calls$grupo[ESvotes_calls$grupo == 'GC-DL']      <-  "CiU"
ESvotes_calls$grupo[ESvotes_calls$grupo == 'GS']         <-  "PSOE"
ESvotes_calls$grupo[ESvotes_calls$grupo == 'GER']        <-  "ERC"
ESvotes_calls$grupo[ESvotes_calls$grupo == 'GIP']        <-  "UP"
ESvotes_calls$grupo[ESvotes_calls$grupo == 'GP-EC-EM']   <-  "UP"
ESvotes_calls$grupo[ESvotes_calls$grupo == 'GCUP-EC-EM'] <-  "UP"
ESvotes_calls$grupo[ESvotes_calls$grupo == 'GMx']        <-  "Mx"

sorted.groups <- c("PP", "Cs", "PNV", "CiU", "PSOE", "ERC", "UP", "Mx")
ESvotes_calls$grupo <- factor(ESvotes_calls$grupo, levels = sorted.groups)
df <- ESvotes_calls %>% filter(legislatura >= 10)

# Assign unique id to each call
df <- df %>% arrange(legislatura, sesion, votacion)
df$idvotacion <- group_indices(df, legislatura, sesion, votacion)

##########################"
# Add member-group label so that we can plot name - party
df.members   <- df %>% select(diputado, grupo) %>% distinct() %>% arrange(grupo, diputado)
memberlabels <- paste(df.members$diputado, df.members$grupo, sep = " - ")
df.members$memberlabel <- factor(memberlabels, levels = memberlabels)
df <- merge(df, df.members)
  
# Como votan los diputados -----------------------------------------------------
base_size <- 6
breaks <- levels(df$memberlabel)[seq(1,length(levels(df$memberlabel)), by=10)]

p <- ggplot2::ggplot(df, aes(x=idvotacion, y=memberlabel)) + 
  geom_tile(aes(fill = voto)) + 
  scale_fill_manual(values=c("blue", "red", "black", "green"), na.value = 'white',
                    labels = c("Abstención", "No", "No vota", "Sí")) +
  scale_y_discrete(expand = c(0, 0), breaks = breaks) +
  scale_x_continuous(expand = c(0, 0)) +
  theme_bw() +  
  xlab("votación") + ylab("") +   coord_fixed() +
  theme(
    rect = element_rect(fill = ggthemes::ggthemes_data$wsj$bg["brown"]),
    axis.text.y   = element_text(size=base_size*1, hjust = 1, colour = "black"),
    axis.text.x   = element_text(size=base_size*1, colour = "black"),
    axis.title.x  = element_text(color="black", size=8, face="italic"),
    axis.title.y  = element_text(color="black", size=8, face="italic"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.position  = "top",
    legend.title  = element_blank(),
    legend.box    = "horizontal" ,
    legend.text   = element_text(size=7),
    legend.key    = element_rect(colour = "black"),
    legend.key.size = unit(0.7,"line"),
    plot.title    = element_text(size = 10, face = "bold",  hjust = 1),
    aspect.ratio=2) + ggtitle("Votaciones de los diputados entre 2011 y 2018")
print(p)
ggsave(p, filename = paste0("fig20_matriz_votos_legislaturas_wsj.png"), 
       height=16, width=12, units='cm')


# Que votan los partidos -------------------------------------------------------
df.call_by_group <- df %>% 
  group_by(idvotacion, legislatura, sesion, votacion, fecha, grupo) %>% 
  summarise(noes  = sum(voto == "No"),
            sies  = sum(voto == "Sí"),
            abs   = sum(voto == "Abstención"),
            na    = sum(voto == "No vota"),
            total = n()) %>% ungroup()

df.call_by_group$voto <- apply(cbind(df.call_by_group$noes, 
                                     df.call_by_group$sies, 
                                     df.call_by_group$abs, 
                                     df.call_by_group$na), 
                               1, which.max)
df.call_by_group$voto[df.call_by_group$voto == 4 ] <- NA
df.call_by_group$voto <- factor(df.call_by_group$voto, 
                                labels = c("no", "sí", "abs"))
df.call_by_group <- df.call_by_group %>% 
  select(idvotacion, legislatura, fecha, 
         sesion, votacion, grupo, voto)


base_size <- 12
p <- ggplot2::ggplot(df.call_by_group, aes(x=idvotacion, y=grupo)) + 
  geom_tile(aes(fill = voto)) + 
  scale_fill_manual(values=c("red", "green", "blue"), na.value = 'black',
                    labels = c("No", "Sí", "Abstención", "No vota")) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  theme_bw() +  
  xlab("votación") + ylab("") +   coord_fixed() +
  theme(
    rect = element_rect(fill = ggthemes::ggthemes_data$wsj$bg["brown"]),
    axis.text.y   = element_text(size=base_size, hjust = 1, colour = "black"),
    axis.text.x   = element_text(size=base_size, colour = "black"),
    axis.title.x  = element_text(color="black", size=base_size, face="italic"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.position  = "top",
    legend.title  = element_blank(),
    legend.box    = "horizontal" ,
    legend.text   = element_text(size=base_size*0.9),
    legend.key    = element_rect(colour = "black"),
    legend.key.size = unit(0.9,"line"),
    plot.title    = element_text(size = base_size*1.5, face = "bold",  hjust = 1),
    aspect.ratio=0.38) + ggtitle("Votaciones de los grupos entre 2011 y 2018")
print(p)
ggsave(p, filename = paste0("fig21_matriz_votos_partidos_legislaturas.png"), 
       height=10, width=20, units='cm')

# Correlations -----------------------------------------------------------------

# Abstenstions as NA so that they do not count as a disagreement vote
df.call_by_group$voto[as.character(df.call_by_group$voto) == "abs"] <- NA

# Compute correlations
df.votaciones <- spread(df.call_by_group, grupo, voto)
df.corrs <-  data.frame()
groups <- levels(df.call_by_group$grupo)
for(nlegis in 10:12){
  for(g1 in groups){
    for(g2 in groups){
      votes1 <- df.votaciones %>% filter(legislatura == nlegis) %>% select(g1)
      votes2 <- df.votaciones %>% filter(legislatura == nlegis) %>% select(g2)
      corr <- mean(votes1 == votes2, na.rm = TRUE)
      df.corrs <-bind_rows(df.corrs, 
                           list(from=g1, to = g2, corr =  corr, legislatura = nlegis))
    }
  }
}

df.corrs$from <- factor(df.corrs$from, levels = sorted.groups)
df.corrs$to   <- factor(df.corrs$to, levels = sorted.groups)
df.corrs$legislatura[df.corrs$legislatura==10] <- "X Legistatura"
df.corrs$legislatura[df.corrs$legislatura==11] <- "XI Legistatura"
df.corrs$legislatura[df.corrs$legislatura==12] <- "XII Legistatura"
df.corrs$legislatura <- factor(df.corrs$legislatura, 
                               levels = c("X Legistatura",
                                          "XI Legistatura",
                                          "XII Legistatura"))
base_size <- 12
p <- ggplot2::ggplot(df.corrs, aes(x=from, y=to)) + 
  geom_tile(aes(fill = corr)) +
  geom_text(aes(label = round(corr, 1)))+
  scale_fill_gradient(low = "white", high = "steelblue", na.value = 'black') +
  scale_y_discrete(expand = c(0, 0), breaks = sorted.groups) +
  scale_x_discrete(expand = c(0, 0), breaks = sorted.groups) +
  facet_grid(.~legislatura) +
  theme_bw() +  xlab("") + ylab("") +   coord_fixed() +
  theme(
    rect = element_rect(fill = ggthemes::ggthemes_data$wsj$bg["brown"]),
    strip.background = element_rect(fill="white"),
    strip.text.x =  element_text(size = base_size),
    axis.text.y   = element_text(size=base_size*1, hjust = 1, colour = "black"),
    axis.text.x   = element_text(size=base_size*1, hjust = 1, colour = "black", angle = 90),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    legend.title = element_blank(),
    legend.key= element_rect(colour = "black"),
    plot.title    = element_text(size = base_size*1.5, face = "bold",  hjust = 1),
    aspect.ratio=1) + ggtitle("Similitud entre partidos en cuanto al sentido del voto")
print(p)
ggsave(p, filename = paste0("fig22_matriz_votos_partidos_correlacion.png"), 
       height=10, width=20, units='cm')

# Absentistas---- -------------------------------------------------------------
df.absentismo <- df %>% group_by(diputado, grupo, memberlabel) %>% 
  summarise(noes  = sum(voto == "No"),
            sies  = sum(voto == "Sí"),
            abs   = sum(voto == "Abstención"),
            na    = sum(voto == "No vota"),
            total = n()) %>% ungroup() %>%
  mutate(absentismo = na/total) %>%
  arrange(-absentismo)


base_size <- 10
df.absentismo$memberlabel <- factor(df.absentismo$memberlabel, levels = df.absentismo$memberlabel)
p <- ggplot(head(df.absentismo,75), aes(x=memberlabel, y=absentismo)) + 
  geom_col(width = 0.7) + coord_flip() +
  scale_y_continuous(expand = c(0, 0.01), labels=percent, lim = c(0,1)) +
  theme_bw() +  xlab("") + ylab("Porcentaje de ausencias") +
  theme(
    rect = element_rect(fill = ggthemes::ggthemes_data$wsj$bg["brown"]),
    axis.text.y   = element_text(size=base_size*0.7, hjust = 1, colour = "black"),
    axis.text.x   = element_text(size=base_size*1, hjust = 1, colour = "black"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = ggthemes::ggthemes_data$wsj$bg["brown"],
                                   colour = "lightblue"),
    legend.position = "none",
    legend.title = element_blank(),
    legend.key= element_rect(colour = "black"),
    plot.title    = element_text(size = base_size*1.5, face = "bold",  hjust = 1)
    ) + ggtitle("Absentismo entre los diputados")
print(p)
ggsave(p, filename = paste0("fig23_absentismo_parlamentario100.png"), 
       height=20, width=20, units='cm')
