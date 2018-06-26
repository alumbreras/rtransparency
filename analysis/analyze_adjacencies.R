library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)
library(rMMLEDirBer)

devtools::load_all()

# BADALONA ---------------------------------------------------------------------
data("BDN_parliament_adj")
parliament_adj <- BDN_parliament_adj

# Load data. Sort members by party -twitter
df.adj <- parliament_adj$adj
df.info <- parliament_adj$info %>% arrange(party, twitter)
df.info$twitter <- factor(df.info$twitter, levels = df.info$twitter)
df.adj$from <- factor(df.adj$from, levels = df.info$twitter)
df.adj$to <- factor(df.adj$to, levels = df.info$twitter)
df.adj$value <- 1

twitters <- df.info$twitter
labels <- paste0(as.character(df.info$twitter), '-', as.character(df.info$party))
df.adj$from.label <- df.adj$from
df.adj$to.label <- df.adj$to
levels(df.adj$from.label) <- labels[match(levels(df.adj$from), twitters)]
levels(df.adj$to.label) <- labels[match(levels(df.adj$to), twitters)]

# Plot adjacency matrix
base_size <- 8
breaks <- levels(df.adj$from.label)[seq(1,length(levels(df.adj$from.label)), by=1)]
first.mp <- as.numeric(c(TRUE, diff(as.numeric(df.info$party))!=0))
lines <- which(first.mp==1)-0.5
p <- ggplot2::ggplot(df.adj, aes(x=to.label, y=from.label)) + 
  geom_tile(aes(fill = value)) + 
  geom_hline(yintercept = lines, linetype = "dotted", color = 'blue') +
  geom_vline(xintercept = lines, linetype = "dotted", color = 'blue') +
  scale_fill_gradient(low = "white", high = "black", na.value = 'white') +
  scale_x_discrete(expand = c(0, 0), breaks = breaks) +
  scale_y_discrete(expand = c(0, 0), breaks = breaks) +
  theme_bw() +  
  xlab("hacia") + ylab("desde") +   coord_fixed() +
  theme(
    rect = element_rect(fill = ggthemes::ggthemes_data$wsj$bg["brown"]),
    axis.text.y   = element_text(size=base_size*1, hjust = 1, colour = "black"),
    axis.text.x   = element_text(size=base_size*1, vjust = 0.5, hjust = 1, colour = "black", angle = 90),
    axis.title.x  = element_text(color="black", size=10, face="italic"),
    axis.title.y  = element_text(color="black", size=10, face="italic"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.position  = "none",
    plot.title    = element_text(size = 12, face = "bold",  hjust = 1),
    aspect.ratio=1) + ggtitle("Quién sigue a quién en Badalona")
print(p)
ggsave(p, filename = paste0("fig24_matriz_adj_BDN.png"), 
       height=16, width=16, units='cm')

plot_similarities(df.adj, df.info, "party")

# CATALUNYA --------------------------------------------------------------------

data("CAT_parliament_adj")
parliament_adj <- CAT_parliament_adj


# Load data. Sort members by party -twitter
df.adj <- parliament_adj$adj
df.info <- parliament_adj$info

df.info <- df.info %>% group_by(party) %>% mutate(size = n()) %>% ungroup()
df.info <- df.info %>% arrange(-size, party, twitter)
df.info$party <- factor(df.info$party,
                        levels = unique(as.character(df.info$party)))
df.info$twitter <- factor(df.info$twitter, 
                          levels = unique(as.character(df.info$twitter)))
df.info$twitter <- factor(df.info$twitter, levels = df.info$twitter)
df.adj$from <- factor(df.adj$from, levels = df.info$twitter)
df.adj$to <- factor(df.adj$to, levels = df.info$twitter)
df.adj$value <- 1

twitters <- df.info$twitter
labels <- paste0(as.character(df.info$twitter), '-', as.character(df.info$party))
df.adj$from.label <- df.adj$from
df.adj$to.label <- df.adj$to
levels(df.adj$from.label) <- labels[match(levels(df.adj$from), twitters)]
levels(df.adj$to.label) <- labels[match(levels(df.adj$to), twitters)]

# Plot adjacency matrix
base_size <- 8
idx <- seq(1,length(levels(df.adj$from.label)), by=1)
breaks <- levels(df.adj$from.label)[idx]

parties <- df.info$party[match(levels(df.adj$from), df.info$twitter)]
labels <- rep("",  length(breaks))
idx.centers <- as.vector(ceiling(cumsum(table(parties))- table(parties)/2))
labels[idx.centers] <- as.character(parties[idx.centers])

df.adj$from <- droplevels(df.adj$from)
df.adj$to <- droplevels(df.adj$to)
groups <- df.info$party[match(levels(df.adj$from), df.info$twitter)]
first.mp.from <- as.numeric(c(TRUE, diff(as.numeric(groups))!=0))
groups <- df.info$party[match(levels(df.adj$to), df.info$twitter)]
first.mp.to <- as.numeric(c(TRUE, diff(as.numeric(groups))!=0))
lines.from <- which(first.mp.from==1)-0.5
lines.to   <- which(first.mp.to==1)-0.5

p <- ggplot2::ggplot(df.adj, aes(x=to.label, y=from.label)) + 
  geom_tile(aes(fill = value)) + 
  geom_hline(yintercept = lines.from, linetype = "dotted", color = 'blue') +
  geom_vline(xintercept = lines.to, linetype = "dotted", color = 'blue') +
  scale_fill_gradient(low = "white", high = "black", na.value = 'white') +
  scale_y_discrete(expand = c(0, 0), breaks = breaks, labels = labels) +
  scale_x_discrete(expand = c(0, 0), breaks = breaks, labels = labels) +
  theme_bw() +  
  xlab("hacia") + ylab("desde") +   coord_fixed() +
  theme(
    rect = element_rect(fill = ggthemes::ggthemes_data$wsj$bg["brown"]),
    axis.text.y   = element_text(size=base_size*1, hjust = 1, colour = "black"),
    axis.text.x   = element_text(size=base_size*1, vjust = 0.5, hjust = 1, colour = "black", angle = 90),
    axis.title.x  = element_text(color="black", size=10, face="italic"),
    axis.title.y  = element_text(color="black", size=10, face="italic"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.position  = "none",
    plot.title    = element_text(size = 12, face = "bold",  hjust = 1),
    aspect.ratio=1) + ggtitle("Quién sigue a quién en el Parlament")
print(p)
ggsave(p, filename = paste0("fig25_matriz_adj_CAT.png"), 
       height=16, width=16, units='cm')

plot_similarities(df.adj, df.info, "party")


# ESPAÑA -----------------------------------------------------------------------

data("ES_parliament_adj")
parliament_adj <- ES_parliament_adj

# Load data. Sort members by party -twitter
df.adj <- parliament_adj$adj
df.info <- parliament_adj$info

df.info <- df.info %>% group_by(party) %>% mutate(size = n()) %>% ungroup()
df.info <- df.info %>% arrange(-size, party, twitter)
df.info$party <- factor(df.info$party,
                        levels = unique(as.character(df.info$party)))
df.info$twitter <- factor(df.info$twitter, 
                          levels = unique(as.character(df.info$twitter)))

df.info$twitter <- factor(df.info$twitter, levels = df.info$twitter)
df.adj$from <- factor(df.adj$from, levels = df.info$twitter)
df.adj$to <- factor(df.adj$to, levels = df.info$twitter)
df.adj$value <- 1



twitters <- as.character(df.info$twitter)

labels <- paste(as.character(df.info$twitter), as.character(df.info$party))
df.adj$from.label <- df.adj$from
df.adj$to.label <- df.adj$to
levels(df.adj$from.label) <- labels[match(levels(df.adj$from), twitters)]
levels(df.adj$to.label) <- labels[match(levels(df.adj$to), twitters)]


# Plot adjacency matrix
base_size <- 8
breaks <- levels(df.adj$from.label)[seq(1,length(levels(df.adj$from.label)), by=1)]

parties <- df.info$party[match(levels(df.adj$from), df.info$twitter)]
labels <- rep("",  length(breaks))
idx.centers <- as.vector(ceiling(cumsum(table(parties))- table(parties)/2))
labels[idx.centers] <- as.character(parties[idx.centers])


df.adj$from <- droplevels(df.adj$from)
df.adj$to <- droplevels(df.adj$to)
groups <- df.info$party[match(levels(df.adj$from), df.info$twitter)]
first.mp.from <- as.numeric(c(TRUE, diff(as.numeric(groups))!=0))
groups <- df.info$party[match(levels(df.adj$to), df.info$twitter)]
first.mp.to <- as.numeric(c(TRUE, diff(as.numeric(groups))!=0))
lines.from <- which(first.mp.from==1)-0.5
lines.to   <- which(first.mp.to==1)-0.5

p <- ggplot2::ggplot(df.adj, aes(x=to.label, y=from.label)) + 
  geom_tile(aes(fill = value)) + 
  geom_hline(yintercept = lines.from, linetype = "dotted", color = 'blue') +
  geom_vline(xintercept = lines.to, linetype = "dotted", color = 'blue') +
  scale_fill_gradient(low = "white", high = "black", na.value = 'white') +
  scale_y_discrete(expand = c(0, 0), breaks = breaks, labels = labels) +
  scale_x_discrete(expand = c(0, 0), breaks = breaks, labels = labels) +
  theme_bw() +  
  xlab("hacia") + ylab("desde") +   coord_fixed() +
  theme(
    rect = element_rect(fill = ggthemes::ggthemes_data$wsj$bg["brown"]),
    axis.text.y   = element_text(size=base_size*1, hjust = 1, colour = "black"),
    axis.text.x   = element_text(size=base_size*1, hjust = 1, colour = "black", angle = 90),
    axis.title.x  = element_text(color="black", size=10, face="italic"),
    axis.title.y  = element_text(color="black", size=10, face="italic"),
    axis.ticks = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.position  = "none",
    plot.title    = element_text(size = 12, face = "bold",  hjust = 1),
    aspect.ratio=1) + ggtitle("Quién sigue a quién en el Congreso")
print(p)
ggsave(p, filename = paste0("fig26_matriz_adj_ES.png"), 
       height=16, width=16, units='cm')

plot_similarities(df.adj, df.info, "party")


# EUROPE -------------------------------------------------------------------------

data("EU_parliament_adj")
parliament_adj <- EU_parliament_adj
parliament_adj$info$id <- NULL

# Load data. Sort members by party -twitter
df.adj  <- parliament_adj$adj
df.info <- parliament_adj$info
df.adj$value <- 1

####################################################

length(unique(df.adj$from.label))
length(unique(df.adj$to.label))

# Add initials to Political Groups
df.info$politicalGroup <- as.character(df.info$politicalGroup)
df.info$politicalGroup[grepl("People's Party", df.info$politicalGroup)] <- "EPP (PP)"
df.info$politicalGroup[grepl("Socialists", df.info$politicalGroup)] <- "S&D (PSOE)"
df.info$politicalGroup[grepl("Conservatives", df.info$politicalGroup)] <- "ECR (Tories)"
df.info$politicalGroup[grepl("Liberals", df.info$politicalGroup)] <- "ALDE (C's, PDCat)"
df.info$politicalGroup[grepl("Left", df.info$politicalGroup)] <- "GUE-NGL (IU)"
df.info$politicalGroup[grepl("Greens", df.info$politicalGroup)] <- "Greens-EFA (EQUO)"
df.info$politicalGroup[grepl("Direct Democracy", df.info$politicalGroup)] <- "EFDD (UKIP, M5)"
df.info$politicalGroup[grepl("Nations and Freedom", df.info$politicalGroup)] <- "ENL (FN,LN)"
df.info$politicalGroup[grepl("Non-attached Members", df.info$politicalGroup)] <- "NI"
df.info$politicalGroup <- as.factor(df.info$politicalGroup)

df.info$country <- as.character(df.info$country )
df.info$country[grepl("Belgium", df.info$country)] <- "BE"
df.info$country[grepl("Bulgaria", df.info$country)] <- "BG"
df.info$country[grepl("Czech Republic", df.info$country)] <- "CZ"
df.info$country[grepl("Denmark", df.info$country)] <- "DK"
df.info$country[grepl("Germany", df.info$country)] <- "DE"
df.info$country[grepl("Estonia", df.info$country)] <- "EE"
df.info$country[grepl("Ireland", df.info$country)] <- "IE"
df.info$country[grepl("Greece", df.info$country)] <- "EL"
df.info$country[grepl("Spain", df.info$country)] <- "ES"
df.info$country[grepl("France", df.info$country)] <- "FR"
df.info$country[grepl("Hungary", df.info$country)] <- "HU"
df.info$country[grepl("Croatia", df.info$country)] <- "HR"
df.info$country[grepl("Lithuania", df.info$country)] <- "LT"
df.info$country[grepl("Luxembourg", df.info$country)] <- "LU"
df.info$country[grepl("Malta", df.info$country)] <- "MT"
df.info$country[grepl("Italy", df.info$country)] <- "IT"
df.info$country[grepl("Netherlands", df.info$country)] <- "NL"
df.info$country[grepl("Cyprus", df.info$country)] <- "CY"
df.info$country[grepl("Austria", df.info$country)] <- "AT"
df.info$country[grepl("Latvia", df.info$country)] <- "LV"
df.info$country[grepl("Poland", df.info$country)] <- "PL"
df.info$country[grepl("Portugal", df.info$country)] <- "PT"
df.info$country[grepl("Romania", df.info$country)] <- "RO"
df.info$country[grepl("Slovenia", df.info$country)] <- "SK"
df.info$country[grepl("Slovakia", df.info$country)] <- "SK"
df.info$country[grepl("Finland", df.info$country)] <- "FI"
df.info$country[grepl("Sweden", df.info$country)] <- "SE"
df.info$country[grepl("United Kingdom", df.info$country)] <- "UK"
df.info$country <- as.factor(df.info$country)

df.info <- df.info %>% group_by(politicalGroup) %>% mutate(size = n()) %>% ungroup()
df.info <- df.info %>% arrange(-size, politicalGroup, country, nationalPoliticalGroup,
                                           fullName, twitter)

df.info$politicalGroup <- factor(df.info$politicalGroup,
                                levels = unique(as.character(df.info$politicalGroup)))

df.info <- df.info %>% filter(twitter != "")


# Twitters and complete labels as twitter-group-country
twitters <- df.info$twitter
labels <- paste(as.character(df.info$twitter), as.character(df.info$politicalGroup), 
                as.character(df.info$country))

df.info$twitter <- factor(df.info$twitter, levels = df.info$twitter)
df.adj$from <- factor(df.adj$from, levels = df.info$twitter)
df.adj$to <- factor(df.adj$to, levels = df.info$twitter)

df.adj$from.label <- df.adj$from
df.adj$to.label <- df.adj$to
levels(df.adj$from.label) <- labels[match(levels(df.adj$from), twitters)]
levels(df.adj$to.label) <- labels[match(levels(df.adj$to), twitters)]


# Plot adjacency matrix
base_size <- 8
breaks <- levels(df.adj$from.label)
parties <- df.info$politicalGroup[match(levels(df.adj$from), df.info$twitter)]
labels <- rep("",  length(breaks))
idx.centers <- as.vector(ceiling(cumsum(table(parties))- table(parties)/2))
labels[idx.centers] <- as.character(parties[idx.centers])

df.adj$from <- droplevels(df.adj$from)
df.adj$to <- droplevels(df.adj$to)
groups <- df.info$politicalGroup[match(levels(df.adj$from), df.info$twitter)]
first.mp.from <- as.numeric(c(TRUE, diff(as.numeric(groups))!=0))
groups <- df.info$politicalGroup[match(levels(df.adj$to), df.info$twitter)]
first.mp.to <- as.numeric(c(TRUE, diff(as.numeric(groups))!=0))
lines.from <- which(first.mp.from==1)-0.5
lines.to   <- which(first.mp.to==1)-0.5

p <- ggplot2::ggplot(df.adj, aes(x=to.label, y=from.label)) + 
  geom_tile(aes(fill = value)) + 
  scale_fill_gradient(low = "white", high = "black", na.value = 'white') +
  geom_hline(yintercept = lines.from, linetype = "dotted", color = 'blue') +
  geom_vline(xintercept = lines.to, linetype = "dotted", color = 'blue') +  
  scale_y_discrete(expand = c(0, 0), breaks = breaks, labels = labels) +
  scale_x_discrete(expand = c(0, 0), breaks = breaks, labels = labels) +
  theme_bw() +  
  xlab("hacia") + ylab("desde") +   coord_fixed() +
  theme(
    rect = element_rect(fill = ggthemes::ggthemes_data$wsj$bg["brown"]),
    axis.text.y   = element_text(size=base_size*1, hjust = 1, colour = "black"),
    axis.text.x   = element_text(size=base_size*1, hjust = 1, vjust = 1, colour = "black", angle = 90),
    axis.title.x  = element_text(color="black", size=10, face="italic"),
    axis.title.y  = element_text(color="black", size=10, face="italic"),
    axis.ticks = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.position  = "none",
    plot.title    = element_text(size = 12, face = "bold",  hjust = 1),
    aspect.ratio=1) + ggtitle("Quién sigue a quién en el Europarlamento")
print(p)
ggsave(p, filename = paste0("fig27_matriz_adj_UE.png"), 
       height=16, width=16, units='cm')

# SIMILARITIES
# Compute similarity by group, party, country
plot_similarities(df.adj, df.info, "country")
plot_similarities(df.adj, df.info, "nationalPoliticalGroup")
plot_similarities(df.adj, df.info, "politicalGroup")
  