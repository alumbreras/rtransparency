plot_similarities <- function(df.adj, df.info, variable){
  
  df.adj <- df.adj %>% 
    mutate(from_ = df.info[,variable][match(df.adj$from, df.info$twitter)],
           to_   = df.info[,variable][match(df.adj$to, df.info$twitter)])
  
  df.totals <- df.adj %>% distinct(from, from_) %>% group_by(from_) %>% summarize(total = n()) %>% ungroup()
  df.counts <- df.adj %>% group_by(from_, to_) %>% summarize(count=n()) %>% ungroup()
  df.counts <- df.counts %>% 
    mutate(from.n = df.totals$total[match(df.counts$from_, 
                                          df.totals$from_)],
           to.n  =  df.totals$total[match(df.counts$to_,
                                          df.totals$from_)],
           total = from.n * to.n,
           count_perc = count / total)
  
  # Re_arrange by similarity
  M <- acast(df.counts, from_~to_, value.var="count_perc", fill=0, drop=FALSE)
  res <- heatmap(M)
  levels <- rownames(M)[res$rowInd]
  df.counts$from_ <- factor(df.counts$from_, levels = levels)
  df.counts$to_   <- factor(df.counts$to_, levels = levels)
  
  p <- ggplot2::ggplot(df.counts, aes(x=from_, y=to_)) + 
    geom_tile(aes(fill = count_perc)) + 
    geom_text(aes(label = round(count_perc, 1))) +
    scale_fill_gradient(low = "white", high = "steelblue", na.value = 'white') +
    scale_y_discrete(expand = c(0, 0)) +
    scale_x_discrete(expand = c(0, 0)) +
    theme_bw() +  
    xlab("") + ylab("") +   coord_fixed() +
    theme(
      rect = element_rect(fill = ggthemes::ggthemes_data$wsj$bg["brown"]),
      axis.text.y   = element_text(size=base_size*1, hjust = 1, colour = "black"),
      axis.text.x   = element_text(size=base_size*1, hjust = 1, colour = "black", angle = 90),
      axis.title.x  = element_text(color="black", size=8, face="italic"),
      axis.title.y  = element_text(color="black", size=8, face="italic"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      legend.position  = "none",
      plot.title    = element_text(size = 10, face = "bold",  hjust = 1),
      aspect.ratio=1)
  print(p)
}