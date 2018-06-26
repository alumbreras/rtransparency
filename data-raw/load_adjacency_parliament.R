# European Parliament
filename.adj <- 'adjacency-parliament-EU-out.csv'
filename.info <- 'MP-UE-info.csv'
df.adj <- read.csv(filename.adj)
df.info <- read.csv(filename.info)
EU_parliament_adj <- list(adjacencies = df.adj, info = df.info)
devtools::use_data(EU_parliament_adj, overwrite = TRUE)

# Spanish Parliament
filename.adj <- 'adjacency-parliament-ES-out.csv'
filename.info <- 'MP-ES-info.csv'
df.adj <- read.csv(filename.adj)
df.info <- read.csv(filename.info)
ES_parliament_adj <- list(adjacencies = df.adj, info = df.info)
devtools::use_data(ES_parliament_adj, overwrite = TRUE)

# Catalan Parliament
filename.adj <- 'adjacency-parliament-CAT-out.csv'
filename.info <- 'MP-CAT-info.csv'
df.adj <- read.csv(filename.adj)
df.info <- read.csv(filename.info)
CAT_parliament_adj <-list(adjacencies = df.adj, info = df.info)
devtools::use_data(CAT_parliament_adj, overwrite = TRUE)
  
# Badalona Parliament
filename.adj <- 'adjacency-parliament-BDN-out.csv'
filename.info <- 'MP-BDN-info.csv'
df.adj <- read.csv(filename.adj)
df.info <- read.csv(filename.info)
BDN_parliament_adj <-list(adjacencies = df.adj, info = df.info)
devtools::use_data(BDN_parliament_adj, overwrite = TRUE)



