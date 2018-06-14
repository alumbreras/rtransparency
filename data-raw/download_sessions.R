# Download sessions from url like
# https://app.congreso.es/votacionesWeb/OpenData?sesion=119&completa=1&legislatura=12
# and create a single csv file for all the calls.
# Create also its .rda file

library(httr)
library(XML)
library(dplyr)

# Download data from the 10th,11th,12th congresses
for(nlegis in 10:12){
  cat("\n downloading legislatura", nlegis)
  dir <- paste0('./ESvotes/legislatura',nlegis)
  dir.create(dir, showWarnings = TRUE, recursive = TRUE)
  for(nsession in 1:300){
    file <- paste0("legislatura", nlegis, "sesion", nsession, ".zip")
    GET("https://app.congreso.es/votacionesWeb/OpenData",
        query = list(sesion = nsession, completa = 1, legislatura = nlegis), 
        write_disk(file.path(dir, file), overwrite=TRUE))
    
    # Unzip session
    unzip(file.path(dir, file), exdir = dir)
  }
}

# Create dataframes and then a csv file
df.calls <- data.frame()
res <- list()
idrow <- 1
for(nlegis in 10:12){
  cat("\n processing legislatura", nlegis)
  dir <- paste0('./ESvotes/legislatura',nlegis)
  
  # Process each unzipped session-call and put it in the dataframe
  files <- list.files(dir, pattern = "\\.xml$", full.names = TRUE)
  for(f in 1:length(files)){
    cat("\nfile: ", files[f], f, "/", length(files))
    data.xml <- xmlParse(files[f])
    data <- xmlToList(data.xml)
    
    sesion    <- iconv(data$Informacion$Sesion, to = 'UTF-8')
    votacion  <- data$Informacion$NumeroVotacion
    fecha     <- as.Date(data$Informacion$Fecha, format = "%d/%m/%Y")
    titulo    <- iconv(data$Informacion$Titulo, to = 'UTF-8')
    if(length(data$Votaciones) < 10) {
      next
    }
    for(i in 1:length(data$Votaciones)){
      diputado  <- iconv(data$Votaciones[[i]]$Diputado, to = 'UTF-8') 
      grupo     <- iconv(data$Votaciones[[i]]$Grupo, to = 'UTF-8') 
      voto      <- iconv(data$Votaciones[[i]]$Voto, to = 'UTF-8') 
      if(identical(grupo, character(0))) { 
        grupo <- NA
      }
      res[[idrow]] <- list(legislatura = nlegis,
                           sesion = sesion, 
                           votacion = votacion, 
                           fecha = fecha, 
                           diputado = diputado,
                           grupo = grupo, 
                           voto = voto)
      idrow <- idrow + 1
    }
  }
}

df.calls <- bind_rows(res)
df.calls$legislatura <- 12
df.calls$legislatura[df.calls$fecha <= as.Date("28/04/2016", format = "%d/%m/%Y")] <- 11
df.calls$legislatura[df.calls$fecha <= as.Date("20/10/2015", format = "%d/%m/%Y")] <- 10

df.calls$legislatura[df.calls$fecha <= as.Date("3/5/2019", format = "%d/%m/%Y")] <- 12
df.calls$legislatura[df.calls$fecha <= as.Date("3/5/2016", format = "%d/%m/%Y")] <- 11
df.calls$legislatura[df.calls$fecha <= as.Date("27/10/2015", format = "%d/%m/%Y")] <- 10
df.calls$legislatura[df.calls$fecha <= as.Date("27/9/2011", format = "%d/%m/%Y")] <- 9



df.calls$legislatura <- as.integer(df.calls$legislatura)
#df.calls$voto <- as.factor(df.calls$voto)
df.calls$sesion <- as.integer(df.calls$sesion)
df.calls$votacion <- as.integer(df.calls$votacion)
df.calls <- arrange(df.calls, fecha, sesion, votacion, grupo, diputado)
df.calls <- select(df.calls, 
                   legislatura, fecha, sesion, votacion, diputado, grupo, voto)

# Sometimes, group is missing in the original data. Fix it. Also,
# take CiU and ERC members out from the Grupo Mixto

ERC <- c("Jordà i Roura, Teresa",
         "Tardà i Coma, Joan",
         "Bosch i Pascual, Alfred")

CiU <- c("Nogueras i Camero, Míriam",
         "Homs Molist, Francesc",
         "Bel Accensi, Ferran",
         "Miquel i Valentí, Sergi",
         "Postius Terrado, Antoni")

df.calls$grupo[df.calls$diputado == "Ramos Jordán, Alicia"] <- "GCUP-EC-EM"
df.calls$grupo[df.calls$diputado %in% ERC] <- "GER"
df.calls$grupo[df.calls$diputado %in% CiU] <- "GC-CiU"


for(nleg in 10:12){
  df <- df.calls %>% filter(legislatura==nleg, is.na(grupo))
  members <- unique(df$diputado)
  for(m in members){
    grupo <- df.calls %>% filter(legislatura==nleg, 
                                 diputado == m, 
                                 !is.na(grupo)) %>% 
                          select(grupo)
    grupo <- as.character(grupo[1,])
    df.calls$grupo[df.calls$diputado == m] <- grupo
  }
}

write.csv(df.calls, file = "ESvotes_calls.csv", row.names = FALSE)
ESvotes_calls <-  df.calls
devtools::use_data(ESvotes_calls, overwrite = TRUE)
###########################


df.diputados <- df.calls %>% select(diputado, grupo) %>% arrange(grupo, diputado)
df.calls <- df.calls %>% select(diputado, grupo) %>% arrange(grupo, diputado)
df.calls$diputado <- factor(df.calls$diputado, levels = unique(df.diputados$diputado))
df.calls <- df.calls %>% arrange(sesion, votacion)
df.calls$idvotacion <- group_indices(df.calls, sesion, votacion)

parlamentvotes_ES_full <- df.calls
devtools::use_data(parlamentvotes_ES_full)