library(ggplot2)
library(gridExtra)
library(ggbreak) 
library(stringr)
library(svglite)
################################################ CLUSTER SIZE DATA ################################################

setwd('~/Work/bin_project/data_test/')

files <- list.files(pattern = '\\cluster_size.tsv$', recursive = TRUE, include.dirs = TRUE)
cluster_size.df <- data.frame() #generacion de data frame vacio 
#loop a traves de los distintos archivos en files 
for (file in files) {
  species = str_split_fixed(file,'/',2)[,1] #obtener el nombre de la especie 
  bin_result = str_split_fixed(file,'/',5)[,3] #obtener el nombre del resultado
  experiment = str_split_fixed(file,'/',5)[,2]
  
  df <- read.delim(file) #lectura del archivo
  df$species = species # seteo del nombre de la especie dentro del dataframe
  df$bin_result = bin_result # seteo del nombre deresultado y parametro dentro del dataframe
  df$experiment = experiment 
  
  cluster_size.df <- rbind(cluster_size.df, df) #concatenacion de los dataframes 
  
}
files <- list.files(pattern = '\\cluster_size_SS.tsv$', recursive = TRUE, include.dirs = TRUE)
cluster_size_ss.df <- data.frame() #generacion de data frame vacio 
for (file in files) {
  species = str_split_fixed(file,'/',2)[,1] #obtener el nombre de la especie 
  bin_result = str_split_fixed(file,'/',5)[,3] #obtener el nombre del resultado
  experiment = str_split_fixed(file,'/',5)[,2]
  
  df <- read.delim(file) #lectura del archivo
  df$species = species # seteo del nombre de la especie dentro del dataframe
  df$bin_result = bin_result # seteo del nombre deresultado y parametro dentro del dataframe
  df$experiment = experiment 
  cluster_size_ss.df <- rbind(cluster_size_ss.df, df) #concatenacion de los dataframes 
}

#merging shortstack and binres data

names(cluster_size.df)[which(names(cluster_size.df) == 'lenght')] <- 'length'

cluster_size.df <- cluster_size.df[, names(cluster_size_ss.df)]
cluster_size.df <- rbind(cluster_size.df, cluster_size_ss.df)

################################################ CLUSTER DENSITY DATA ################################################

files <- list.files(pattern = '\\cluster_density.tsv$', recursive = TRUE, include.dirs = TRUE)
cluster_density.df <- data.frame() #generacion de data frame vacio 
#loop a traves de los distintos archivos en files 
for (file in files) {
  species = str_split_fixed(file,'/',2)[,1] #obtener el nombre de la especie 
  bin_result = str_split_fixed(file,'/',5)[,3] #obtener el nombre del resultado
  experiment = str_split_fixed(file,'/',5)[,2]
  
  df <- read.delim(file) #lectura del archivo
  df$species = species # seteo del nombre de la especie dentro del dataframe
  df$bin_result = bin_result # seteo del nombre deresultado y parametro dentro del dataframe
  df$experiment = experiment 
  cluster_density.df <- rbind(cluster_density.df, df) #concatenacion de los dataframes 
  
}

################################################ CLUSTER NUMBER DATA ################################################

files <- list.files(pattern = '\\cluster_num.tsv$', recursive = TRUE, include.dirs = TRUE)
cluster_num.df <- data.frame() #generacion de data frame vacio 
#loop a traves de los distintos archivos en files 
for (file in files) {
  species = str_split_fixed(file,'/',2)[,1] #obtener el nombre de la especie 
  bin_result = str_split_fixed(file,'/',5)[,3] #obtener el nombre del resultado
  experiment = str_split_fixed(file,'/',5)[,2]
  
  df <- read.delim(file) #lectura del archivo
  df$species = species # seteo del nombre de la especie dentro del dataframe
  df$bin_result = bin_result # seteo del nombre deresultado y parametro dentro del dataframe
  df$experiment = experiment 
  cluster_num.df <- rbind(cluster_num.df, df) #concatenacion de los dataframes 
  
}

dim(cluster_num.df)
dim(cluster_num_ss.df)
files <- list.files(pattern = '\\cluster_num_SS.tsv$', recursive = TRUE, include.dirs = TRUE)
cluster_num_ss.df <- data.frame() #generacion de data frame vacio 
for (file in files) {
  species = str_split_fixed(file,'/',2)[,1] #obtener el nombre de la especie 
  bin_result = str_split_fixed(file,'/',5)[,3] #obtener el nombre del resultado
  experiment = str_split_fixed(file,'/',5)[,2]
  
  df <- read.delim(file) #lectura del archivo
  df$species = species # seteo del nombre de la especie dentro del dataframe
  df$bin_result = bin_result # seteo del nombre deresultado y parametro dentro del dataframe
  df$experiment = experiment 
  
  cluster_num_ss.df <- rbind(cluster_num_ss.df, df) #concatenacion de los dataframes 
}

cluster_num <- rbind(cluster_num.df, cluster_num_ss.df)
################################################ CLUSTER NUMBER PLOTS ################################################
for (specie in unique(cluster_num.df$species)) { #loop atraves de las distintas especies

  b = 'bin_result_5' #seleccion de resultados 
  
  #result =  paste(specie,b, sep = '_')
  #png(paste0(result, '.png') , 480,480)
  df <- cluster_size.df[cluster_size.df$specie == specie & cluster_size.df$bin_result == b,]
  tab <- table(df$chr, df$experiment) #obtencion del numero de clusters 
  tab <- t(tab)
  
  num_colors = length(unique(df$experiment))
  colors = colors <- hcl.colors(num_colors, 'Geyser')
  names(colors) <-  unique(df$experiment)
  
  par(mar=c(10,6,2,2))
  barplot(tab, las=2, beside=T, col=colors,
          cex.name=.8, main=specie)
  
  legend("topright", inset = c(0, 0),  names(colors), col=colors, pch=15, cex=0.7)
  #dev.off()
}

################ Stripchart numbre of clusters ##########################3
for (specie in unique(cluster_num$species)) {
  
  f1 = cluster_num$species == specie
  
  df <- cluster_num[f1, ]
  f2 = which(df$X == 'total_clusters')
  df <- df[-f2,]
  file_name = paste(specie, 'number_clusters.svg', sep = '_')
  svglite(filename = file_name, width = 15, height = 12)
  par(mar=c(10,4,4,2))
  #boxplot(df$number ~ df$bin_result + df$X, las=3, xlab='')

  num_colors = length(unique(df$bin_result))
  colors = colors <- hcl.colors(num_colors, 'Geyser')
  names(colors) <-  unique(df$bin_result)
  
  
  stripchart(df$number ~ df$bin_result + df$X, main=paste('Number of clusters', specie), las=3, xlab='', cex.axis = 0.6, cex=1, 
             ylab="Number of clusters",
             pch=19, vertical=T,
             col = scales::alpha(colors, 0.5))
  total = length((df$X)) + 1
  abline(v=seq(1,total ,5)-0.5, lty=3)

  legend("topright", inset = c(0, 0),  names(colors), col=colors, pch=19, cex=0.8)
  dev.off()
}

############################################### CLUSTER CUMULATIVE SIZE PLOTS ###############################################
###largo de clusters por resultados y por especie ###
colors <- hcl.colors(4, 'Geyser') #obtener colores determinados de una paleta
names(colors) <-  unique(cluster_size.df$species) #darle nombre a los colores seleccionados en colors de acuerdo a la especie

b = 'bin_result_01'
par(mar=c(8,6,4,2), xpd=F)
plot(1,1, type='n', xlim=c(0,500),ylim=c(0,1), #plot vacio
     xlab='Cluster length (nucleotides)',
     ylab="Cumulative proportion",
     main=b)

for (species in unique(cluster_size.df$species)) { #loop atraves de las distintas especies
  
   #seleccion de resultados 
  df <- cluster_size.df[cluster_size.df$species == species &
                          cluster_size.df$bin_result == b,] #se genera un dataframe especifico para cada especie 
  
  cdf <- ecdf(df$length) #funcion de densidad acumulativa del largo de los bins
  color <- colors[species] #seleccion del color de acuerdo a la especie
  plot(cdf, add=T, verticals = T, do.points=F, col=color, lwd=2) #plot de la funcion 
  
}
legend("bottomright", names(colors), col=colors, pch=19, inset=0.05) #leyenda para mostrar de que color es cada especie  

graphics.off()
 
#########################################################################################################################
############################################### CLUSTER CUMULATIVE LENGHT PLOT ##########################################
###4 PLOTS OF CLUSTER LENGHT BY SPECIE AND WORKFLOW

svglite('Lenght_cluster.svg',  width = 9.14,
        height = 8.42, )

layout(matrix(c(1, 2,
                3, 4,
                5, 5), 
              ncol = 2, nrow=3, byrow=TRUE), heights=c(4, 4))

for (specie in unique(cluster_size.df$species)) { 
  par(mar=c(6,4,4,2), xpd=F)
  plot(1,1, type='n', xlim=c(0,500),ylim=c(0,1), #plot vacio
       xlab='Cluster length (nucleotides)',
       ylab="Cumulative proportion",
       main=specie)
  
  df_sp <- cluster_size.df[cluster_size.df$species == specie,] #se genera un dataframe especifico para cada especie 
  
  num_colors = length(unique(df_sp$bin_result))
  colors <- hcl.colors(num_colors, 'Geyser')
  names(colors) <-  unique(df_sp$bin_result)
  
  for (b in unique(df_sp$bin_result)){
    
    df_b <- df_sp[df_sp$bin_result == b,]
    
    for (e in unique(df_b$experiment)) {
      
      df_e <- df_b[df_b$experiment == e,]
      
      cdf <- ecdf(df_e$length) #funcion de densidad acumulativa del largo de los bins
      color <- colors[b] #seleccion del color de acuerdo a la especie
      
      plot(cdf, add=T, verticals = T, do.points=F, col=color, lwd=2) #plot de la funcion 
    
    }
  }
}
par(mai=c(0,0,0,0))
plot.new()
legend("center", ncol=5, names(colors), col=colors, pch=19, xpd=TRUE)

dev.off()
graphics.off()
 
###4 PLOTS OF DISTANCE TO NEXT CLUSTER BY SPECIE AND WORKFLOW

svglite('Distance_next_cluster.svg',  width = 9.14,
        height = 8.42, )

layout(matrix(c(1, 2,
                3, 4,
                5, 5), 
              ncol = 2, nrow=3, byrow=TRUE), heights=c(4, 4))

for (specie in unique(cluster_size.df$species)) { 
  par(mar=c(6,4,4,2), xpd=F)
  plot(1,1, type='n', xlim=c(0,500),ylim=c(0,1), #plot vacio
       xlab='Distance to next cluster (nucleotides)',
       ylab="Cumulative proportion",
       main=specie)
  
  df_sp <- cluster_size.df[cluster_size.df$species == specie,] #se genera un dataframe especifico para cada especie 
  
  num_colors = length(unique(df_sp$bin_result))
  colors <- hcl.colors(num_colors, 'Geyser')
  names(colors) <-  unique(df_sp$bin_result)
  
  for (b in unique(df_sp$bin_result)){
    
    df_b <- df_sp[df_sp$bin_result == b,]
    
    for (e in unique(df_b$experiment)) {
      
      df_e <- df_b[df_b$experiment == e,]
      
      cdf <- ecdf(df_e$distance_next_cluster) #funcion de densidad acumulativa del largo de los bins
      color <- colors[b] #seleccion del color de acuerdo a la especie
      
      plot(cdf, add=T, verticals = T, do.points=F, col=color, lwd=2) #plot de la funcion 
      
    }
  }
}

par(mai=c(0,0,0,0))
plot.new()
legend("center", ncol=5, names(colors), col=colors, pch=19, xpd=TRUE)

dev.off()
