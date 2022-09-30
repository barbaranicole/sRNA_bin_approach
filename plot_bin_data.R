library(ggplot2)
library(gridExtra)
library(ggbreak) 
library(stringr)

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


################################################ CLUSTER NUMBER PLOTS ################################################
for (specie in unique(cluster_num.df$species)) { #loop atraves de las distintas especies
  
  print(specie)
  b = 'bin_result_5' #seleccion de resultados 
  
  
  #result =  paste(specie,b, sep = '_')
  png(paste0(result, '.png') , 480,480)
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


for (bin_result in unique(cluster_num.df$bin_result)) { #loop atraves de las distintas especies
  
  species= 'Arabidopsis_thaliana'
  df <- cluster_size.df[cluster_size.df$species == species &
                          cluster_size.df$bin_result == bin_result,] #se genera un dataframe especifico para cada especie 
  
  tab <- table(df$chr, df$experiment) #obtencion del numero de clusters 
  tab <- t(tab)
  tab
  num_colors = length(unique(df$experiment))
  colors = colors <- hcl.colors(num_colors, 'Geyser')
  names(colors) <-  unique(df$experiment)
  
  par(mar=c(8,4,4,10), xpd=T)
  barplot(tab, las=2, beside=T, col=colors,
          cex.name=.8, main=paste(species, bin_result))
  
  legend("topright", inset = c(-0.3, 0),  names(colors), col=colors, pch=15, cex=0.8)
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
  
  cdf <- ecdf(df$lenght) #funcion de densidad acumulativa del largo de los bins
  color <- colors[species] #seleccion del color de acuerdo a la especie
  plot(cdf, add=T, verticals = T, do.points=F, col=color, lwd=2) #plot de la funcion 
  
}
legend("bottomright", names(colors), col=colors, pch=19, inset=0.05) #leyenda para mostrar de que color es cada especie  


###largo de clusters por especie y por resultado ###
specie = 'Arabidopsis_thaliana'
par(mar=c(8,6,4,2), xpd=F)
plot(1,1, type='n', xlim=c(0,500),ylim=c(0,1), #plot vacio
     xlab='Cluster length (nucleotides)',
     ylab="Cumulative proportion",
     main= specie)

colors <- hcl.colors(4, 'Geyser') #obtener colores determinados de una paleta
names(colors) <-  unique(cluster_size.df$bin_result) 

for (bin_result in unique(cluster_size.df$bin_result)) { #loop atraves de las distintas especies
  df <- cluster_size.df[cluster_size.df$species == specie &
                          cluster_size.df$bin_result == bin_result,] #se genera un dataframe especifico para cada especie 
  cdf <- ecdf(df$lenght) #funcion de densidad acumulativa del largo de los bins
  color <- colors[bin_result] #seleccion del color de acuerdo a la especie
  plot(cdf, add=T, verticals = T, do.points=F, col=color, lwd=2) #plot de la funcion 
}
legend("bottomright", names(colors), col=colors, pch=19, inset=0.05) #leyenda para mostrar de que color es cada especie  


### Distance to next cluster by specie and rpm ###
specie = 'Botrytis_cinerea'

par(mar=c(8,6,4,2), xpd=F)
plot(1,1, type='n', xlim=c(0,6000),ylim=c(0,1), #plot vacio
     xlab='Distance to next cluster (nucleotides)',
     ylab="Cumulative proportion",
     main= specie)
colors <- hcl.colors(4, 'Geyser') #obtener colores determinados de una paleta
names(colors) <-  unique(cluster_size.df$bin_result) 

for (bin_result in unique(cluster_size.df$bin_result)) { #loop atraves de las distintas especies

  df <- cluster_size.df[cluster_size.df$species == specie &
                          cluster_size.df$bin_result == bin_result,] #se genera un dataframe especifico para cada especie 
  cdf <- ecdf(df$distance_next_cluster) #funcion de densidad acumulativa del largo de los bins
  color <- colors[bin_result] #seleccion del color de acuerdo a la especie
  plot(cdf, add=T, verticals = T, do.points=F, col=color, lwd=2) #plot de la funcion 
  
}
legend("bottomright", names(colors), col=colors, pch=19, inset=0.05) #leyenda para mostrar de que color es cada especie  


specie = 'Botrytis_cinerea'
bin_result = 'bin_result_01'
par(mar=c(8,6,4,2), xpd=F)
plot(1,1, type='n', xlim=c(0,6000),ylim=c(0,1), #plot vacio
     xlab='Distance to next cluster (nucleotides)',
     ylab="Cumulative proportion",
     main= specie)
colors <- hcl.colors(3, 'Geyser') #obtener colores determinados de una paleta
names(colors) <-  unique(cluster_size.df[cluster_size.df$species == specie,]$experiment)

for (experiment in unique(cluster_size.df[cluster_size.df$species == specie,]$experiment)) { #loop atraves de los distintos exp

  df <- cluster_size.df[cluster_size.df$species == specie &
                          cluster_size.df$bin_result == bin_result & 
                          cluster_size.df$experiment == experiment ,] #se genera un dataframe especifico para cada especie 
  cdf <- ecdf(df$distance_next_cluster) #funcion de densidad acumulativa del largo de los bins
  color <- colors[experiment] #seleccion del color de acuerdo a la especie
  plot(cdf, add=T, verticals = T, do.points=F, col=color, lwd=2) #plot de la funcion 
  
}
legend("bottomright", names(colors), col=colors, pch=19, inset=0.05) #leyenda para mostrar de que color es cada especie  
 
