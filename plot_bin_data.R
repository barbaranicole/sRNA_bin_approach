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
  
  df <- read.delim(file) #lectura del archivo
  df$species = species # seteo del nombre de la especie dentro del dataframe
  df$bin_result = bin_result # seteo del nombre deresultado y parametro dentro del dataframe
  
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

plot_num <-ggplot(data=cluster_num, aes(x=X, y=number)) + geom_bar(stat="identity", fill="steelblue")+ 
  geom_text(aes(label=number),hjust = -0.5, size=3.5)+ theme_minimal()
plot_num <- plot_num + coord_flip() + ggtitle('Number of clusters')
plot_num


#plot(1,1, type='n')
for (species in unique(cluster_num.df$species)) { #loop atraves de las distintas especies
  
  b = 'bin_results' #seleccion de resultados 
  df <- cluster_num.df[cluster_num.df$species == species &
                          cluster_num.df$bin_result == b,] #se genera un dataframe especifico para cada especie 
  num_colors = length(unique(df$X))
  colors = colors <- hcl.colors(num_colors, 'Geyser')
  names(colors) <-  unique(df$X)
  print(colors)
  bar <- barplot(df$number, main=species,  beside = TRUE,  names.arg = c(df$X), col = colors)
  legend("topright", inset = c(-0.2, 0),  names(colors), col=colors, pch=15)
  
}

for (bin_result in unique(cluster_num.df$bin_result)) { #loop atraves de las distintas especies
  
    #b = 'bin_results' #seleccion de resultados 
  species= 'Pyricularia_oryzae'
  df <- cluster_num.df[cluster_num.df$species == species &
                         cluster_num.df$bin_result == bin_result,] #se genera un dataframe especifico para cada especie 
  num_colors = length(unique(df$X))
  colors = colors <- hcl.colors(num_colors, 'Geyser')
  names(colors) <-  unique(df$X)
  print(colors)
  bar <- barplot(df$number, main=paste(species, bin_result),  beside = TRUE,  names.arg = c(df$X), col = colors, las=2, cex.names=0.7)
  legend("topright", inset = c(-0.1, 0),  names(colors), col=colors, pch=15, cex=0.6)
  
  unique(df$X)
}  


#set colors
hcl.pals('qualitative') #paletas que se utilizan de forma cuantitativa
colors <- hcl.colors(4, 'Dynamic') #obtener colores determinados de una paleta
names(colors) <-  unique(cluster_size.df$species) #darle nombre a los colores seleccionados en colors de acuerdo a la especie

#funcion para generar un plot de distintas especies
plot_cdf <- function(save=F) {
  
  if (save) {png("bin_results.png", 480,480)} #generacion de la imagen para guardar el plot 
  
  plot(1,1, type='n', xlim=c(0,500), ylim=c(0,1), #plot vacio
       xlab='Cluster length (nucleotides)',
       ylab="Cumulative proportion",
       main="Bin_results")
  
  for (species in unique(cluster_size.df$species)) { #loop atraves de las distintas especies
    
    b = 'bin_results' #seleccion de resultados 
    df <- cluster_size.df[cluster_size.df$species == species &
                            cluster_size.df$bin_result == b,] #se genera un dataframe especifico para cada especie 
    cdf <- ecdf(df$lenght) #funcion de densidad acumulativa del largo de los bins
    color <- colors[species] #seleccion del color de acuerdo a la especie
    plot(cdf, add=T, verticals = T, do.points=F, col=color, lwd=2) #plot de la funcion 
    
  }
  
  legend("bottomright", names(colors), col=colors, pch=19, inset=0.05) #leyenda para mostrar de que color es cada especie  
  
  if (save) {dev.off()} #si save es T entonces, cerrara la ventana del png para guardar el plot 
  
}

plot_cdf()

