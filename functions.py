
from math import floor, ceil

#funcion para pasar las posiciones a bins 
def pos_to_bin(p, b=50, overlap=False):

    if overlap:
        p = float(p) - b/2
    else:
        p = float(p)
    bin = floor(p/b)
    #set de nombres de los bins como secuencias sucesivas (1,2,3,4,5 ... N) impares up - pares overlap
    if overlap:
        bin = bin * 2 + 1        
    else:
        bin = bin * 2
    bin += 1
    return(bin)

#calcula los RPM de cada bin 
def bin_to_RPM(bin, total_reads):
    return (bin/total_reads)*1000000


# agrupar bins de forma consecutiva y que pasen un trshold determinado 
def rpm_treshold(chromosome, treshold = 0.5):
    
    rpm_values = list(chromosome.values())
    bin_values = list(chromosome.keys())

    consecutive_bins = [[]] #iniciar la generacion de listas
    
    for bin, rpm in enumerate(rpm_values): # lista de posiciones (bin) y valores (rpm)
        
        if rpm > treshold: #si el rpm es mayor al treshold
            consecutive_bins[-1].append(bin_values[bin]) # agrega el valor a la lista anterior para generar la susecion de numeros
        elif len(consecutive_bins[-1]) != 0: #si un valor no cumple la condicion y ademas la lista anterior tiene valores dentro genera una nueva lista
            consecutive_bins.append([])

    if len(consecutive_bins[-1]) == 0:
         consecutive_bins.remove([])
    
    return consecutive_bins
 
 
#clusteriza los bins de acuerdo a su distancia con otros clusters secuenciales
def clustering_bins(bins, treshold = 2):

    if len(bins) >= 1:
        clusters = [bins.pop(0)] #extrae la primera lista de clusters
        
        for cluster in bins:

            if abs(clusters[-1][-1] - cluster[0]) <= treshold: #revisa si la distancia entre los ultimos y los primeros valores de cada grupo de bins 
                clusters[-1] += cluster 
            else:
                clusters.append(cluster) 

    else:
        clusters = []
        
    return clusters


def summary_bins(bins_dict = dict, lenghts = 'dict', genome_size = int):
        #number of clusters
    cluster_density = {}
    num_clusters = {}
    
    total_clusters = 0
    for k,v in bins_dict.items():
    
        num_cluster = len(v)
        total_clusters += num_cluster    #obtencion de bins 
    
        cluster_density_chr = num_cluster/lenghts[k]
    
        num_clusters[k] = num_cluster
        cluster_density[k] = cluster_density_chr
        
    num_clusters['total_clusters'] = total_clusters
    cluster_density['cluster density global'] = total_clusters/genome_size
    
    return(cluster_density, num_clusters)


def cluster_size(bins_dict = dict):

    cluster_size = {}

    for k,v in bins_dict.items():
        
        clusters = []
        for n in range(len(v)):
            l = (v[n][-1] - v[n][0])

            try:

                if ((l+1)%2) == 0:
                    start = int((v[n][0]/2)*50)
                    stop = int(((v[n][-1]/2)*50)+50)
                    inter_space = int((v[n+1][0]/2)*50) - stop
                    cluster_data = {'cluster':[v[n][0], v[n][-1]], 'start': start, 'stop': stop, 'lenght': (l+2)*25, 'size' : (l+1), 'distance_next_cluster': inter_space}
                    clusters.append(cluster_data)        

                else:

                    start = int((v[n][0]/2)*50)
                    stop = int(((v[n][-1]/2)*50)+50)
                    inter_space = int((v[n+1][0]/2)*50) - stop
                    cluster_data = {'cluster':[v[n][0], v[n][-1]], 'start': start, 'stop': stop, 'lenght': (ceil((l+1)/2))*50, 'size' : (l+1), 'distance_next_cluster': inter_space}
                    clusters.append(cluster_data)

            except IndexError:

                if ((l+1)%2) == 0:
                    start = int((v[n][0]/2)*50)
                    stop = int(((v[n][-1]/2)*50)+50)
                    inter_space = 0
                    cluster_data = {'cluster':[v[n][0], v[n][-1]], 'start': start, 'stop': stop, 'lenght': (l+2)*25, 'size' : (l+1), 'distance_next_cluster': inter_space}
                    clusters.append(cluster_data)        

                else:

                    start = int((v[n][0]/2)*50)
                    stop = int(((v[n][-1]/2)*50)+50)
                    inter_space = 0
                    cluster_data = {'cluster':[v[n][0], v[n][-1]], 'start': start, 'stop': stop, 'lenght': (ceil((l+1)/2))*50, 'size' : (l+1), 'distance_next_cluster': inter_space}
                    clusters.append(cluster_data)

        cluster_size[k] = clusters
    
    return(cluster_size)