import pysam
import collections
from math import floor
from pprint import pprint
import sys


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

    return(bin)

#calcula los RPM de cada bin 
def bin_to_RPM(bin, total_reads):
    return (bin/total_reads)*1000000


# agrupar bins de forma consecutiva y que pasen un trshold determinado 
def bin_group(chromosome, treshold = 0.5):
    consecutive_bins = [[]] #iniciar la generacion de listas
    
    for bin, rpm in enumerate(chromosome): # lista de posiciones (bin) y valores (rpm)
        
        if rpm > treshold: #si el rpm es mayor al treshold
            consecutive_bins[-1].append(bin) # agrega el valor a la lista anterior para generar la susecion de numeros
        elif len(consecutive_bins[-1]) != 0: #si un valor no cumple la condicion y ademas la lista anterior tiene valores dentro genera una nueva lista
            consecutive_bins.append([])

    if len(consecutive_bins[-1]) == 0:
         consecutive_bins.remove([])
    return consecutive_bins


#clusteriza los bins de acuerdo a su distancia con otros clusters secuenciales
def clustering_bins(bins, treshold = 2):

    clusters = [bins.pop(0)] #extrae la primera lista de clusters
    for cluster in bins:

        if abs(clusters[-1][-1] - cluster[0]) <= treshold: #revisa si la distancia entre los ultimos y los primeros valores de cada grupo de bins 
            clusters[-1] += cluster 
        else:
            clusters.append(cluster) 

    return clusters