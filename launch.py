import pysam
import collections
from math import floor, ceil
from pprint import pprint
import functions 
import pandas as pd
import argparse
import os

parser = argparse.ArgumentParser(description='Detection and analysis of small RNA seq through a bin approach')
parser.add_argument('--bam', help='Bam file already trimmed for be analyzed', type = str ,required = True)
parser.add_argument('--bin_size', help='Size of the bins, default: 50pb', type = int, default=50, required=False)
parser.add_argument('--rpm_t', '-gt', help='Treshold for aggrouping the bins by rpm, default: 0.5 rpm', default=0.5, required=False)
parser.add_argument('--bin_t', '-ct', help='Treshold for clustering the bins by distnaces in bins, default: 2 bins', default=2, required=False)

args = vars(parser.parse_args())

path = './bin_results'
os.mkdir(path)

#lectura del archivo bam 
bam_file = args['bam']
bam_file = pysam.AlignmentFile(bam_file,'rb')
#header - contiene informacion sobre el alineamiento y los cromosomas
header = bam_file.header.to_dict()
total_reads = bam_file.mapped

#obtencion de reads e informacion por cromosoma
chromosomes = {}
chromosomes_lenght = {} #for calculated density
genome_size = 0 #for calculated density
bin_pos = {}

for i in header['SQ']:
    chromosome_name = i['SN']
    chromosome_lenght = i['LN']     #i['LN'] es el largo
    genome_size += chromosome_lenght

    reads = bam_file.fetch(i['SN'])
    
     #lista de posiciones de los reads
    pos = []
    for read in reads:
        p = (read.to_dict())['ref_pos']
        pos.append(p)

    #obtencion de bins 
    bins = [functions.pos_to_bin(p) for p in pos]
    o_bins = [functions.pos_to_bin(p, overlap=True) for p in pos]
    c = dict(collections.Counter(bins + o_bins))

    bin_pos[chromosome_name] = [pos]
    
    chromosomes_lenght[chromosome_name] = chromosome_lenght
    chromosomes[chromosome_name] = dict(sorted(c.items()))

bam_file.close()

for k,v in chromosomes.items():

    dict_aux = {}
    bins = list(v.keys())
    counts = list(v.values())
    
    for i in range(len(v)):
        dict_aux[bins[i]] = functions.bin_to_RPM(counts[i], total_reads) 
        #print(bins[i], counts[i])
    
    chromosomes[k] = dict_aux


rpm_t = args['rpm_t']
bin_t = args['bin_t']

chromosomes_bin = {}
for chr_name, values in chromosomes.items():

  bins = functions.rpm_treshold(values, treshold= rpm_t)
  if len(bins) != 0 :
    chromosomes_bin[chr_name] = functions.clustering_bins(bins, treshold = bin_t)

cluster_density,num_clusters = functions.summary_bins(chromosomes_bin, chromosomes_lenght, genome_size)

df_cluster_density = pd.DataFrame.from_dict(cluster_density, orient='index', columns= ['density'])
df_cluster_density.to_csv(path + "cluster_density.tsv", sep='\t')

df_cluster_num = pd.DataFrame.from_dict(num_clusters, orient='index', columns= ['number'])
df_cluster_num.to_csv(path + "cluster_num.tsv", sep='\t')

cluster_size = functions.cluster_size(chromosomes_bin)

list_aux = []
cluster_size_df = {}
for index, clusters in cluster_size.items():
    df = pd.DataFrame(clusters)
    df['chr'] = index
    df = df.set_index('chr')
    list_aux.append(df)
    
cluster_size_df = pd.concat(list_aux)

cluster_size_df.to_csv(path + "cluster_size.tsv", sep='\t')

print(args)