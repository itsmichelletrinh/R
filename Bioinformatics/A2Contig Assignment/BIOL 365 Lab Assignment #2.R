# BIOL 365 Assign2 Gene Annotation using DECIPHER
# Michelle Trinh
# This assignment uses DECIPHER and dependent R packages
R.version.string
date()

# Task 1
#Loading A2contig FASTA file from local copy to R 
A2contig <- readDNAStringSet("C:/Users/miche/OneDrive/Documents/University of Waterloo/Fifth Year 2024-2025/BIOL 365/A2contig.fasta", format="fasta")

#Confirming data has loaded
A2contig

#Retrieving name of data: "JQGU01000058.1 Leptospira alexanderi strain 56643 Contig058, whole genome shotgun sequence"
names(A2contig)


# Task 2
#Finding genes using FindGenes
orfs <- FindGenes(A2contig, showPlot=TRUE, allScores=TRUE)
orfs

#Subset ORFs that are predicted as genes
genes <- orfs[orfs[, "Gene"]==1,]

#Extract genes using ExtractGenes
dna <-  ExtractGenes(genes, A2contig)
dna

#Determine shortest and longest genes
w <- which(width(dna) < 200)
dna[w]
w <- which(width(dna) > 2000)
dna[w]

#Determine start site locations and orientations for first three genes
w <- which(width(dna) == 1164)
genes[w]
w <- which(width(dna) == 2550)
genes[w]
w <- which(width(dna) == 792)
genes[w]

#Translate genes to obtain predicted protein sequences
aa <- ExtractGenes(genes, A2contig, type="AAStringSet")
aa
write.csv(aa)


#Task 3
#Obtain pre-built models of non-coding RNAs commonly found in bacteria
data(NonCodingRNA_Bacteria)
x <- NonCodingRNA_Bacteria

#Search for these models in A2contig
rnas <- FindNonCoding(x, A2contig)
rnas

#Match Gene value with corresponding model 
annotations <- attr(rnas, "annotations")
m <- match(rnas[, "Gene"], annotations)
sort(table(names(annotations)[m]))

#Determine sequence of non-coding gene
gene_sequence <- subseq(A2contig, start = 26511, end = 26583)
write.csv(gene_sequence)

#Determine overlap between coding and non-coding genes
genes[Begin, End]

