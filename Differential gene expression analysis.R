#Name: Aryan Boruah
#Project: Differential gene expression analysis 

library(dplyr)
library(Seurat)
library(patchwork)
library(ggplot2)


# Load the PBMC dataset
pbmc.data <- Read10X(data.dir = "D:/pbmc3k_filtered_gene_bc_matrices/filtered_gene_bc_matrices/hg19")

# Initialize the Seurat object with the raw (non-normalized data).
pbmc <- CreateSeuratObject(counts = pbmc.data, project = "pbmc3k", min.cells = 3, min.features = 200)
pbmc




# The [[ operator can add columns to object metadata. This is a great place to stash QC stats
pbmc[["percent.mt"]] <- PercentageFeatureSet(pbmc, pattern = "^MT-")

# Visualize QC metrics as a violin plot
VlnPlot(pbmc, features = c("nFeature_RNA", "nCount_RNA", "percent.mt"), ncol = 3)




# FeatureScatter is typically used to visualize feature-feature relationships, but can be used
# for anything calculated by the object, i.e. columns in object metadata, PC scores etc.

plot1 <- FeatureScatter(pbmc, feature1 = "nCount_RNA", feature2 = "percent.mt")
plot2 <- FeatureScatter(pbmc, feature1 = "nCount_RNA", feature2 = "nFeature_RNA")
plot1 + plot2

#VST function calculates a variance stabilizing transformation (VST) from 
#the fitted dispersion-mean relation(s) and then transforms the count data 
#(normalized by division by the size factors or normalization factors), 
#yielding a matrix of values which are now approximately homoskedastic 
#(having constant variance along the range of mean values). 
#The transformation also normalizes with respect to library size. 
#The rlog is less sensitive to size factors, which can be an issue when 
#size factors vary widely. These transformations are useful when checking 
#for outliers or as input for machine learning techniques such as clustering 
#or linear discriminant analysis.

#data normalization
pbmc <- NormalizeData(pbmc, normalization.method = "LogNormalize")

#Identification of highly variable features (feature selection)
pbmc <- FindVariableFeatures(pbmc, selection.method = "vst", nfeatures = 2000)




# Identify the 10 most highly variable genes
top10 <- head(VariableFeatures(pbmc), 10)
top10




# plot variable features with and without labels
plot1 <- VariableFeaturePlot(pbmc)
plot2 <- LabelPoints(plot = plot1, points = top10, repel = TRUE)

plot1
plot2
#or
plot1 + plot2



#Scaling of data
all.genes <- rownames(pbmc)
pbmc <- ScaleData(pbmc, features = all.genes)



#Run PCA
pbmc <- RunPCA(pbmc, features = VariableFeatures(object = pbmc))

# Examine and visualize PCA results a few different ways
print(pbmc[["pca"]], dims = 1:5, nfeatures = 5)
VizDimLoadings(pbmc, dims = 1:2, nfeatures = 15, reduction = "pca")
DimPlot(pbmc, reduction = "pca")
DimHeatmap(pbmc, dims = 1, cells = 500, balanced = TRUE)



Elbowplot(pbmc)

#Clustering of cells 
pbmc <- FindNeighbors(pbmc, dims = 1:10)
pbmc <- FindClusters(pbmc, resolution = 0.5)



# Look at cluster IDs of the first 5 cells
head(Idents(pbmc), 5)



#Run UMAP non linear dimentionality reduction 
pbmc <- RunUMAP(pbmc, dims = 1:10)

DimPlot(pbmc, reduction = "umap")




# find all markers of cluster 1
cluster1.markers <- FindMarkers(pbmc, ident.1 = 1, min.pct = 0.25)
head(cluster1.markers, n = 5)

VlnPlot(pbmc, features = c(row.names(cluster1.markers)[1], row.names(cluster1.markers)[2]))




# find all markers of cluster 2
cluster2.markers <- FindMarkers(pbmc, ident.1 = 2, min.pct = 0.25)
head(cluster2.markers, n = 5)

VlnPlot(pbmc, features = c(row.names(cluster2.markers)[1], row.names(cluster2.markers)[2]))






# find all markers distinguishing cluster 5 from clusters 0 and 3
cluster5.markers <- FindMarkers(pbmc, ident.1 = 5, ident.2 = c(0, 3), min.pct = 0.25)
head(cluster5.markers, n = 5)

VlnPlot(pbmc, features = c(row.names(cluster5.markers)[1], row.names(cluster5.markers)[2]))




# find markers for every cluster compared to all remaining cells, report only the positive ones
pbmc.markers <- FindAllMarkers(pbmc, only.pos = TRUE, min.pct = 0.25, logfc.threshold = 0.25)




x <- pbmc.markers %>% group_by(cluster) %>% top_n(n = 1, wt = avg_log2FC)
FeaturePlot(pbmc, features = x$gene[1:4])
FeaturePlot(pbmc, features = x$gene[5:8])




# Assigning cell type identity to clusters
new.cluster.ids <- c("Naive CD4 T", "Memory CD4 T", "CD14+ Mono", "B", "CD8 T", "FCGR3A+ Mono", "NK", "DC", "Platelet")
names(new.cluster.ids) <- levels(pbmc)

pbmc <- RenameIdents(pbmc, new.cluster.ids)
DimPlot(pbmc, reduction = "pca", label = TRUE, pt.size = 0.5)

pbmc
DimPlot(pbmc, reduction = "umap", label = TRUE, pt.size = 0.5)


