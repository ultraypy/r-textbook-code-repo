# Seurat 流程参考代码：PBMC 4k 数据分析工作流

# ====================================================================
# I. 环境配置与数据导入
# ====================================================================

# 1. 安装 Seurat (如果尚未安装)
if (!requireNamespace("Seurat", quietly = TRUE)) install.packages("Seurat")

# 2. 加载所需库
library(Seurat)
library(TENxPBMCData)
library(SingleCellExperiment)

# 3. 数据导入与转换为 Seurat 对象
# 从 ExperimentHub 加载数据 (SCE 对象)
tenx_pbmc4k <- TENxPBMCData(dataset = "pbmc4k")
# 提取原始计数矩阵
counts_matrix <- counts(tenx_pbmc4k)

# 创建 Seurat 对象 (从原始计数开始)
pbmc_seurat <- CreateSeuratObject(counts = counts_matrix, project = "PBMC_4k")

# ====================================================================
# II. 质量控制（QC）与过滤
# ====================================================================

# 1. 计算线粒体 QC 指标
pbmc_seurat[["percent.mt"]] <- PercentageFeatureSet(pbmc_seurat, pattern = "^MT-")

# 2. 过滤低质量细胞
# 过滤条件：最少 200 个基因；最多 50000 UMI；线粒体占比低于 15%
pbmc_seurat_filtered <- subset(pbmc_seurat, subset = nFeature_RNA > 200 & 
                                            nCount_RNA < 50000 & 
                                            percent.mt < 15)

# ====================================================================
# III. 标准化、降维与聚类
# ====================================================================

# 1. 标准化 (LogNormalize)
pbmc_seurat_filtered <- NormalizeData(pbmc_seurat_filtered, normalization.method = "LogNormalize", scale.factor = 10000)

# 2. 特征选择 (鉴定高变异基因)
pbmc_seurat_filtered <- FindVariableFeatures(pbmc_seurat_filtered, selection.method = "vst", nfeatures = 2000)

# 3. 尺度归一化 (ScaleData)
all.genes <- rownames(pbmc_seurat_filtered)
pbmc_seurat_filtered <- ScaleData(pbmc_seurat_filtered, features = all.genes)

# 4. 运行 PCA
pbmc_seurat_filtered <- RunPCA(pbmc_seurat_filtered, features = VariableFeatures(object = pbmc_seurat_filtered))

# 5. 运行 UMAP
pbmc_seurat_filtered <- RunUMAP(pbmc_seurat_filtered, dims = 1:10)

# 6. 构建邻域图与聚类 (Louvain Algorithm)
pbmc_seurat_filtered <- FindNeighbors(pbmc_seurat_filtered, dims = 1:10)
pbmc_seurat_filtered <- FindClusters(pbmc_seurat_filtered, resolution = 0.5)

# ====================================================================
# IV. 可视化与标记基因鉴定
# ====================================================================

# 1. 可视化聚类结果
DimPlot(pbmc_seurat_filtered, reduction = "umap", label = TRUE)

# 2. 鉴定标记基因 (使用 Seurat 默认的 Wilcoxon Rank Sum Test)
# 鉴定 Cluster 0 的标记基因
cluster0.markers <- FindMarkers(pbmc_seurat_filtered, ident.1 = 0, min.pct = 0.25)
head(cluster0.markers, n = 5)

# 3. 鉴定所有聚类的标记基因
pbmc.markers <- FindAllMarkers(pbmc_seurat_filtered, only.pos = TRUE, 
                               min.pct = 0.25, logfc.threshold = 0.25)

# 4. 筛选 Top 5 标记基因
top5_markers <- pbmc.markers %>%
    group_by(cluster) %>%
    slice_max(n = 5, order_by = avg_log2FC)

# 最终：记录会话信息以确保可重复性
sessionInfo()
