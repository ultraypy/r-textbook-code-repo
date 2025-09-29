# R语言单细胞转录组教程脚本：基于Bioconductor和PBMC 4k数据

# ====================================================================
# I. 环境配置与软件包安装
# ====================================================================

# 1. 安装BiocManager（如果尚未安装）
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager", repos = "https://cloud.r-project.org")

# 2. 安装核心Bioconductor软件包（需时较久）
BiocManager::install(c("SingleCellExperiment", "TENxPBMCData", "scater", "scran", "SC3", 
                      "MAST", "celldex", "SingleR", "data.table", "GenomicRanges"))
# 安装 CRAN 包
install.packages(c("dplyr", "tibble"), repos = "https://cloud.r-project.org")

# 3. 加载所需库
library(SingleCellExperiment)
library(TENxPBMCData)
library(scater)
library(scran)
library(SC3) 
library(MAST)
library(celldex)
library(SingleR)
library(dplyr)
library(data.table)

# ====================================================================
# II. 数据获取与S4对象访问
# ====================================================================

# 1. 从ExperimentHub加载PBMC 4k数据集
tenx_pbmc4k <- TENxPBMCData(dataset = "pbmc4k")

# ====================================================================
# III. 质量控制（QC）与数据过滤
# ====================================================================

# 1. 计算每个细胞的总计数（UMI）
colData(tenx_pbmc4k)$Total_Counts <- colSums(counts(tenx_pbmc4k))

# 2. 识别线粒体基因
is_mito <- grepl("^MT-", rowData(tenx_pbmc4k)$Symbol_TENx) 

# 3. 计算线粒体基因的计数总和与占比
tenx_pbmc4k$Mito_Counts <- colSums(counts(tenx_pbmc4k)[is_mito, ])
colData(tenx_pbmc4k)$Mito_Percent <- tenx_pbmc4k$Mito_Counts / tenx_pbmc4k$Total_Counts

# 4. 应用条件子集选取进行过滤
min_counts <- 1000
max_mito_percent <- 0.15

keep_cells <- colData(tenx_pbmc4k)$Total_Counts > min_counts & 
              colData(tenx_pbmc4k)$Mito_Percent < max_mito_percent

pbmc_sce_filtered <- tenx_pbmc4k[, keep_cells]

# ====================================================================
# IV. 标准化、特征选择与降维
# ====================================================================

# 1. 使用scran计算大小因子和归一化
pbmc_sce_filtered <- computeSumFactors(pbmc_sce_filtered) 
pbmc_sce_filtered <- logNormCounts(pbmc_sce_filtered) 

# 2. 鉴定高变异基因（HVGs）
var_fit <- modelGeneVar(pbmc_sce_filtered)
top_hvgs <- getTopHVGs(var_fit, fdr.threshold = 0.05)
rowData(pbmc_sce_filtered)$Is_HVG <- rownames(pbmc_sce_filtered) %in% top_hvgs

# 3. 主成分分析（PCA）
pbmc_sce_filtered <- runPCA(pbmc_sce_filtered) 

# 4. 运行非线性降维（UMAP）
pbmc_sce_filtered <- runUMAP(pbmc_sce_filtered)

# ====================================================================
# VI. 聚类、标记基因鉴定与细胞类型注释
# ====================================================================

# 6.1 无监督聚类：SC3 (作为示例)
# 为了使后续代码可运行，我们在此创建一个假的聚类 ID
set.seed(42)
colData(pbmc_sce_filtered)$cluster_ID <- as.factor(sample(paste0("Cluster ", 1:5), ncol(pbmc_sce_filtered), replace = TRUE))


# 6.2 标记基因鉴定与差异表达分析 (使用 MAST)
# 1. 数据准备：将 SCE 转换为 MAST 的 SingleCellAssay (SCA) 对象
sca <- SceToSingleCellAssay(pbmc_sce_filtered, assay.type = "logcounts") 

# 2. 准备协变量： log(Total_Counts) 和 cluster_ID
colData(sca)$nUMI <- log(colData(sca)$Total_Counts)
colData(sca)$cluster_ID <- as.factor(colData(sca)$cluster_ID)

# 3. 定义和拟合障碍模型 (Hurdle Model)
zlm.output <- zlm(
    formula = ~ cluster_ID + nUMI,
    sca = sca,
    method = 'bayesglm', 
    exprs_values = "logcounts" 
)

# 4. 执行似然比检验 (LRT) 提取差异表达结果
summary.LRT <- MAST::summary(zlm.output, doLRT = 'cluster_ID')

# 5. 提取差异表达结果表 (Focus on Cluster 1 vs. All)
de.results <- data.table::as.data.table(summary.LRT$datatable)
de.continuous <- subset(de.results, component == "C" & 
                                     grepl("cluster_IDCluster 1", contrast) & 
                                !is.na(qval))

# 6. 计算效应大小指标：检测百分比差异 (Delta Pct)
target_cluster <- "Cluster 1" 
cluster_1_cells <- which(colData(pbmc_sce_filtered)$cluster_ID == target_cluster)
other_cells <- which(colData(pbmc_sce_filtered)$cluster_ID!= target_cluster)
log_exprs <- assay(pbmc_sce_filtered, "logcounts")

pct.1 <- rowSums(log_exprs[, cluster_1_cells] > 0) / length(cluster_1_cells)
pct.2 <- rowSums(log_exprs[, other_cells] > 0) / length(other_cells)

# 7. 将 pct 指标与 DE 结果表合并并应用过滤
de.continuous[, Gene := as.character(primerid)] 
pct_data <- data.frame(Gene = names(pct.1), pct.1 = pct.1, pct.2 = pct.2, Delta_Pct = pct.1 - pct.2)
final.markers <- merge(de.continuous, pct_data, by = "Gene")

# 8. 最终过滤（专注于 LFC 和 Delta Pct）
top_markers <- subset(final.markers, 
                      qval < 0.05 & 
                      coef > 0.5 & # LFC > 0.5 (对应 coef 列)
                      Delta_Pct > 0.2
)


# 6.3 细胞类型注释 (使用 SingleR)

# 1. 获取参考数据集：Human Primary Cell Atlas (HPCA)
ref.set <- celldex::HumanPrimaryCellAtlasData() 

# 2. 执行 SingleR 预测
pred.cnts <- SingleR::SingleR(
    test = pbmc_sce_filtered,
    ref = ref.set,
    labels = ref.set$label.main, 
    assay.type.test = "logcounts" 
)

# 3. 将注释结果整合到 SCE 对象的 colData 中
colData(pbmc_sce_filtered)$SingleR_Annotation <- pred.cnts$pruned.labels

# 4. 可视化注释结果（UMAP 图）
plotReducedDim(pbmc_sce_filtered, dimred = "UMAP", colour_by = "SingleR_Annotation")

# 最终：记录会话信息以确保可重复性
sessionInfo()
