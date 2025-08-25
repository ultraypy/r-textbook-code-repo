# R/chapter_01_basics.R

# ==============================================================================
# 1.2 R 语言基础
# ==============================================================================

# 1.2.1 R 包管理与 Bioconductor 生态
# 安装 BiocManager
# if (!requireNamespace("BiocManager", quietly = TRUE))
#     install.packages("BiocManager")
# 安装 Bioconductor 包 (例如 limma)
# BiocManager::install("limma")
# 加载包
# library(ggplot2)
# library(limma)

# 1.2.2 简单计算、函数调用与帮助获取
# 简单计算
5 + 3
10 - 4
6 * 7
15 / 3
2 ^ 4
log(10)
log10(100)

# 函数调用
sum(1, 2, 3, 4)
mean(c(2, 4, 6, 8))

# 帮助获取
# help(sum)
# ?mean
# example(mean)
# help.search("regression")
# ??regression

# 1.2.3 变量与赋值
# 赋值数值
x <- 5
x
# 赋值字符
name <- "R Language"
name
# 赋值逻辑值
is_true <- TRUE
is_true
# 赋值向量
vec <- c(1, 2, 3, 4, 5)
vec

# 1.2.4 向量、向量化与索引
# 创建向量
num_vec <- c(10, 20, 30, 40, 50)
char_vec <- c("apple", "banana", "orange", "grape")
log_vec <- c(TRUE, FALSE, TRUE, TRUE)
# 向量化操作
num_vec + 5
num_vec * 2
vec1 <- c(1, 2, 3)
vec2 <- c(4, 5, 6)
vec1 + vec2
vec1 * vec2
# 索引
num_vec[3]
num_vec[c(1, 4)]
num_vec[2:5]
num_vec[num_vec > 30]
num_vec[num_vec == 20 | num_vec == 50]
num_vec[num_vec %in% c(20, 50)]
num_vec[-2]
