# R/chapter_02_data_processing_and_vis.R

# ==============================================================================
# 2.1 在 R 中处理与可视化数据
# ==============================================================================
# 需要先安装并加载相关包
# install.packages(c("readxl", "dplyr", "ggplot2", "tidyr", "stringr"))
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)

# 2.1.1 数据加载到 R
# 加载 CSV 文件
data_csv <- read.csv(file = "data/data.csv", header = TRUE, sep = ",")
# 加载 Excel 文件
data_excel <- read_excel(path = "data/data.xlsx", sheet = 1)
# 加载 TXT 文件
data_txt <- read.table(file = "data/data.txt", header = TRUE, sep = "	", stringsAsFactors = FALSE)
# 查看数据前 6 行
head(data_csv)

# 2.1.2 数据框探索与转换
# 使用 R 内置的 iris 数据集进行演示
data(iris)
# 数据框探索
str(iris)
summary(iris)
dim(iris)
nrow(iris)
ncol(iris)
colnames(iris)
head(iris, n = 3)
sum(is.na(iris))
# 数据框转换
iris$Sepal.Area <- iris$Sepal.Length * iris$Sepal.Width
head(iris)
# 删除列
iris$Sepal.Area <- NULL
# 修改列名
colnames(iris)[which(colnames(iris) == "Sepal.Length")] <- "Sepal_Length"
# 数据类型转换
iris$Species <- as.character(iris$Species)
# 假设有缺失值的数据框
iris_with_na <- iris
iris_with_na[1:3, "Sepal_Length"] <- NA
# 处理缺失值
iris_no_na <- na.omit(iris_with_na)
# 或者使用 complete.cases
iris_no_na2 <- iris_with_na[complete.cases(iris_with_na), ]
# 填充缺失值（用均值）
iris_with_na$Sepal_Length[is.na(iris_with_na$Sepal_Length)] <- mean(iris_with_na$Sepal_Length, na.rm = TRUE)

# 2.1.3 数据切片：子集化数据框
# 按列子集化
iris_cols1 <- iris[, c(1, 3)]
iris_cols2 <- iris[, c("Sepal_Length", "Species")]
# 按行子集化
iris_rows1 <- iris[c(1, 5, 10), ]
iris_sepal_long <- iris[iris$Sepal_Length > 5.0, ]
iris_two_species <- iris[iris$Species %in% c("versicolor", "virginica"), ]
# 同时按行和列子集化
iris_subset <- iris[iris$Sepal_Length > 5.0, c("Sepal_Length", "Petal.Length", "Species")]

# ==============================================================================
# 2.2 ggplot2 可视化技术
# ==============================================================================
# 2.2.1 散点图与密度图
# 散点图
p1 <- ggplot(data = iris, aes(x = Sepal_Length, y = Sepal.Width)) +
  geom_point(aes(color = Species, shape = Species), size = 2) +
  labs(title = "鸢尾花花萼长度与宽度的关系",
       x = "花萼长度 (cm)", y = "花萼宽度 (cm)",
       color = "鸢尾花品种", shape = "鸢尾花品种") +
  theme_minimal()
print(p1)

# 密度图
p2 <- ggplot(data = iris, aes(x = Petal.Length, fill = Species)) +
  geom_density(alpha = 0.5) +
  labs(title = "不同品种鸢尾花的花瓣长度分布",
       x = "花瓣长度 (cm)", y = "密度", fill = "鸢尾花品种") +
  theme_bw()
print(p2)

# 2.2.2 平滑技术
# 线性回归平滑（使用内置的 mtcars 数据集）
data(mtcars)
p3 <- ggplot(data = mtcars, aes(x = wt, y = mpg)) +
  geom_point(size = 2, color = "blue") +
  geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +
  labs(title = "汽车重量与油耗的关系",
       x = "车重 (千磅)", y = "每加仑英里数 (mpg)") +
  theme_classic()
print(p3)

# 局部加权散点平滑 (LOESS)
p4 <- ggplot(data = iris, aes(x = Petal.Length, y = Petal.Width)) +
  geom_point(aes(color = Species), size = 2) +
  geom_smooth(method = "loess", se = TRUE, span = 0.75) +
  labs(title = "鸢尾花花瓣长度与宽度的关系（LOESS平滑）",
       x = "花瓣长度 (cm)", y = "花瓣宽度 (cm)", color = "鸢尾花品种") +
  theme_minimal()
print(p4)

# 2.2.3 数据分箱与条形图
# 数据分箱
mtcars$mpg_bin <- cut(mtcars$mpg, breaks = 3, labels = c("低油耗", "中油耗", "高油耗"))
# 频数条形图
p5 <- ggplot(data = mtcars, aes(x = factor(cyl))) +
  geom_bar(fill = "steelblue", width = 0.7) +
  labs(title = "不同气缸数汽车的数量分布", x = "气缸数", y = "汽车数量") +
  theme_bw()
print(p5)

# 均值条形图
iris_mean <- aggregate(Sepal_Length ~ Species, data = iris, FUN = mean)
p6 <- ggplot(data = iris_mean, aes(x = Species, y = Sepal_Length)) +
  geom_col(fill = c("red", "green", "blue"), width = 0.7) +
  labs(title = "不同品种鸢尾花的平均花萼长度", x = "鸢尾花品种", y = "平均花萼长度 (cm)") +
  theme_minimal()
print(p6)

# 2.2.4 数据合并
# 向量匹配
id <- c(101, 102, 103, 104, 105)
score <- c(85, 92, 78, 90, 88)
selected_id <- c(102, 104, 106)
positions <- match(selected_id, id)
selected_score <- score[positions]
# 数据框合并（内连接）
student_info <- data.frame(id = c(101, 102, 103, 104, 105), name = c("张三", "李四", "王五", "赵六", "钱七"))
student_score <- data.frame(id = c(102, 103, 104, 106, 107), score = c(92, 78, 90, 85, 88))
inner_join_df <- inner_join(student_info, student_score, by = "id")
# 左连接
left_join_df <- left_join(student_info, student_score, by = "id")
# 右连接
right_join_df <- right_join(student_info, student_score, by = "id")
# 全连接
full_join_df <- full_join(student_info, student_score, by = "id")

# 2.2.5 ggplot2 分面
# 按 Species 分面
p7 <- ggplot(data = iris, aes(x = Petal.Length, y = Petal.Width)) +
  geom_point(size = 2, color = "purple") +
  facet_wrap(~ Species, ncol = 1, scales = "free") +
  labs(title = "不同品种鸢尾花的花瓣长度与宽度关系",
       x = "花瓣长度 (cm)", y = "花瓣宽度 (cm)") +
  theme_minimal()
print(p7)
# 按 am 和 cyl 分面
mtcars$cyl <- factor(mtcars$cyl, labels = c("4缸", "6缸", "8缸"))
mtcars$am <- factor(mtcars$am, labels = c("自动变速箱", "手动变速箱"))
p8 <- ggplot(data = mtcars, aes(x = wt, y = mpg)) +
  geom_point(size = 2, color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  facet_grid(am ~ cyl, scales = "fixed") +
  labs(title = "不同变速箱类型和气缸数的汽车重量与油耗关系",
       x = "车重 (千磅)", y = "每加仑英里数 (mpg)") +
  theme_bw()
print(p8)