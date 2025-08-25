# R/iris_analysis_script.R

##############################################################################
# 脚本名称：iris_analysis.R
# 作      者：XXX
# 创建日期：2025-08-25
# 脚本功能：对鸢尾花（iris）数据集进行探索性数据分析，包括描述性统计和可视化
# 数据来源：R 语言内置数据集
# 依赖包：dplyr, ggplot2, tidyr
##############################################################################

# ==============================================================================
# 1. 安装并加载依赖包
# ==============================================================================
required_packages <- c("dplyr", "ggplot2", "tidyr")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
}
library(dplyr)
library(ggplot2)
library(tidyr)

# ==============================================================================
# 2. 数据加载与预处理
# ==============================================================================
data(iris)
cat("数据集基本信息：
")
str(iris)
cat("
数据集前 5 行：
")
head(iris)
cat("
数据集缺失值情况：
")
print(colSums(is.na(iris)))

iris_processed <- iris %>%
  mutate(
    Sepal_Area = Sepal.Length * Sepal.Width,
    Petal_Area = Petal.Length * Petal.Width
  ) %>%
  rename(
    Sepal_Length = Sepal.Length,
    Sepal_Width = Sepal.Width,
    Petal_Length = Petal.Length,
    Petal_Width = Petal.Width
  )
cat("
预处理后的数据前 5 行：
")
head(iris_processed)

# ==============================================================================
# 3. 数据分析：描述性统计
# ==============================================================================
iris_stats <- iris_processed %>%
  group_by(Species) %>%
  summarise(
    Sepal_Length_Mean = mean(Sepal_Length, na.rm = TRUE),
    Sepal_Width_Mean = mean(Sepal_Width, na.rm = TRUE),
    Petal_Length_Mean = mean(Petal_Length, na.rm = TRUE),
    Petal_Width_Mean = mean(Petal_Width, na.rm = TRUE),
    Sepal_Area_Mean = mean(Sepal_Area, na.rm = TRUE),
    Petal_Area_Mean = mean(Petal_Area, na.rm = TRUE),
    .groups = "drop"
  )
cat("
各品种鸢尾花的描述性统计结果：
")
print(iris_stats)

# ==============================================================================
# 4. 数据可视化
# ==============================================================================
theme_set(theme_minimal())
p1 <- ggplot(iris_processed, aes(x = Sepal_Length, y = Sepal_Width)) +
  geom_point(aes(color = Species, shape = Species), size = 2) +
  labs(
    title = "鸢尾花花萼长度与宽度的关系",
    x = "花萼长度 (cm)", y = "花萼宽度 (cm)", color = "品种", shape = "品种"
  ) +
  theme(plot.title = element_text(hjust = 0.5))
print(p1)
# ggsave("output/sepal_scatter.png", plot = p1, width = 10, height = 6, dpi = 300)

p2 <- ggplot(iris_processed, aes(x = Species, y = Petal_Area)) +
  geom_boxplot(aes(fill = Species), alpha = 0.7) +
  labs(
    title = "不同品种鸢尾花的花瓣面积分布",
    x = "品种", y = "花瓣面积 (cm²)", fill = "品种"
  ) +
  theme(plot.title = element_text(hjust = 0.5))
print(p2)
# ggsave("output/petal_area_boxplot.png", plot = p2, width = 8, height = 6, dpi = 300)

iris_long <- iris_processed %>%
  pivot_longer(
    cols = -Species,
    names_to = "Variable",
    values_to = "Value"
  )
p3 <- ggplot(iris_long, aes(x = Value, fill = Species)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~Variable, scales = "free") +
  labs(
    title = "鸢尾花各变量的分布（按品种）",
    x = "变量值", y = "密度", fill = "品种"
  ) +
  theme(plot.title = element_text(hjust = 0.5))
print(p3)
# ggsave("output/variables_density.png", plot = p3, width = 12, height = 8, dpi = 300)
