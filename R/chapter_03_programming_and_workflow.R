# R/chapter_03_programming_and_workflow.R

# ==============================================================================
# 3.1 编程与工作流
# ==============================================================================
# 3.1.1 控制流：if/for/while
# if/else if/else
score <- 85
if (score >= 90) {
  grade <- "A"
} else if (score >= 80) {
  grade <- "B"
} else {
  grade <- "C"
}
cat("学生成绩等级为：", grade, "\n")
# for 循环
squares <- numeric(5)
for (i in 1:5) {
  squares[i] <- i ^ 2
}
print(squares)
# while 循环
sum_result <- 0
i <- 1
while (sum_result <= 50) {
  sum_result <- sum_result + i
  i <- i + 1
}
cat("累加和首次大于 50 时的和为：", sum_result, "\n")

# 3.2.1 多文件加载与合并
# 假设数据文件在 data/multiple_files/ 目录下
# setwd("path/to/r-textbook-code-repo")
library(dplyr)
library(readr) # 使用 readr 包的 read_csv 更高效
data_dir <- "data/multiple_files"
# 修正了正则表达式中的转义字符，使用了双反斜杠 `\\`
csv_files <- list.files(path = data_dir, pattern = "\\.csv$", full.names = TRUE)
data_list <- lapply(csv_files, read_csv)
combined_data <- bind_rows(data_list)
print(head(combined_data))

# 3.2.2 数据导出
# 创建一个数据框用于导出
result_df <- data.frame(
  Name = c("张三", "李四", "王五"),
  Age = c(20, 21, 22),
  Score = c(85, 92, 78)
)
# 导出为 CSV 文件
write.csv(result_df, file = "output/student_scores.csv", row.names = FALSE, fileEncoding = "UTF-8")
# 导出为 TXT 文件
write.table(result_df, file = "output/student_scores.txt", sep = "\t", row.names = FALSE, fileEncoding = "UTF-8")
# 导出为 Excel 文件（需安装 openxlsx 包）
# library(openxlsx)
# write.xlsx(result_df, file = "output/student_scores.xlsx", sheetName = "学生成绩", rowNames = FALSE)
# 导出 ggplot2 图形
library(ggplot2)
p <- ggplot(result_df, aes(x = Age, y = Score)) +
  geom_point(size = 3) +
  labs(title = "学生年龄与成绩的关系")
# ggsave("output/age_score.png", plot = p)