rm(list = ls())

# 加载相关的包
library(linkET)
library(ggplot2)
library(dplyr)
library(openxlsx)



otu <- read.csv("QS.csv",header = TRUE, row.names = 1)
otu <- t(otu)
env <- read.csv("ENV.csv",header = TRUE, row.names = 1)





# 创建结果列表
mantel_list <- list()

# 循环遍历每一列细菌 OTU
for (j in 1:2) {
  spec_select <- otu[, j]
  
  # 创建当前细菌 OTU 列对应的结果列表
  result_list <- list()
  
  # 循环遍历每一列环境因素
  for (k in 1:2) {
    env_select <- env[, k]
    
    # 进行 Spearman 相关性测试
    test <- cor.test(spec_select, env_select, method = "spearman")
    
    # 提取 P 值和估计值
    result <- data.frame(
      Taxon = colnames(otu)[j], # 加入菌名称列
      p.value = test$p.value,
      estimate = test$estimate
    )
    
    # 将结果加入到当前细菌 OTU 列对应的结果列表
    result_list[[colnames(env)[k]]] <- result
  }
  
  # 将当前细菌 OTU 列对应的结果列表加入到结果列表
  mantel_list[[colnames(otu)[j]]] <- result_list
}

# 将结果转换为数据框形式
result_df <- do.call(rbind, lapply(mantel_list, function(x) do.call(rbind, x)))

# 重新排列列的顺序，将菌名称列放在第一列
result_df <- result_df[, c("Taxon", "p.value", "estimate")]

# 输出结果数据框
print(result_df)

#写出文件
write.csv(result_df,"QS_result.csv")
