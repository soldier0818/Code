


#1#####################总PFS生存分析######################
library(openxlsx)
a = read.xlsx("数据分析.xlsx")
library(survival)

library(survminer)


fit <- survfit(Surv(PFS, 是否进展 == 1) ~ 分组, data = a)

ggsurvplot(fit,
           pval = TRUE, pval.size = 5, pval.coord = c(0.01, 0.1), # 在图上添加log rank检验的p值
           conf.int = TRUE,# 添加置信区间
           conf.int.style = "ribbon",  # 设置置信区间的风格?
           conf.int.alpha = c(0.2), # 生存曲线置信区间透明度
           xlab = "Time in months",   # 设置x轴标签
           ylab = "Progression-Free-Survival ", # y轴标签
           break.time.by = 3,     # 将x轴按照6为间隔进行切分
           xlim = c(0,30), # 自定义x轴（时间）显示范围
           censor.shape= "|", censor.size = 3, # 删失线形状和大小
           risk.table = "abs_pct",  # 在风险表中添加绝对数和相对数
           risk.table.y.text.col = TRUE,# 设置风险表的文字颜色
           risk.table.y.text = FALSE, # risk.table y轴显示文字，FALSE则显示色块
           risk.table.title = "Number at risk", # risk.table标题
           risk.table.fontsize = 3, # risk.table字体大小
           risk.table.height = 0.25, # risk.table占图片比例
           surv.plot.height = 0.75, # 生存图占图片比例
           surv.median.line = "hv",  # 添加中位生存时间
           legend.labs = 
             c("No immuno", "Immuno combination"),    # 改变图例标签
           font.legend = c(16, "plain"),      # 图例字体设置
           font.x = c(16, "plain", "black"),  # x轴标签字体设置
           font.y = c(16, "plain", "black"),  # y轴标签字体设置
           font.xtickslab = c(14, "plain", "black"),  # x轴刻度标签字体设置
           font.ytickslab = c(14, "plain", "black"),  # y轴刻度标签字体设置
           legend = c(0.8, 0.85), # 改变legend的位置为c(0.8, 0.85),也可以设为"top","right"
           palette = 
             c("#CD3333", "#483D8B") # 设置颜色  palette = "nejmhttp://127.0.0.1:38885/graphics/plot_zoom_png?width=1200&height=900", "grey","npg","aaas","lancet","jco", "ucscgb","uchicago","simpsons"和"rickandmorty"
)


###配色
####("#FF4040", "#483D8B")
###palette = "nejm", "grey","npg","aaas","lancet","jco", "ucscgb","uchicago","simpsons"和"rickandmorty"







#2#####################总OS生存分析######################
library(openxlsx)
x = read.xlsx("数据分析.xlsx")
library(survival)

library(survminer)


fit <- survfit(Surv(OS, 是否死亡 == 1) ~ 分组, data = x)

ggsurvplot(fit,
           pval = TRUE, # 在图上添加log rank检验的p值
           conf.int = TRUE,# 添加置信区间
           conf.int.style = "ribbon",  # 设置置信区间的风格?
           conf.int.alpha = c(0.3), # 生存曲线置信区间透明度
           xlab = "Time in months",   # 设置x轴标签
           ylab = "Overall Survival ", # y轴标签
           break.time.by = 3,     # 将x轴按照6为间隔进行切分
           xlim = c(0,30), # 自定义x轴（时间）显示范围
           censor.shape="|", censor.size = 2, # 删失线形状和大小
           risk.table = "abs_pct",  # 在风险表中添加绝对数和相对数
           risk.table.y.text.col = TRUE,# 设置风险表的文字颜色
           risk.table.y.text = FALSE, # risk.table y轴显示文字，FALSE则显示色块
           risk.table.title = "Number at risk", # risk.table标题
           risk.table.fontsize = 3, # risk.table字体大小
           risk.table.height = 0.25, # risk.table占图片比例
           surv.plot.height = 0.75, # 生存图占图片比例
           surv.median.line = "hv",  # 添加中位生存时间
           legend.labs = 
             c("No immuno", "Immuno combination"),    # 改变图例标签
           font.legend = c(16, "plain"),      # 图例字体设置
           font.x = c(16, "plain", "black"),  # x轴标签字体设置
           font.y = c(16, "plain", "black"),  # y轴标签字体设置
           font.xtickslab = c(14, "plain", "black"),  # x轴刻度标签字体设置
           font.ytickslab = c(14, "plain", "black"),  # y轴刻度标签字体设置
           legend = c(0.8, 0.85), # 改变legend的位置为c(0.8, 0.85),也可以设为"top","right"
           palette = 
             c("#CD3333", "#483D8B") # 设置颜色  palette = "nejmhttp://127.0.0.1:38885/graphics/plot_zoom_png?width=1200&height=900", "grey","npg","aaas","lancet","jco", "ucscgb","uchicago","simpsons"和"rickandmorty"
)


###配色
####("#FF4040", "#483D8B")
###palette = "nejm", "grey","npg","aaas","lancet","jco", "ucscgb","uchicago","simpsons"和"rickandmorty"







#3#####################化疗和抗血管的PFS生存分析######################
library(openxlsx)
a = read.xlsx("数据分析.xlsx")
library(survival)

library(survminer)


fit <- survfit(Surv(PFS, 是否进展 == 1) ~ 分组1, data = a)

ggsurvplot(fit,
           pval = TRUE, # 在图上添加log rank检验的p值
           conf.int = TRUE,# 添加置信区间
           conf.int.style = "ribbon",  # 设置置信区间的风格?
           conf.int.alpha = c(0.3), # 生存曲线置信区间透明度
           xlab = "Time in months",   # 设置x轴标签
           ylab = "Progression-Free-Survival ", # y轴标签
           break.time.by = 3,     # 将x轴按照6为间隔进行切分
           xlim = c(0,30), # 自定义x轴（时间）显示范围
           censor.shape="|", censor.size = 2, # 删失线形状和大小
           risk.table = "abs_pct",  # 在风险表中添加绝对数和相对数
           risk.table.y.text.col = TRUE,# 设置风险表的文字颜色
           risk.table.y.text = FALSE, # risk.table y轴显示文字，FALSE则显示色块
           risk.table.title = "Number at risk", # risk.table标题
           risk.table.fontsize = 3, # risk.table字体大小
           risk.table.height = 0.25, # risk.table占图片比例
           surv.plot.height = 0.75, # 生存图占图片比例
           surv.median.line = "hv",  # 添加中位生存时间
           legend.labs = 
             c("Immuno+Chemo", "Immuno+Antiangiogenic"),    # 改变图例标签
           font.legend = c(16, "plain"),      # 图例字体设置
           font.x = c(16, "plain", "black"),  # x轴标签字体设置
           font.y = c(16, "plain", "black"),  # y轴标签字体设置
           font.xtickslab = c(14, "plain", "black"),  # x轴刻度标签字体设置
           font.ytickslab = c(14, "plain", "black"),  # y轴刻度标签字体设置
           legend = c(0.75, 0.85), # 改变legend的位置为c(0.7, 0.85),也可以设为"top","right"
           palette = 
             c("#CD3333", "#483D8B") # 设置颜色  palette = "nejmhttp://127.0.0.1:38885/graphics/plot_zoom_png?width=1200&height=900", "grey","npg","aaas","lancet","jco", "ucscgb","uchicago","simpsons"和"rickandmorty"
)


###配色
####("#FF4040", "#483D8B")
###palette = "nejm", "grey","npg","aaas","lancet","jco", "ucscgb","uchicago","simpsons"和"rickandmorty"





#4#####################化疗和抗血管的OS生存分析######################
library(openxlsx)
a = read.xlsx("数据分析.xlsx")
library(survival)

library(survminer)


fit <- survfit(Surv(OS, 是否死亡 == 1) ~ 分组1, data = a)

ggsurvplot(fit,
           pval = TRUE, # 在图上添加log rank检验的p值
           conf.int = TRUE,# 添加置信区间
           conf.int.style = "ribbon",  # 设置置信区间的风格?
           conf.int.alpha = c(0.3), # 生存曲线置信区间透明度
           xlab = "Time in months",   # 设置x轴标签
           ylab = "Overall Survival ", # y轴标签
           break.time.by = 3,     # 将x轴按照6为间隔进行切分
           xlim = c(0,30), # 自定义x轴（时间）显示范围
           censor.shape="|", censor.size = 2, # 删失线形状和大小
           risk.table = "abs_pct",  # 在风险表中添加绝对数和相对数
           risk.table.y.text.col = TRUE,# 设置风险表的文字颜色
           risk.table.y.text = FALSE, # risk.table y轴显示文字，FALSE则显示色块
           risk.table.title = "Number at risk", # risk.table标题
           risk.table.fontsize = 3, # risk.table字体大小
           risk.table.height = 0.25, # risk.table占图片比例
           surv.plot.height = 0.75, # 生存图占图片比例
           surv.median.line = "hv",  # 添加中位生存时间
           legend.labs = 
             c("Immuno+Chemo", "Immuno+Antiangiogenic"),    # 改变图例标签
           font.legend = c(16, "plain"),      # 图例字体设置
           font.x = c(16, "plain", "black"),  # x轴标签字体设置
           font.y = c(16, "plain", "black"),  # y轴标签字体设置
           font.xtickslab = c(14, "plain", "black"),  # x轴刻度标签字体设置
           font.ytickslab = c(14, "plain", "black"),  # y轴刻度标签字体设置
           legend = c(0.79, 0.87), # 改变legend的位置为c(0.8, 0.85),也可以设为"top","right"
           palette = 
             c("#CD3333", "#483D8B") # 设置颜色  palette = "nejmhttp://127.0.0.1:38885/graphics/plot_zoom_png?width=1200&height=900", "grey","npg","aaas","lancet","jco", "ucscgb","uchicago","simpsons"和"rickandmorty"
)


###配色
####("#FF4040", "#483D8B")
###palette = "nejm", "grey","npg","aaas","lancet","jco", "ucscgb","uchicago","simpsons"和"rickandmorty"





##5####################持续时间的OS生存分析######################
library(openxlsx)
a = read.xlsx("数据分析2.xlsx")
library(survival)

library(survminer)


fit <- survfit(Surv(OS, 是否死亡 == 1) ~ 分组3, data = a)

ggsurvplot(fit,
           pval = TRUE, # 在图上添加log rank检验的p值
           conf.int = TRUE,# 添加置信区间
           conf.int.style = "ribbon",  # 设置置信区间的风格?
           conf.int.alpha = c(0.3), # 生存曲线置信区间透明度
           xlab = "Time in months",   # 设置x轴标签
           ylab = "Overall Survival ", # y轴标签
           break.time.by = 3,     # 将x轴按照6为间隔进行切分
           xlim = c(0,30), # 自定义x轴（时间）显示范围
           censor.shape="|", censor.size = 2, # 删失线形状和大小
           risk.table = "abs_pct",  # 在风险表中添加绝对数和相对数
           risk.table.y.text.col = TRUE,# 设置风险表的文字颜色
           risk.table.y.text = FALSE, # risk.table y轴显示文字，FALSE则显示色块
           risk.table.title = "Number at risk", # risk.table标题
           risk.table.fontsize = 3, # risk.table字体大小
           risk.table.height = 0.25, # risk.table占图片比例
           surv.plot.height = 0.75, # 生存图占图片比例
           surv.median.line = "hv",  # 添加中位生存时间
           legend.labs = 
             c("Duration > 6 months", "Duration ≤ 6 months"),    # 改变图例标签
           font.legend = c(15, "plain"),      # 图例字体设置
           font.x = c(16, "plain", "black"),  # x轴标签字体设置
           font.y = c(16, "plain", "black"),  # y轴标签字体设置
           font.xtickslab = c(14, "plain", "black"),  # x轴刻度标签字体设置
           font.ytickslab = c(14, "plain", "black"),  # y轴刻度标签字体设置
           legend = c(0.8, 0.85), # 改变legend的位置为c(0.8, 0.85),也可以设为"top","right"
           palette = 
             c("#CD3333", "#483D8B") # 设置颜色  palette = "nejmhttp://127.0.0.1:38885/graphics/plot_zoom_png?width=1200&height=900", "grey","npg","aaas","lancet","jco", "ucscgb","uchicago","simpsons"和"rickandmorty"
)


###配色
####("#FF4040", "#483D8B")
###palette = "nejm", "grey","npg","aaas","lancet","jco", "ucscgb","uchicago","simpsons"和"rickandmorty"








##6####################持续时间的PFS生存分析######################
library(openxlsx)
a = read.xlsx("数据分析2.xlsx")
library(survival)

library(survminer)


fit <- survfit(Surv(PFS, 是否进展 == 1) ~ 分组3, data = a)

ggsurvplot(fit,
           pval = TRUE, # 在图上添加log rank检验的p值
           conf.int = TRUE,# 添加置信区间
           conf.int.style = "ribbon",  # 设置置信区间的风格?
           conf.int.alpha = c(0.3), # 生存曲线置信区间透明度
           xlab = "Time in months",   # 设置x轴标签
           ylab = "Progression-Free-Survival ", # y轴标签
           break.time.by = 3,     # 将x轴按照6为间隔进行切分
           xlim = c(0,30), # 自定义x轴（时间）显示范围
           censor.shape="|", censor.size = 2, # 删失线形状和大小
           risk.table = "abs_pct",  # 在风险表中添加绝对数和相对数
           risk.table.y.text.col = TRUE,# 设置风险表的文字颜色
           risk.table.y.text = FALSE, # risk.table y轴显示文字，FALSE则显示色块
           risk.table.title = "Number at risk", # risk.table标题
           risk.table.fontsize = 3, # risk.table字体大小
           risk.table.height = 0.25, # risk.table占图片比例
           surv.plot.height = 0.75, # 生存图占图片比例
           surv.median.line = "hv",  # 添加中位生存时间
           legend.labs = 
             c("Duration > 6 months", "Duration ≤ 6 months"),    # 改变图例标签
           font.legend = c(16, "plain"),      # 图例字体设置
           font.x = c(16, "plain", "black"),  # x轴标签字体设置
           font.y = c(16, "plain", "black"),  # y轴标签字体设置
           font.xtickslab = c(14, "plain", "black"),  # x轴刻度标签字体设置
           font.ytickslab = c(14, "plain", "black"),  # y轴刻度标签字体设置
           legend = c(0.8, 0.85), # 改变legend的位置为c(0.8, 0.85),也可以设为"top","right"
           palette = 
             c("#CD3333", "#483D8B") # 设置颜色  palette = "nejmhttp://127.0.0.1:38885/graphics/plot_zoom_png?width=1200&height=900", "grey","npg","aaas","lancet","jco", "ucscgb","uchicago","simpsons"和"rickandmorty"
)


###配色
####("#FF4040", "#483D8B")
###palette = "nejm", "grey","npg","aaas","lancet","jco", "ucscgb","uchicago","simpsons"和"rickandmorty"








##7####################药物类型的PFS生存分析######################
library(openxlsx)
a = read.xlsx("二逼.xlsx")
library(survival)

library(survminer)


fit <- survfit(Surv(PFS, 是否进展 == 1) ~ 药物类型分组, data = a)

ggsurvplot(fit,
           pval = TRUE, # 在图上添加log rank检验的p值
           conf.int = TRUE,# 添加置信区间
           conf.int.style = "ribbon",  # 设置置信区间的风格?
           conf.int.alpha = c(0.3), # 生存曲线置信区间透明度
           xlab = "Time in months",   # 设置x轴标签
           ylab = "Progression-Free-Survival ", # y轴标签
           break.time.by = 3,     # 将x轴按照6为间隔进行切分
           xlim = c(0,30), # 自定义x轴（时间）显示范围
           censor.shape="|", censor.size = 2, # 删失线形状和大小
           risk.table = "abs_pct",  # 在风险表中添加绝对数和相对数
           risk.table.y.text.col = TRUE,# 设置风险表的文字颜色
           risk.table.y.text = FALSE, # risk.table y轴显示文字，FALSE则显示色块
           risk.table.title = "Number at risk", # risk.table标题
           risk.table.fontsize = 3, # risk.table字体大小
           risk.table.height = 0.25, # risk.table占图片比例
           surv.plot.height = 0.75, # 生存图占图片比例
           surv.median.line = "hv",  # 添加中位生存时间
           legend.labs = 
             c("PD-1", "PD-L1"),    # 改变图例标签
           font.legend = c(16, "plain"),      # 图例字体设置
           font.x = c(16, "plain", "black"),  # x轴标签字体设置
           font.y = c(16, "plain", "black"),  # y轴标签字体设置
           font.xtickslab = c(14, "plain", "black"),  # x轴刻度标签字体设置
           font.ytickslab = c(14, "plain", "black"),  # y轴刻度标签字体设置
           legend = c(0.8, 0.85), # 改变legend的位置为c(0.8, 0.85),也可以设为"top","right"
           palette = 
             c("#CD3333", "#483D8B") # 设置颜色  palette = "nejmhttp://127.0.0.1:38885/graphics/plot_zoom_png?width=1200&height=900", "grey","npg","aaas","lancet","jco", "ucscgb","uchicago","simpsons"和"rickandmorty"
)


###配色
####("#FF4040", "#483D8B")
###palette = "nejm", "grey","npg","aaas","lancet","jco", "ucscgb","uchicago","simpsons"和"rickandmorty"









##8####################药物类型的OS生存分析######################
library(openxlsx)
a = read.xlsx("二逼.xlsx")
library(survival)

library(survminer)


fit <- survfit(Surv(OS, 是否死亡 == 1) ~ 药物类型分组, data = a)

ggsurvplot(fit,
           pval = TRUE, # 在图上添加log rank检验的p值
           conf.int = TRUE,# 添加置信区间
           conf.int.style = "ribbon",  # 设置置信区间的风格?
           conf.int.alpha = c(0.3), # 生存曲线置信区间透明度
           xlab = "Time in months",   # 设置x轴标签
           ylab = "Overall Survival ", # y轴标签
           break.time.by = 3,     # 将x轴按照6为间隔进行切分
           xlim = c(0,30), # 自定义x轴（时间）显示范围
           censor.shape="|", censor.size = 2, # 删失线形状和大小
           risk.table = "abs_pct",  # 在风险表中添加绝对数和相对数
           risk.table.y.text.col = TRUE,# 设置风险表的文字颜色
           risk.table.y.text = FALSE, # risk.table y轴显示文字，FALSE则显示色块
           risk.table.title = "Number at risk", # risk.table标题
           risk.table.fontsize = 3, # risk.table字体大小
           risk.table.height = 0.25, # risk.table占图片比例
           surv.plot.height = 0.75, # 生存图占图片比例
           surv.median.line = "hv",  # 添加中位生存时间
           legend.labs = 
             c("PD-1", "PD-L1"),    # 改变图例标签
           font.legend = c(16, "plain"),      # 图例字体设置
           font.x = c(16, "plain", "black"),  # x轴标签字体设置
           font.y = c(16, "plain", "black"),  # y轴标签字体设置
           font.xtickslab = c(14, "plain", "black"),  # x轴刻度标签字体设置
           font.ytickslab = c(14, "plain", "black"),  # y轴刻度标签字体设置
           legend = c(0.8, 0.85), # 改变legend的位置为c(0.8, 0.85),也可以设为"top","right"
           palette = 
             c("#CD3333", "#483D8B") # 设置颜色  palette = "nejmhttp://127.0.0.1:38885/graphics/plot_zoom_png?width=1200&height=900", "grey","npg","aaas","lancet","jco", "ucscgb","uchicago","simpsons"和"rickandmorty"
)


###配色
####("#FF4040", "#483D8B")
###palette = "nejm", "grey","npg","aaas","lancet","jco", "ucscgb","uchicago","simpsons"和"rickandmorty"







