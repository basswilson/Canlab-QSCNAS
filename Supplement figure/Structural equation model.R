
# 加载包，读取数据
library(lavaan) #加载包
dat<-read.csv("SEM.csv",row.names = 1,header=T)#读取文件


# 转换数据格式
##scale to the same variance
dat<-data.frame(scale(dat,center = F))
pairs(dat)


# 设置模型

model <- '

MLSS~F.M+Annual.average.T+Mixed.T+DO
F.M~Inf.BOD
SRT~a.AHLs.QQ
Actual.Inf.rate~a.AHLs.QQ+a.AHLs.QS
Inf.BOD~a.AHLs.QQ+a.AHLs.QS+a.AHLs.QSC
BOD.COD~a.AHLs.QQ+a.AHLs.QS+a.AHLs.QSC+a.Total.QQ
Inf.NH4.N~a.AHLs.QQ+a.AHLs.QS+a.AHLs.QSC+a.Total.QQ+a.Total.QS


SRT~PC1.AHLs.QQ
Actual.Inf.rate~PC1.AHLs.QQ+PC1.AHLs.QS
Inf.BOD~PC1.AHLs.QQ+PC1.AHLs.QS+PC1.AHLs.QSC
BOD.COD~PC1.AHLs.QQ+PC1.AHLs.QS+PC1.AHLs.QSC+PC1.Total.QQ
Inf.NH4.N~PC1.AHLs.QQ+PC1.AHLs.QS+PC1.AHLs.QSC+PC1.Total.QQ+PC1.Total.QS

'


#开始计算SEM
abioticCompFit <- sem(model, missing="ml",data=dat, fixed.x = FALSE)

#统计计算结果
summary(abioticCompFit, rsquare=T, standardized=T, fit.measures=TRUE)

# 查看残余变量
residuals(abioticCompFit, type="cor")

# 查看修正指数
modificationIndices(abioticCompFit,standardized=F)

#️️⭐️⭐️但是要注意有时候依据修正指数添加后，会提示模型不收敛“lavaan WARNING: model did not converge”
#️️⭐️⭐️这个时候就不要添加这个修正指数所提示的回归模型了。
#️️⭐️⭐️所以为了避免将来倘若出现不收敛的情况，建议根据修正指数，一个一个添加模型。若同时添加，提示收敛，这时候不知道是哪个修正指数的模型引起了不收敛。



#⭐查看拟合效果，判断是否能用️⭐️⭐️⭐️⭐️⭐️⭐️⭐️⭐️##
fitMeasures(abioticCompFit,c("chisq","df","pvalue","gfi","cfi","rmr","srmr","rmsea"))
#具体数值拟合的效果。具体标准请查看文件夹中的《检验的接受标准.PNG》文件
