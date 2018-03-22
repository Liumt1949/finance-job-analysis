
####################################################################################
##                                                                                ## 
##                             Data Description                                   ##  
##                                                                                ## 
####################################################################################

rm(list=ls())   ## 清空工作环境
setwd("E:/financejob/financejob") #可自己设定文件路径
## package: readxl  用于读取Excel文件
## package: ggplot2 用于绘制各类图表
library(readxl)
library(ggplot2)

#读入csv格式的数据
job.info=read.csv("financejob.csv",header = T)

options(scipen = 200)  ## 去除科学计数法

## 图片颜色设置，利用rgb函数产生颜色，用于后续画图使用
col1 =rgb(255,230,0,maxColorValue = 255) #某种黄色
col2 =rgb(60,97,159,maxColorValue = 255) #某种蓝色
col3 = rgb(32,40,51,maxColorValue = 255)   ##某一种黑色

str(job.info)  ## 查看数据结构
job.info$lowsalary = as.numeric(job.info$lowsalary)  ## 将最低薪资的字符型变量改为数值型变量
job.info$highsalary = as.numeric(job.info$highsalary)  ## 将最高薪资的字符型变量改为数值型变量
job.info$对数平均薪资=as.numeric(job.info$对数平均薪资) ## 将对数平均薪资的字符型变量改为数值型变量


############ 行业类别分析
## 从jobinfo中取出行业类别
hangye = job.info$sector
## 查看行业类别
table(hangye)
## 统计各个行业的频率，返回格式为data.frame
hangye=as.data.frame(ftable(hangye))
hangye=hangye[order(hangye$Freq,decreasing=T),]
hangye$hangye=factor(hangye$hangye,levels=c("基金/证券/期货/投资","保险","信托/担保/拍卖/典当","银行"))

##绘制柱状图
col= c(col2,col2,col2,col2)

##### mac代码段
ggplot(data=hangye,aes(x=hangye,y=Freq)) + geom_bar(stat ='identity',fill= col)+
  annotate("text",x=1:4,y=c(hangye$Freq[1:4]+800),label=hangye$Freq) +
  theme(text =element_text(family = "STKaiti",size =11))+
  labs(x="行业大类",y="频数")

##### win代码段
ggplot(data=hangye,aes(x=hangye,y=Freq)) + geom_bar(stat ='identity',fill= col)+
  annotate("text",x=1:4,y=c(hangye$Freq[1:4]+800),label=hangye$Freq) +
  theme(text =element_text(size =11))+
  labs(x="行业大类",y="频数")



#### 薪资整体情况
## 利用ggplot2中的ggplot画出对数平均薪资直方图
##### mac代码段
ggplot(data = job.info,aes(x=对数平均薪资)) +
  geom_histogram(binwidth = 0.5,fill=col2,colour=col3) +
  labs(y="频数") +
  theme(text = element_text(family = "STKaiti", size = 12))
##### win代码段
ggplot(data = job.info,aes(x=对数平均薪资)) +
  geom_histogram(binwidth = 0.5,fill=col2,colour=col3) +
  labs(y="频数")+
  theme(text = element_text( size = 12))



################# 薪资影响因素分析

################# 行业类别对薪资的影响
## 求出行业的对数平均薪资的中位数
sector = unique(job.info$sector)
sector
table(job.info$sector)
baoxian.med=median(job.info$对数平均薪资[job.info$sector=="保险"],na.rm=TRUE)
jijin.med=median(job.info$对数平均薪资[job.info$sector=="基金/证券/期货/投资"],na.rm=TRUE)
yinhang.med=median(job.info$对数平均薪资[job.info$sector=="银行"],na.rm=TRUE)
danbao.med=median(job.info$对数平均薪资[job.info$sector=="信托/担保/拍卖/典当"],na.rm=TRUE)
## median合并,这里的顺序保持与sector中的顺序一致
sector.med = c(jijin.med,yinhang.med,danbao.med,baoxian.med)
sector.med
## 按median从大到小排列
sector= sector[order(sector.med,decreasing=TRUE)]
#除了银行比较高之外，其他三个类别的中位数一样，再根据样本量调整位置
sector=c("银行","信托/担保/拍卖/典当","保险","基金/证券/期货/投资")
## 重新划分因子型向量
job.info$sector= factor(job.info$sector,levels = sector)
rm(sector,baoxian.med,jijin.med,danbao.med,yinhang.med,sector.med) #删除中间变量
## 利用箱线图画出，薪资vs公司类别的分布，箱体的宽度越宽表示样本量越多
col = c(col2,col2,col2,col2)
##### mac代码段
ggplot(job.info,aes(sector,对数平均薪资)) + 
  geom_boxplot(varwidth=TRUE, fill = col ) + 
  theme(text = element_text(family = "STKaiti",size =15))+
  labs(x="行业类别")
##### win代码段
ggplot(job.info,aes(sector,对数平均薪资)) + 
  geom_boxplot(varwidth = TRUE,fill = col )+ 
  theme(text = element_text(size =14))+
  labs(x="行业类别")



#################公司子行业对薪资的影响
table(job.info$subsector)
#因为类别为“其他”的数量太多，且不能提供特别信息，因此画箱线图时将之删除
subsector.info=job.info[job.info$subsector!="其他",]
## 将公司的具体子行业转化为因子型变量，便于画图
table(subsector.info$subsector)
#求中位数
subsector=unique(subsector.info$subsector)
subsector
jijin.med=median(subsector.info$对数平均薪资[subsector.info$subsector=="基金"],na.rm=TRUE)
zhengquan.med=median(subsector.info$对数平均薪资[subsector.info$subsector=="证券"],na.rm=TRUE)
zulin.med=median(subsector.info$对数平均薪资[subsector.info$subsector=="租赁"],na.rm=TRUE)
danbao.med=median(subsector.info$对数平均薪资[subsector.info$subsector=="担保"],na.rm=TRUE)
qihuo.med=median(subsector.info$对数平均薪资[subsector.info$subsector=="期货"],na.rm=TRUE)
xintuo.med=median(subsector.info$对数平均薪资[subsector.info$subsector=="信托"],na.rm=TRUE)
baoxian.med=median(subsector.info$对数平均薪资[subsector.info$subsector=="保险"],na.rm=TRUE)
yinhang.med=median(subsector.info$对数平均薪资[subsector.info$subsector=="银行"],na.rm=TRUE)
#合并中位数向量,顺序与subsector一样
subsector.med=c(zhengquan.med,jijin.med,zulin.med,qihuo.med,baoxian.med,yinhang.med,danbao.med,xintuo.med)
#按中位数排序
subsector=subsector[order(subsector.med,decreasing=TRUE)]
#按中位数排序划分因子向量
subsector.info$subsector=factor(subsector.info$subsector,levels=subsector)
table(subsector.info$subsector)
col=c(col1,col1,col1,col1,col2,col2,col2,col2)
##### mac代码段
ggplot(subsector.info,aes(subsector,对数平均薪资)) + 
  geom_boxplot(varwidth = TRUE, fill = col) + 
  theme(text = element_text(family = "STKaiti",size =15))+
  labs(x="主要子行业")
##### win代码段
ggplot(subsector.info,aes(subsector,对数平均薪资)) + 
  geom_boxplot(varwidth = TRUE, fill = col)+ 
  theme(text = element_text(size =15))+
  labs(x="主要子行业")
rm(subsector.info) #删除中间变量
rm(jijin.med,zhengquan.med,zulin.med,danbao.med,qihuo.med,xintuo.med,baoxian.med,yinhang.med)
rm(subsector.med,subsector)



#################nature 公司性质对薪资的影响
#求中位数
nature=unique(job.info$nature)
nature
guoqi.med=median(job.info$对数平均薪资[job.info$nature=="国企"],na.rm=TRUE)
hezi.med=median(job.info$对数平均薪资[job.info$nature=="合资"],na.rm=TRUE)
minying.med=median(job.info$对数平均薪资[job.info$nature=="民营"],na.rm=TRUE)
gufen.med=median(job.info$对数平均薪资[job.info$nature=="股份制企业"],na.rm=TRUE)
qita.med=median(job.info$对数平均薪资[job.info$nature=="其他"],na.rm=TRUE)
waishang.med=median(job.info$对数平均薪资[job.info$nature=="外商独资"],na.rm=TRUE)
shangshi.med=median(job.info$对数平均薪资[job.info$nature=="上市公司"],na.rm=TRUE)
#合并中位数向量，顺序保持与nature一致
nature.med=c(minying.med,hezi.med,gufen.med,guoqi.med,waishang.med,shangshi.med,qita.med)
nature.med
#按中位数排序
nature=nature[order(nature.med,decreasing=TRUE)]
nature
#因合资、股份制、国企、上市公司的中位数一样，但上市公司的最大值高，调一下顺序
nature = c("外商独资","其他","民营", "上市公司","合资", "股份制企业" ,"国企")
#按中位数排序划分因子向量
job.info$nature=factor(job.info$nature,levels=nature)
#删除中间变量
rm(nature,guoqi.med,hezi.med,minying.med,gufen.med,qita.med,waishang.med,shangshi.med,nature.med)
## 利用箱线图画图，箱体的宽度越宽表示样本量越多
col = c(col1,col1,col1,col1,col2,col2,col2)
##### mac代码段
ggplot(job.info,aes(nature,对数平均薪资)) + 
  geom_boxplot(varwidth = TRUE,fill = col ) + 
  labs(x="公司性质")+
  theme(text = element_text(family = "STKaiti", size = 15))
##### win代码段
ggplot(job.info,aes(nature,对数平均薪资)) + 
  geom_boxplot(varwidth = TRUE, fill = col)  + 
  labs(x="公司性质")+
  theme(text = element_text( size = 15))



#################公司规模对薪资的影响
## 利用箱线图画出，公司规模vs公司类别的分布，箱体的宽度越宽表示样本量越多
## 将公司规模转化为因子型变量，便于画图
table(job.info$size)
job.info$size=factor(job.info$size,levels=c("20人以下","20-99人","100-499人","500-999人","1000-9999人","10000人以上"))
col =c(col1,col1,col2,col2,col2,col1)
##### mac代码段
ggplot(job.info,aes(size,对数平均薪资)) + 
  geom_boxplot(varwidth = TRUE, fill =col) + 
  theme(text = element_text(family = "STKaiti",size =11))+
  labs(x="公司规模")
##### win代码段
ggplot(job.info,aes(size,对数平均薪资)) + 
  geom_boxplot(varwidth = TRUE, fill = col)+ 
  theme(text = element_text(size =11))+
  labs(x="公司规模")



#################重点岗位的薪资水平分析
impposition=unique(job.info$impposition)
impposition
#生成中位数
qita.med=median(job.info$对数平均薪资[job.info$impposition=="其他"],na.rm=TRUE)
touhang.med=median(job.info$对数平均薪资[job.info$impposition=="投行岗"],na.rm=TRUE)
xiangmu.med=median(job.info$对数平均薪资[job.info$impposition=="项目投资岗"],na.rm=TRUE)
xiaoshou.med=median(job.info$对数平均薪资[job.info$impposition=="销售岗"],na.rm=TRUE)
yanjiu.med=median(job.info$对数平均薪资[job.info$impposition=="研究岗"],na.rm=TRUE)
fengkong.med=median(job.info$对数平均薪资[job.info$impposition=="风控岗"],na.rm=TRUE)
jiaoyi.med=median(job.info$对数平均薪资[job.info$impposition=="交易岗"],na.rm=TRUE)
#合并中位数,顺序与impposition一样
impposition.med=c(touhang.med,qita.med,xiangmu.med,xiaoshou.med,yanjiu.med,fengkong.med,jiaoyi.med)
#按中位数排序
impposition=impposition[order(impposition.med,decreasing=TRUE)]
#将重点岗位转换成因子型变量，便于画图
job.info$impposition=factor(job.info$impposition, levels=impposition)
col=c(col1,col1,col1,col2,col2,col2,col2)

##### mac代码段
ggplot(job.info,aes(impposition,对数平均薪资)) + 
  geom_boxplot(varwidth = TRUE, fill =col) + 
  theme(text = element_text(family = "STKaiti",size =11.5))+
  labs(x="重点岗位")

##### win代码段
ggplot(job.info,aes(impposition,对数平均薪资)) + 
  geom_boxplot(varwidth = TRUE, fill = col)+ 
  theme(text = element_text(size =11.5))+
  labs(x="重点岗位")



################# 学历对薪资的影响
## 利用箱线图画出，学历vs公司类别的分布，箱体的宽度越宽表示样本量越多
## 将学历转化为因子型变量，便于画图
table(job.info$education)
job.info$education= factor(job.info$education,levels=c("不限","大专","本科","硕士及以上"))
col= c(col2,col2,col1,col1)
##### mac代码段
ggplot(job.info,aes(education,对数平均薪资)) + 
  geom_boxplot(varwidth = TRUE,fill = col) + 
  labs(x="学历要求")+
  theme(text = element_text(family = "STKaiti", size = 15))
##### win代码段
ggplot(job.info,aes(education,对数平均薪资)) + 
  geom_boxplot(varwidth = TRUE, fill = col)  + 
  labs(x="学历要求")+
  theme(text = element_text(size = 15))



################# 经验对薪资的影响

## 将经验区间转化为因子型变量，便于画图
job.info$experinter = factor(job.info$experinter,levels=c("无经验","1-2年","3-4年","5-6年","7年及以上"))
table(job.info$experinter)#“7-8"年只有3条记录
## 利用箱线图画出，经验区间vs公司类别的分布，箱体的宽度越宽表示样本量越多
col = c(col2,col2,col1,col1,col1)
##### mac代码段
ggplot(job.info,aes(experinter,对数平均薪资)) + 
  geom_boxplot(varwidth = TRUE, fill = col) + 
  labs(x="工作经验要求") +
  theme(text = element_text(family = "STKaiti",size = 15))
##### win代码段
ggplot(job.info,aes(experinter,对数平均薪资)) + 
  geom_boxplot(varwidth = TRUE, fill = col) + 
  labs(x="工作经验要求")
theme(text = element_text(size = 15))



