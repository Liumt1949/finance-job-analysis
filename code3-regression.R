
####################################################################################
##                                                                                ## 
##                             Regression Analysis                                ##  
##                                                                                ## 
####################################################################################

rm(list=ls())   ## 清空工作环境
setwd("E:/financejob/financejob") 
#若更改为自己的工作路径，则下文的读取和保存路径要相应更改

library(ggplot2)

#读入csv格式的数据
job.info=read.csv("financejob.csv",header = T)

colnames(job.info)
#生成一个回归用的dataframe,去掉ID变量、原始的一些变量如financeid,position,category, sector, 
#salary, education, experience, descrip, company, nature, size 
#以及一些衍生变量如subsector, lowsalary, highsalary, avesalary, experience2, 经验区间, impposition
newjob=job.info[,-c(1:11,20:23,25,26,33)]#
colnames(newjob)


## 图片颜色设置，利用rgb函数产生颜色，用于后续画图使用
col1 =rgb(255,230,0,maxColorValue = 255) #某种黄色
col2 =rgb(150,150,150,maxColorValue = 255) #某种浅灰色
col3 =rgb(60,97,159,maxColorValue = 255) #某种蓝色


############ 回归分析

lm.fit = lm(对数平均薪资~.,data=newjob)

## 查看回归结果
summary(lm.fit)
coef=data.frame(coef(lm.fit))

#生成显著性水平的star
p=summary(lm.fit)$coefficients[,4] 
star3 = ifelse(p<=0.001,"***","")
star2 = ifelse(p>0.001 & p<=0.01,"**","") 
star1 = ifelse(p>0.01 & p<=0.05,"*","")
star0 = ifelse(p>0.05 & p<=0.1,".","")

star = rep("",length(p))
star[which(star3=="***")]="***"
star[which(star2=="**")]="**"
star[which(star1=="*")]="*"
star[which(star0==".")]="."
star

significance = paste(as.character(round(coef(lm.fit),3)),star,sep = "")
significance 
coef$significance=significance
#write.csv(coef,file="coef.csv")

rm(p,star3,star2,star1,star0,star) #删除中间变量


##############################################################################
#                                                                            #
#                             回归系数可视化                                 #
#                                                                            #     
#############################################################################
## 将各类回归系数放到同一个dataframe,以公司性质为例，将公司性质系数用coef取出，然后放入nature.coef中；
## 取出nature.coef的行名，作为变量“公司性质”,给列命名并去除行名

#######公司子行业dataframe
subsector.coef = data.frame(coef(lm.fit)[2:9],significance[2:9])
subsector.coef$公司子行业 = c("银行","保险", "基金", "证券","期货","信托","租赁","担保") 
subsector.coef$公司子行业 = factor(subsector.coef$公司子行业,
                              levels = subsector.coef$公司子行业[order(subsector.coef$coef.lm.fit..2.9.)])
colnames(subsector.coef) = c("系数","显著性水平","公司子行业")
row.names(subsector.coef) = NULL
subsector.coef = subsector.coef[order(subsector.coef$系数),]

## 公司子行业可视化，画出公司子行业的回归系数直方图
col=c(col3,col3,col3,col3,col2,col2,col2,col1)
##### mac代码段
ggplot(data=subsector.coef, aes(x=公司子行业, y=系数)) +
  geom_bar(fill=col,stat="identity",width = 0.6) +
  annotate("text",x=1:8,y=c(subsector.coef$系数[1:4]-0.03,subsector.coef$系数[5:8]+0.03),label=subsector.coef$显著性水平) +
  theme(text = element_text(family = "STKaiti", size = 11.5))
##### win代码段
ggplot(data=subsector.coef, aes(x=公司子行业, y=系数)) +
  geom_bar(fill=col,stat="identity",width = 0.6) +
  annotate("text",x=1:8,y=c(subsector.coef$系数[1:4]-0.03,subsector.coef$系数[5:8]+0.03),label=subsector.coef$显著性水平) +
  theme(text = element_text(size = 11.5))



#######公司性质dataframe
nature.coef = data.frame(coef(lm.fit)[22:27],significance[22:27])
nature.coef$公司性质 = factor(row.names(nature.coef),
                      levels = row.names(nature.coef)[order(nature.coef$coef.lm.fit..22.27.)])

colnames(nature.coef) = c("系数","显著性水平","公司性质")
nature.coef = nature.coef[order(nature.coef$系数),]
row.names(nature.coef) = NULL

## 公司性质系数可视化，画出公司性质的回归系数直方图
col=c(col3,col2,col2,col2,col1,col1)
##### mac代码段
ggplot(data=nature.coef, aes(x=公司性质, y=系数)) +
  geom_bar(fill=col,stat="identity",width = 0.6) +
  annotate("text",x=1:6,y=c(nature.coef$系数[1:2]-0.007,nature.coef$系数[3:6]+0.007),label=nature.coef$显著性水平) +
  theme(text = element_text(family = "STKaiti", size = 11))
##### win代码段
ggplot(data=nature.coef, aes(x=公司性质, y=系数)) +
  geom_bar(fill=col,stat="identity",width = 0.6) +
  annotate("text",x=1:6,y=c(nature.coef$系数[1:2]-0.007,nature.coef$系数[3:6]+0.007),label=nature.coef$显著性水平) +
  theme(text = element_text(size = 11))



#######公司规模dataframe
size.coef = data.frame(coef(lm.fit)[17:21],significance[17:21])
size.coef$公司规模 = c("20人以下","20-99人","100-499人","1000-9999人","10000人以上")
size.coef$公司规模 = factor(size.coef$公司规模,levels = c("20人以下","20-99人","100-499人","1000-9999人","10000人以上"))
colnames(size.coef) = c("系数","显著性水平","公司规模")
row.names(size.coef) = NULL

## 公司规模系数可视化，画出公司规模的回归系数直方图
col=c(col3,col3,col2,col2,col1)
##### mac代码段
ggplot(data=size.coef, aes(x=公司规模, y=系数)) +
  geom_bar(fill=col,stat="identity",width = 0.6) +
  annotate("text",x=1:5,y=c(size.coef$系数[1]+0.007,size.coef$系数[2:4]-0.007,size.coef$系数[5]+0.007),label=size.coef$显著性水平) +
  theme(text = element_text(family = "STKaiti", size = 11))
##### win代码段
ggplot(data=size.coef, aes(x=公司规模, y=系数)) +
  geom_bar(fill=col,stat="identity",width = 0.6) +
  annotate("text",x=1:5,y=c(size.coef$系数[1]+0.007,size.coef$系数[2:4]-0.007,size.coef$系数[5]+0.007),label=size.coef$显著性水平) +
  theme(text = element_text(size = 11))


#######重点岗位dataframe
#为前文中的重点岗位
imppos.coef = data.frame(coef(lm.fit)[10:15],significance[10:15])
imppos.coef$重点岗位 = c("研究岗", "交易岗", "风控岗","投行岗","项目投资岗","销售岗") 
imppos.coef$重点岗位 = factor(imppos.coef$重点岗位,
                          levels = imppos.coef$重点岗位[order(imppos.coef$coef.lm.fit..10.15.)])
colnames(imppos.coef) = c("系数","显著性水平","重点岗位")
imppos.coef = imppos.coef[order(imppos.coef$系数),]
row.names(imppos.coef) = NULL

## 重点岗位可视化，画出重点岗位的回归系数直方图
col=c(col2,col2,col1,col1,col1,col1)
##### mac代码段
ggplot(data=imppos.coef, aes(x=重点岗位, y=系数)) +
  geom_bar(fill=col,stat="identity",width = 0.6) +
  annotate("text",x=1:6,y=c(imppos.coef$系数[1]+0.01,imppos.coef$系数[2:6]+0.01),label=imppos.coef$显著性水平) +
  theme(text = element_text(family = "STKaiti", size = 11.5))
##### win代码段
ggplot(data=imppos.coef, aes(x=重点岗位, y=系数)) +
  geom_bar(fill=col,stat="identity",width = 0.6) +
  annotate("text",x=1:6,y=c(imppos.coef$系数[1]+0.01,imppos.coef$系数[2:6]+0.01),label=imppos.coef$显著性水平) +
  theme(text = element_text(size = 11.5))



#######学历dataframe
edu.coef = data.frame(coef(lm.fit)[28:30],significance[28:30])
edu.coef$学历 = factor(row.names(edu.coef),
                     levels = row.names(edu.coef))
colnames(edu.coef) = c("系数","显著性水平","学历")
row.names(edu.coef) = NULL

## 学历系数可视化，画出学历的回归系数直方图
col=c(col3,col3,col1)
##### mac代码段
ggplot(data=edu.coef, aes(x=学历, y=系数)) +
  geom_bar(fill=col,stat="identity",width = 0.6) +
  annotate("text",x=1:3,y=c(edu.coef$系数[1:2]-0.02,edu.coef$系数[3]+0.02),label=edu.coef$显著性水平) +
  labs(x="学历要求")+
  theme(text = element_text(family = "STKaiti", size = 15))
##### win代码段
ggplot(data=edu.coef, aes(x=学历, y=系数)) +
  geom_bar(fill=col,stat="identity",width = 0.6) +
  annotate("text",x=1:3,y=c(edu.coef$系数[1:2]-0.02,edu.coef$系数[3]+0.02),label=edu.coef$显著性水平) +
  labs(x="学历要求")+
  theme(text = element_text(size = 15))



#######工作经验dataframe
exper.coef = data.frame(coef(lm.fit)[31:34],significance[31:34])
exper.coef$工作经验 = c("1-2年","3-4年","5-6年","7年及以上")
exper.coef$工作经验 = factor(exper.coef$工作经验,
                          levels = exper.coef$工作经验[order(exper.coef$coef.lm.fit..31.34.)])

colnames(exper.coef) = c("系数","显著性水平","工作经验")
exper.coef = exper.coef[order(exper.coef$系数),]
row.names(exper.coef) = NULL

## 工作经验系数可视化，画出工作经验的回归系数直方图
col=c(col2,col1,col1,col1)
##### mac代码段
ggplot(data=exper.coef, aes(x=工作经验, y=系数)) +
  geom_bar(fill=col,stat="identity",width = 0.6) +
  annotate("text",x=1:4,y=c(exper.coef$系数[1]-0.1,exper.coef$系数[2:4]+0.1),label=exper.coef$显著性水平) +
  theme(text = element_text(family = "STKaiti", size = 12))
##### win代码段
ggplot(data=exper.coef, aes(x=工作经验, y=系数)) +
  geom_bar(fill=col,stat="identity",width = 0.6) +
  annotate("text",x=1:4,y=c(exper.coef$系数[1]-0.1,exper.coef$系数[2:4]+0.1),label=exper.coef$显著性水平) +
  theme(text = element_text(size = 12))



######证书dataframe
zhengshu.coef=data.frame(coef(lm.fit)[35:37],significance[35:37])
zhengshu.coef=zhengshu.coef[c(3,2,1),]
zhengshu.coef$证书=c("从业资格","司考","CPA")
zhengshu.coef$证书=factor(zhengshu.coef$证书,levels=c("从业资格","司考","CPA"))
colnames(zhengshu.coef)=c("系数","显著性水平","证书")
row.names(zhengshu.coef)=NULL
## 系数可视化，画出回归系数直方图
col=c(col3,col2,col2)
##### mac代码段
ggplot(data=zhengshu.coef, aes(x=证书, y=系数)) +
  geom_bar(fill=col,stat="identity",width = 0.6) +
  annotate("text",x=1:3,y=c(zhengshu.coef$系数[1]-0.012,zhengshu.coef$系数[2:3]+0.012),label=zhengshu.coef$显著性水平) +
  theme(text = element_text(family = "STKaiti", size = 12))
####win代码段
ggplot(data=zhengshu.coef, aes(x=证书, y=系数)) +
  geom_bar(fill=col,stat="identity",width = 0.6) +
  annotate("text",x=1:3,y=c(zhengshu.coef$系数[1]-0.012,zhengshu.coef$系数[2:3]+0.012),label=zhengshu.coef$显著性水平) +
  theme(text = element_text(size = 12))



######编程 dataframe
biancheng.coef=data.frame(coef(lm.fit)[38:39],significance[38:39])
biancheng.coef=biancheng.coef[order(biancheng.coef$coef.lm.fit..38.39),]
biancheng.coef$编程=c("PythonSQL等","R")
biancheng.coef$编程=factor(biancheng.coef$编程,levels=c("PythonSQL等","R"))
colnames(biancheng.coef)=c("系数","显著性水平","编程")
row.names(biancheng.coef)=NULL
## 系数可视化，画出回归系数直方图
col=c(col2,col1)
##### mac代码段
ggplot(data=biancheng.coef, aes(x=编程, y=系数)) +
    geom_bar(fill=col,stat="identity",width = 0.6) +
    annotate("text",x=1:2,y=c(biancheng.coef$系数[1:2]+0.012),label=biancheng.coef$显著性水平) +
    theme(text = element_text(family = "STKaiti", size = 12))
####win代码段
ggplot(data=biancheng.coef, aes(x=编程, y=系数)) +
  geom_bar(fill=col,stat="identity",width = 0.6) +
  annotate("text",x=1:2,y=c(biancheng.coef$系数[1:2]+0.012),label=biancheng.coef$显著性水平) +
  theme(text = element_text(size = 12))

table(job.info$R)
table(job.info$SPM)

######特质dataframe
tezhi.coef=data.frame(coef(lm.fit)[40:43],significance[40:43])
tezhi.coef = tezhi.coef[order(tezhi.coef$coef.lm.fit..40.43.),]
tezhi.coef$特质=c("学习能力","知识","责任心","团队意识")
tezhi.coef$特质=factor(tezhi.coef$特质,levels=c("学习能力","知识","责任心","团队意识"))
colnames(tezhi.coef)=c("系数","显著性水平","特质")
row.names(tezhi.coef)=NULL

## 系数可视化，画出回归系数直方图
col=c(col2,col2,col1,col1)
##### mac代码段
ggplot(data=tezhi.coef, aes(x=特质, y=系数)) +
  geom_bar(fill=col,stat="identity",width = 0.6) +
  annotate("text",x=1:4,y=c(tezhi.coef$系数[1]+0.007,tezhi.coef$系数[2]+0.007,tezhi.coef$系数[3]+0.007,tezhi.coef$系数[4]+0.007),label=tezhi.coef$显著性水平) +
  theme(text = element_text(family = "STKaiti", size = 12))
####win代码段
ggplot(data=tezhi.coef, aes(x=特质, y=系数)) +
  geom_bar(fill=col,stat="identity",width = 0.6) +
  annotate("text",x=1:4,y=c(tezhi.coef$系数[1]+0.007,tezhi.coef$系数[2]+0.007,tezhi.coef$系数[3]+0.007,tezhi.coef$系数[4]+0.007),label=tezhi.coef$显著性水平) +
  theme(text = element_text(size = 12))

table(job.info$fuze)
table(job.info$tuandui)




##############################################################################
#                                                                            #
#                            本科生技能分析                                  #
#                                                                            #     
#############################################################################
####### 本科基准：应届生，技能一般，应聘一家中型民营证券公司的风控员
xinren0 = newjob[1,] #为了使新生成的data.frame结构与原有的相同,取原有数据中一行
xinren0= xinren0[,-9] #去除因变量对数平均薪资这一列
colnames(xinren0)
xinren0[,c(1:15)] = FALSE #初始化设置
xinren0[,c(16:33)] = 0 #初始化设置
xinren0[,c(34:42)] = FALSE #初始化设置

xinren0$security=TRUE
xinren0$risker=TRUE
xinren0$minying=1
xinren0$本科=1
interval.xinren0 = predict(lm.fit,xinren0,interval="confidence")  ## 区间估计
income.xinren0 = exp(interval.xinren0)   ##将预测的对数薪资转化为实际薪资
income.xinren0=as.data.frame(income.xinren0)
rownames(income.xinren0)=("xinren.jizhun")

benke=income.xinren0



####### 基准+勤奋努力+销售技能较强：应聘一家中型民营银行的销售岗位
xinren0 = newjob[1,] #为了使新生成的data.frame结构与原有的相同,取原有数据中一行
xinren0= xinren0[,-9] #去除因变量对数平均薪资这一列
colnames(xinren0)
xinren0[,c(1:15)] = FALSE #初始化设置
xinren0[,c(16:33)] = 0 #初始化设置
xinren0[,c(34:42)] = FALSE #初始化设置

xinren0$bank=TRUE
xinren0$minying=1
xinren0$本科=1
xinren0$seller=TRUE
interval.xinren0 = predict(lm.fit,xinren0,interval="confidence")  ## 区间估计
income.xinren0 = exp(interval.xinren0)   ##将预测的对数薪资转化为实际薪资
income.xinren0=as.data.frame(income.xinren0)
rownames(income.xinren0)=("xinren.seller")

benke[2,]=income.xinren0[1,]



####### 基准+基础知识扎实+学习能力强+熟练使用R：应聘一家中型合资基金公司的固定收益类岗位

xinren0 = newjob[1,] #为了使新生成的data.frame结构与原有的相同,取原有数据中一行
xinren0= xinren0[,-9] #去除因变量对数平均薪资这一列
colnames(xinren0)
xinren0[,c(1:15)] = FALSE #初始化设置
xinren0[,c(16:33)] = 0 #初始化设置
xinren0[,c(34:42)] = FALSE #初始化设置

xinren0$fund=TRUE
xinren0$minying=1
xinren0$本科=1
xinren0$bond=TRUE
xinren0$researcher=TRUE
xinren0$"合资"=1
xinren0$R=TRUE
xinren0$xuexi=TRUE
xinren0$zhishi=TRUE
interval.xinren0 = predict(lm.fit,xinren0,interval="confidence")  ## 区间估计
income.xinren0 = exp(interval.xinren0)   ##将预测的对数薪资转化为实际薪资
income.xinren0=as.data.frame(income.xinren0)
rownames(income.xinren0)=("xinren.bond")

benke[3,]=income.xinren0[1,]



####### 基准+责任心强+团队合作能力强+有CPA证书：应聘一家大型上市Y银行公司金融市场部的资产支持证券项目投资岗

xinren0 = newjob[1,] #为了使新生成的data.frame结构与原有的相同,取原有数据中一行
xinren0= xinren0[,-9] #去除因变量对数平均薪资这一列
colnames(xinren0)
xinren0[,c(1:15)] = FALSE #初始化设置
xinren0[,c(16:33)] = 0 #初始化设置
xinren0[,c(34:42)] = FALSE #初始化设置

xinren0$Band=TRUE
xinren0$projecter=TRUE
xinren0$"10000人及以上"=1
xinren0$"上市公司"=1
xinren0$CPA=TRUE
xinren0$fuze=TRUE
xinren0$tuandui=TRUE
xinren0$本科=1
xinren0$bond=TRUE

interval.xinren0 = predict(lm.fit,xinren0,interval="confidence")  ## 区间估计
income.xinren0 = exp(interval.xinren0)   ##将预测的对数薪资转化为实际薪资
income.xinren0=as.data.frame(income.xinren0)
rownames(income.xinren0)=("xinren.projecter")

benke[4,]=income.xinren0[1,]



###本科应届生4种不同薪资可视化
colnames(benke)=c("薪资点预测","low","high")
benke$个案分析=c("风控员","客户经理","固收研究员","项目投资员")
benke=benke[,c(1,4)]
row.names(benke) = NULL
benke=benke[order(benke$薪资点预测),]
benke$个案分析 = factor(c("风控员","客户经理","固收研究员","项目投资员"),
                          levels = c("风控员","客户经理","固收研究员","项目投资员"))

###可视化
col=c(col2,col1,col1,col1)
##### mac代码段
ggplot(data=benke, aes(x=个案分析, y=薪资点预测)) +
  geom_bar(fill=col,stat="identity",width = 0.6) +
  annotate("text",x=1:4,y=c(benke$薪资点预测[1:4]+1000),label=round(benke$薪资点预测),0) +
  theme(text = element_text(family = "STKaiti", size = 11.5))
##### win代码段
ggplot(data=benke, aes(x=个案分析, y=薪资点预测)) +
  geom_bar(fill=col,stat="identity",width = 0.6) +
  annotate("text",x=1:4,y=c(benke$薪资点预测[1:4]+1000),label=round(benke$薪资点预测),0) +
  theme(text = element_text(size = 11.5))




##############################################################################
#                                                                            #
#                            应聘者画像与薪资定制                            #
#                                                                            #     
#############################################################################

####### 预测：新人1：本科毕业 无工作经验 基础知识一般
#受聘于规模为400人的期货公司 该公司为非上市国企 应聘业务经纪人
## 创建一个名为xinren1的data frame (职场新人)
#职场新人1指标向量
xinren1 = newjob[1,] #为了使新生成的data.frame结构与原有的相同,取原有数据中一行
xinren1= xinren1[,-9] #去除因变量对数平均薪资这一列
colnames(xinren1)
xinren1[,c(1:15)] = FALSE #初始化设置
xinren1[,c(16:33)] = 0 #初始化设置
xinren1[,c(34:42)] = FALSE #初始化设置


#在期货公司
xinren1$futures=T
#经纪人
xinren1$seller=T
#400人公司
xinren1$"100-499人"=1
#非上市国企
xinren1$国企=1
#本科毕业
xinren1$本科=1
#无工作经验
#基础知识一般
xinren1$zhishi=T

interval.xinren1 = predict(lm.fit,xinren1,interval="confidence")  ## 区间估计
income.xinren1 = exp(interval.xinren1)   ##将预测的对数薪资转化为实际薪资
income.xinren1



####### 预测：新人2：硕士毕业 基础知识扎实 r熟练 学习能力较强 团队意识强
#受聘于规模为500人的基金公司 该公司为非上市民营企业 应聘研究助理
## 创建一个名为xinren2的data frame (职场新人)
#职场新人2指标向量
xinren2 = newjob[1,] #为了使新生成的data.frame结构与原有的相同,取原有数据中一行
xinren2= xinren2[,-9] #去除因变量对数平均薪资这一列
colnames(xinren2)
xinren2[,c(1:15)] = FALSE #初始化设置
xinren2[,c(16:33)] = 0 #初始化设置
xinren2[,c(34:42)] = FALSE #初始化设置



#在基金公司
xinren2$fund=T
#研究岗
xinren2$researcher=T
#非上市民营企业
xinren2$民营=1
#公司500-999人
#本科毕业
xinren2$硕士及以上=1
#无工作经验
#基础知识扎实
xinren2$zhishi=T
#R熟练
xinren2$R=T
#团队意识强
xinren2$tuandui=T
#学习能力强
xinren2$xuexi=T


interval.xinren2 = predict(lm.fit,xinren2,interval="confidence")  ## 区间估计
income.xinren2 = exp(interval.xinren2)   ##将预测的对数薪资转化为实际薪资
income.xinren2



####### 预测：达人1：本科毕业 3年工作经验 专业知识熟练 专业技能较强 团队意识强
#受聘于某大型上市证券公司 该公司员工超过10000人 从事投行项目投资
## 创建一个名为daren1的data frame (职场达人)
#职场达人指标向量
daren1= newjob[1,] #为了使新生成的data.frame结构与原有的相同,取原有数据中一行
daren1=daren1[,-9] #去除因变量对数平均薪资这一列
colnames(daren1)
daren1[,c(1:15)] = FALSE #初始化设置
daren1[,c(16:33)] = 0 #初始化设置
daren1[,c(34:42)] = FALSE #初始化设置



#在证券公司
daren1$security= T
#投行业务
daren1$invester= T
#项目投资
daren1$projecter=T
#公司10000人以上
daren1$"10000人以上"=1
#公司是上市公司
daren1$"上市公司"=1
#本科毕业
daren1$"本科"=1
#3年工作经验
daren1$"3-4年"=1
#专业知识熟练
daren1$zhishi=T
#团队意识强
daren1$tuandui=T

interval.daren1 = predict(lm.fit,daren1,interval="confidence")  ## 区间估计
income.daren1 = exp(interval.daren1)   ##将预测的对数薪资转化为实际薪资
income.daren1


####### 预测：达人2：硕士毕业 8年工作经验 学习能力强  专业知识深厚 团队意识很强
#某合资基金供职 该基金员工约900人 是债券投资的项目经理
## 创建一个名为daren2的data frame (职场达人)
#职场达人指标向量
daren2= newjob[1,] #为了使新生成的data.frame结构与原有的相同,取原有数据中一行
daren2=daren2[,-9] #去除因变量对数平均薪资这一列
colnames(daren2)
daren2[,c(1:15)] = FALSE #初始化设置
daren2[,c(16:33)] = 0 #初始化设置
daren2[,c(34:42)] = FALSE #初始化设置


#在基金公司
daren2$fund= T
#债券业务
daren2$bond= T
#项目经理
daren2$projecter=T
#公司500-999人
#公司是合资公司
daren2$"合资"=1
#硕士毕业
daren2$"硕士及以上"=1
#8年工作经验
daren2$"7年及以上"=1
#学习能力强
daren2$xuexi=T
#专业知识
daren2$zhishi=T
#团队合作
daren2$tuandui=T


interval.daren2 = predict(lm.fit,daren2,interval="confidence")  ## 区间估计
income.daren2 = exp(interval.daren2)   ##将预测的对数薪资转化为实际薪资
income.daren2
