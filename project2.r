.libPaths()

# create a variable for the library paths
# autocomplete press tab
lib_path = 'C:/Users/james/OneDrive/Documents/R/win-library/4.0'

# install libraries
install.packages('ggplot2', lib = lib_path)
install.packages('lubridate', lib = lib_path)
install.packages('GGally', lib = lib_path)
install.packages("ggpubr",lib = lib_path)
install.packages("qpcR",lib = lib_path)
install.packages("matlib",lib = lib_path)
install.packages("car",lib = lib_path)
install.packages("dplyr ",lib = lib_path) 
install.packages("ggrepel ",lib = lib_path)
install.packages("latex2exp",lib = lib_path)
install.packages("graphics",lib = lib_path)
install.packages("knitr",lib = lib_path)
install.packages("cowplot",lib = lib_path)
install.packages("gridExtra",lib = lib_path)
install.packages("patchwork",lib = lib_path)
install.packages("corrplot",lib = lib_path)
install.packages("faraway",lib = lib_path)
install.packages("ggthemes",lib = lib_path)
install.packages("lmtest",lib = lib_path)

# load libraries
library('ggplot2')
library('lubridate')
library('GGally')
library('ggpubr')
library('qpcR')
library('matlib')
library('car')
library('dplyr')
library('ggrepel')
library('latex2exp')
library('graphics')
library('rgl')
library('cowplot')
library('gridExtra')
library('patchwork')
library('corrplot')
library('faraway')
library('ggthemes')
library('lmtest')

# function to find all adjusted r^squared #

### input are strings ###
combination_function = function(dependent_var,independent_vars_vector,data_frame){
    
    # empty list
    equations = list()
    
    for(i in 1:length(independent_vars_vector)){
        
        vector_com = combn(independent_vars_vector,i)
        for (j in 1:ncol(vector_com)){
            
            model_f = as.formula(paste0(dependent_var,"~",paste0(vector_com[,j],collapse = "+")))
            equations = c(equations, model_f)        
    }    
}
    equation_output = NULL
    
    for(k in 1:length(equations)){
        
        equation = lm( equations[[k]], data= data_frame)
        terms = length(equation$coefficients)
        independent_var = c()
        
        independent_var[1] = paste0(dependent_var," = (",round(as.numeric(equation$coefficients[1]),4),") + ")
        
        for(i in 2:terms){
            
            independent_var[i] = paste0("(",round(as.numeric(equation$coefficients[i]),4),") ",names(equation$coefficients[i]))
            
        }
        
        eq = paste(independent_var,collapse = "")
        adj_rsquare = summary(equation)$adj.r.squared
        equation_df = data.frame("Equation"=eq, "adjusted r^2"=adj_rsquare)
        equation_output = rbind(equation_output,equation_df)   
        
    }
    
    equation_output = equation_output[order(-equation_output$adjusted.r.2),]
    return(equation_output)
    
    }   



# upload data

firm_profit_df = read.csv('2F - FirmProfit.csv', header=TRUE)
head(firm_profit_df,5)

#####################################################################
######################## PE boxplot #################################
pe.box = ggplot(data=firm_profit_df, aes(x=PE)) +

geom_boxplot(fill='red') +
 
#### lable names ####
labs(x='PE',y='', title='Box Plot') + 

#### Title-label ####
theme(plot.title = element_text(hjust=.5, size=20, face='bold')) +

#### x-label ####
theme(axis.title.x = element_text(hjust=.5, size=15, face='bold')) +

#### y-label ####
theme(axis.title.x = element_text(hjust=.5, size=15, face='bold'))
#######################################################################
######################## PE histogram #################################
pe.his = ggplot(data=firm_profit_df, aes(x=PE))+

#histogram
geom_histogram(bins = 15, fill = 'red', color = 'black')+

# lable names
labs(x='PE', y='Frequency',title='Histogram')+

# title-label
theme(plot.title = element_text(hjust=.5, size=20, face='bold')) +

# y-label
theme(axis.title.y = element_text(hjust=.5, size=15, face='bold')) +

# x-label
theme(axis.title.x = element_text(hjust=.5, size=15, face='bold')) 
########################################################################
######################## PT vs PE scatt ################################
pe.scat = ggplot(data=firm_profit_df, aes(x=PE,y=PT)) + 

#### point settings ###
geom_point(shape=1,size=4,color="red") + 

#### lable names ####
labs(x='PE', y='PT', title='Scatter Plot') + 

#### Title-label ####
theme(plot.title = element_text(hjust=.5, size=20, face='bold')) +

#### x-label ####
theme(axis.title.x = element_text(hjust=.5, size=15, face='bold')) +

#### y-label ####
theme(axis.title.y = element_text(hjust=.5, size=15, face='bold'))

#####################################################################
######################## CR boxplot #################################
cr.box = ggplot(data=firm_profit_df, aes(x=CR)) +

geom_boxplot(fill='orange') +
 
#### lable names ####
labs(x='CR',y='', title='Box Plot') + 

#### Title-label ####
theme(plot.title = element_text(hjust=.5, size=20, face='bold')) +

#### x-label ####
theme(axis.title.x = element_text(hjust=.5, size=15, face='bold')) +

#### y-label ####
theme(axis.title.x = element_text(hjust=.5, size=15, face='bold'))
#######################################################################
######################## CR histogram #################################
cr.his = ggplot(data=firm_profit_df, aes(x=CR))+

#histogram
geom_histogram(bins = 15, fill = 'orange', color = 'black')+

# lable names
labs(x='CR', y='Frequency',title='Histogram')+

# title-label
theme(plot.title = element_text(hjust=.5, size=20, face='bold')) +

# y-label
theme(axis.title.y = element_text(hjust=.5, size=15, face='bold')) +

# x-label
theme(axis.title.x = element_text(hjust=.5, size=15, face='bold')) 
########################################################################
######################## PT vs CR scatt ################################
cr.scat = ggplot(data=firm_profit_df, aes(x=CR,y=PT)) + 

#### point settings ###
geom_point(shape=1,size=4,color="orange") + 

#### lable names ####
labs(x='CR', y='PT', title='Scatter Plot') + 

#### Title-label ####
theme(plot.title = element_text(hjust=.5, size=20, face='bold')) +

#### x-label ####
theme(axis.title.x = element_text(hjust=.5, size=15, face='bold')) +

#### y-label ####
theme(axis.title.y = element_text(hjust=.5, size=15, face='bold'))

#####################################################################
######################## AS boxplot #################################
as.box = ggplot(data=firm_profit_df, aes(x=AS)) +

geom_boxplot(fill='yellow') +
 
#### lable names ####
labs(x='AS',y='', title='Box Plot') + 

#### Title-label ####
theme(plot.title = element_text(hjust=.5, size=20, face='bold')) +

#### x-label ####
theme(axis.title.x = element_text(hjust=.5, size=15, face='bold')) +

#### y-label ####
theme(axis.title.x = element_text(hjust=.5, size=15, face='bold'))
#######################################################################
######################## AS histogram #################################
as.his = ggplot(data=firm_profit_df, aes(x=AS))+

#histogram
geom_histogram(bins = 15, fill = 'yellow', color = 'black')+

# lable names
labs(x='AS', y='Frequency',title='Histogram')+

# title-label
theme(plot.title = element_text(hjust=.5, size=20, face='bold')) +

# y-label
theme(axis.title.y = element_text(hjust=.5, size=15, face='bold')) +

# x-label
theme(axis.title.x = element_text(hjust=.5, size=15, face='bold')) 
########################################################################
######################## PT vs AS scatt ################################
as.scat = ggplot(data=firm_profit_df, aes(x=AS,y=PT)) + 

#### point settings ###
geom_point(shape=1,size=4,color="yellow") + 

#### lable names ####
labs(x='AS', y='PT', title='Scatter Plot') + 

#### Title-label ####
theme(plot.title = element_text(hjust=.5, size=20, face='bold')) +

#### x-label ####
theme(axis.title.x = element_text(hjust=.5, size=15, face='bold')) +

#### y-label ####
theme(axis.title.y = element_text(hjust=.5, size=15, face='bold'))

#####################################################################
######################## CAS boxplot #################################
cas.box = ggplot(data=firm_profit_df, aes(x=CAS)) +

geom_boxplot(fill='green') +
 
#### lable names ####
labs(x='CAS',y='', title='Box Plot') + 

#### Title-label ####
theme(plot.title = element_text(hjust=.5, size=20, face='bold')) +

#### x-label ####
theme(axis.title.x = element_text(hjust=.5, size=15, face='bold')) +

#### y-label ####
theme(axis.title.x = element_text(hjust=.5, size=15, face='bold'))
#######################################################################
######################## CAS histogram #################################
cas.his = ggplot(data=firm_profit_df, aes(x=CAS))+

#histogram
geom_histogram(bins = 15, fill = 'green', color = 'black')+

# lable names
labs(x='CAS', y='Frequency',title='Histogram')+

# title-label
theme(plot.title = element_text(hjust=.5, size=20, face='bold')) +

# y-label
theme(axis.title.y = element_text(hjust=.5, size=15, face='bold')) +

# x-label
theme(axis.title.x = element_text(hjust=.5, size=15, face='bold')) 
########################################################################
######################## PT vs CAS scatt ################################
cas.scat = ggplot(data=firm_profit_df, aes(x=CAS,y=PT)) + 

#### point settings ###
geom_point(shape=1,size=4,color="green") + 

#### lable names ####
labs(x='CAS', y='PT', title='Scatter Plot') + 

#### Title-label ####
theme(plot.title = element_text(hjust=.5, size=20, face='bold')) +

#### x-label ####
theme(axis.title.x = element_text(hjust=.5, size=15, face='bold')) +

#### y-label ####
theme(axis.title.y = element_text(hjust=.5, size=15, face='bold'))

#####################################################################
######################## GR boxplot #################################
gr.box = ggplot(data=firm_profit_df, aes(x=GR)) +

geom_boxplot(fill='blue') +
 
#### lable names ####
labs(x='GR',y='', title='Box Plot') + 

#### Title-label ####
theme(plot.title = element_text(hjust=.5, size=20, face='bold')) +

#### x-label ####
theme(axis.title.x = element_text(hjust=.5, size=15, face='bold')) +

#### y-label ####
theme(axis.title.x = element_text(hjust=.5, size=15, face='bold'))
#######################################################################
######################## GR histogram #################################
gr.his = ggplot(data=firm_profit_df, aes(x=GR))+

#histogram
geom_histogram(bins = 15, fill = 'blue', color = 'black')+

# lable names
labs(x='GR', y='Frequency',title='Histogram')+

# title-label
theme(plot.title = element_text(hjust=.5, size=20, face='bold')) +

# y-label
theme(axis.title.y = element_text(hjust=.5, size=15, face='bold')) +

# x-label
theme(axis.title.x = element_text(hjust=.5, size=15, face='bold')) 
########################################################################
######################## PT vs GR scatt ################################
gr.scat = ggplot(data=firm_profit_df, aes(x=GR,y=PT)) + 

#### point settings ###
geom_point(shape=1,size=4,color="blue") + 

#### lable names ####
labs(x='GR', y='PT', title='Scatter Plot') + 

#### Title-label ####
theme(plot.title = element_text(hjust=.5, size=20, face='bold')) +

#### x-label ####
theme(axis.title.x = element_text(hjust=.5, size=15, face='bold')) +

#### y-label ####
theme(axis.title.y = element_text(hjust=.5, size=15, face='bold'))

#####################################################################
######################## DIV2 boxplot #################################
div2.box = ggplot(data=firm_profit_df, aes(x=DIV2)) +

geom_boxplot(fill='#4b0082') +
 
#### lable names ####
labs(x='DIV2',y='', title='Box Plot') + 

#### Title-label ####
theme(plot.title = element_text(hjust=.5, size=20, face='bold')) +

#### x-label ####
theme(axis.title.x = element_text(hjust=.5, size=15, face='bold')) +

#### y-label ####
theme(axis.title.x = element_text(hjust=.5, size=15, face='bold'))
#######################################################################
######################## DIV3 histogram #################################
div2.his = ggplot(data=firm_profit_df, aes(x=DIV2))+

#histogram
geom_histogram(bins = 15, fill = '#4b0082', color = 'black')+

# lable names
labs(x='DIV2', y='Frequency',title='Histogram')+

# title-label
theme(plot.title = element_text(hjust=.5, size=20, face='bold')) +

# y-label
theme(axis.title.y = element_text(hjust=.5, size=15, face='bold')) +

# x-label
theme(axis.title.x = element_text(hjust=.5, size=15, face='bold')) 
########################################################################
######################## PT vs DIV2 scatt ################################
div2.scat = ggplot(data=firm_profit_df, aes(x=DIV2,y=PT)) + 

#### point settings ###
geom_point(shape=1,size=4,color="#4b0082") + 

#### lable names ####
labs(x='DIV2', y='PT', title='Scatter Plot') + 

#### Title-label ####
theme(plot.title = element_text(hjust=.5, size=20, face='bold')) +

#### x-label ####
theme(axis.title.x = element_text(hjust=.5, size=15, face='bold')) +

#### y-label ####
theme(axis.title.y = element_text(hjust=.5, size=15, face='bold'))

#####################################################################
######################## DIV3 boxplot #################################
div3.box = ggplot(data=firm_profit_df, aes(x=DIV3)) +

geom_boxplot(fill='violet') +
 
#### lable names ####
labs(x='DIV3',y='', title='Box Plot') + 

#### Title-label ####
theme(plot.title = element_text(hjust=.5, size=20, face='bold')) +

#### x-label ####
theme(axis.title.x = element_text(hjust=.5, size=15, face='bold')) +

#### y-label ####
theme(axis.title.x = element_text(hjust=.5, size=15, face='bold'))
#######################################################################
######################## DIV3 histogram #################################
div3.his = ggplot(data=firm_profit_df, aes(x=DIV3))+

#histogram
geom_histogram(bins = 15, fill = 'violet', color = 'black')+

# lable names
labs(x='DIV3', y='Frequency',title='Histogram')+

# title-label
theme(plot.title = element_text(hjust=.5, size=20, face='bold')) +

# y-label
theme(axis.title.y = element_text(hjust=.5, size=15, face='bold')) +

# x-label
theme(axis.title.x = element_text(hjust=.5, size=15, face='bold')) 
########################################################################
######################## PT vs DIV3 scatt ################################
div3.scat = ggplot(data=firm_profit_df, aes(x=DIV3,y=PT)) + 

#### point settings ###
geom_point(shape=1,size=4,color="violet") + 

#### lable names ####
labs(x='DIV3', y='PT', title='Scatter Plot') + 

#### Title-label ####
theme(plot.title = element_text(hjust=.5, size=20, face='bold')) +

#### x-label ####
theme(axis.title.x = element_text(hjust=.5, size=15, face='bold')) +

#### y-label ####
theme(axis.title.y = element_text(hjust=.5, size=15, face='bold'))

#####################################################################
######################## SIZ boxplot #################################
siz.box = ggplot(data=firm_profit_df, aes(x=SIZ)) +

geom_boxplot(fill='cyan') +
 
#### lable names ####
labs(x='SIZ',y='', title='Box Plot') + 

#### Title-label ####
theme(plot.title = element_text(hjust=.5, size=20, face='bold')) +

#### x-label ####
theme(axis.title.x = element_text(hjust=.5, size=15, face='bold')) +

#### y-label ####
theme(axis.title.x = element_text(hjust=.5, size=15, face='bold'))
#######################################################################
######################## SIZ histogram #################################
siz.his = ggplot(data=firm_profit_df, aes(x=SIZ))+

#histogram
geom_histogram(bins = 15, fill = 'cyan', color = 'black')+

# lable names
labs(x='SIZ', y='Frequency',title='Histogram')+

# title-label
theme(plot.title = element_text(hjust=.5, size=20, face='bold')) +

# y-label
theme(axis.title.y = element_text(hjust=.5, size=15, face='bold')) +

# x-label
theme(axis.title.x = element_text(hjust=.5, size=15, face='bold')) 
########################################################################
######################## PT vs SIZ scatt ################################
siz.scat = ggplot(data=firm_profit_df, aes(x=SIZ,y=PT)) + 

#### point settings ###
geom_point(shape=1,size=4,color="cyan") + 

#### lable names ####
labs(x='SIZ', y='PT', title='Scatter Plot') + 

#### Title-label ####
theme(plot.title = element_text(hjust=.5, size=20, face='bold')) +

#### x-label ####
theme(axis.title.x = element_text(hjust=.5, size=15, face='bold')) +

#### y-label ####
theme(axis.title.y = element_text(hjust=.5, size=15, face='bold'))

#####################################################################
######################## LRSIZ boxplot #################################
lrsiz.box = ggplot(data=firm_profit_df, aes(x=LRSIZ)) +

geom_boxplot(fill='brown') +
 
#### lable names ####
labs(x='LRSIZ',y='', title='Box Plot') + 

#### Title-label ####
theme(plot.title = element_text(hjust=.5, size=20, face='bold')) +

#### x-label ####
theme(axis.title.x = element_text(hjust=.5, size=15, face='bold')) +

#### y-label ####
theme(axis.title.x = element_text(hjust=.5, size=15, face='bold'))
#######################################################################
######################## LRSIZ histogram #################################
lrsiz.his = ggplot(data=firm_profit_df, aes(x=LRSIZ))+

#histogram
geom_histogram(bins = 15, fill = 'brown', color = 'black')+

# lable names
labs(x='LRSIZ', y='Frequency',title='Histogram')+

# title-label
theme(plot.title = element_text(hjust=.5, size=20, face='bold')) +

# y-label
theme(axis.title.y = element_text(hjust=.5, size=15, face='bold')) +

# x-label
theme(axis.title.x = element_text(hjust=.5, size=15, face='bold')) 
########################################################################
######################## PT vs LRSIZ scatt ################################
lrsiz.scat = ggplot(data=firm_profit_df, aes(x=LRSIZ,y=PT)) + 

#### point settings ###
geom_point(shape=1,size=4,color="brown") + 

#### lable names ####
labs(x='LRSIZ', y='PT', title='Scatter Plot') + 

#### Title-label ####
theme(plot.title = element_text(hjust=.5, size=20, face='bold')) +

#### x-label ####
theme(axis.title.x = element_text(hjust=.5, size=15, face='bold')) +

#### y-label ####
theme(axis.title.y = element_text(hjust=.5, size=15, face='bold'))



pe.box+pe.his+pe.scat+
cr.box+cr.his+cr.scat+
as.box+as.his+as.scat

cas.box+cas.his+cas.scat+
gr.box+gr.his+gr.scat+
div2.box+div2.his+div2.scat

div3.box+div3.his+div3.scat+
siz.box+siz.his+siz.scat+
lrsiz.box+lrsiz.his+lrsiz.scat



head(firm_profit_df,3)



###### Full blown model #####
ind_vars = c('PE','CR','AS','CAS','GR','DIV2','DIV3','SIZ','LRSIZ')
model = lm(PT ~PE+CR+AS+CAS+GR+DIV2+DIV3+SIZ+LRSIZ,data=firm_profit_df)

######### eigenn values #########
X = model.matrix(model)[,-1]
eig = eigen(t(X)%*%X)
eig$val
K = sqrt(eig$val[1]/eig$val)



### corr maxtrix trail 1###

X = model.matrix(model)[,-1] # remove the intercept part
M = cor(X)
corrplot(M, method="number",col = rainbow(12)) ### AS variable will be removed

model = lm(PT ~PE+CR+CAS+GR+DIV2+DIV3+SIZ+LRSIZ,data=firm_profit_df)
round(vif(model),3)

### corr maxtrix trail 2###

X = model.matrix(model)[,-1] # remove the intercept part
M = cor(X)
corrplot(M, method="number",col = rainbow(12)) ### DIV3 variable will be removed

### corr maxtrix trail 3###

model = lm(PT ~PE+CR+CAS+GR+DIV2+SIZ+LRSIZ,data=firm_profit_df)
round(vif(model),3)

### corr maxtrix trail 3###

X = model.matrix(model)[,-1] # remove the intercept part
M = cor(X)
corrplot(M, method="number",col = rainbow(12)) ### SIZ variable will be removed

X = model.matrix(model)[,-1]
eig = eigen(t(X)%*%X)
eig$val

model = lm(PT ~PE+CR+CAS+GR+DIV2+LRSIZ,data=firm_profit_df)
round(vif(model),3)

### corr maxtrix trail 4###

X = model.matrix(model)[,-1] # remove the intercept part
M = cor(X)
corrplot(M, method="number",col = rainbow(12)) ### Looks Great!

ind_vars = c('PE','CR','CAS','GR','DIV2','LRSIZ')
results = combination_function('PT',ind_vars,firm_profit_df)

##### top 16 from the list #####
head(results,16)

### all models that are sig at 0.05 ###

model = lm(PT~PE+CAS+LRSIZ,firm_profit_df) # best model
#model = lm(PT~PE+CAS,firm_profit_df)
#model3 =lm(PT~PE+DIV2,firm_profit_df)
#model4 = lm(PT~PE,firm_profit_df)
#model5 = lm(PT~CAS,firm_profit_df)
#model6 = lm(PT~CR+GR,firm_profit_df)
summary(model)



# getting the residuals
residuals = model$residuals

# standardize residuals
stand_residuals = rstandard(model)

# jackknife residuals
jack_residuals = rstudent(model)

#find Cook's distance for each observation
cooks_distance = cooks.distance(model)

#diangoal of the hat matrix
leverage = hatvalues(model)

#make a dataframe of interest

#empty dataframe
df = data.frame()


#add cols of interest
n = length(firm_profit_df$LRSIZ)
df = cbind( c(1:n),firm_profit_df$PE,firm_profit_df$CAS,firm_profit_df$LRSIZ,firm_profit_df$PT,fitted(model),model$residuals,stand_residuals,jack_residuals,cooks_distance,leverage)


#cols name
col_labels=c('observations',"PE(x2)","CAS(x6)","LRSIZ(x11)","PT(y)",'PT(yhat)',"residuals","stand_residuals","jackknife_residuals",'cooks_distance','leverage')
colnames(df) = col_labels

#convert matrix to dataframe
df = data.frame(df)



head(df,5)



### Linearity ###

#upload data
ggplot(data=df, aes(x=PT.yhat., y=residuals)) + 

#point settings
geom_point(shape=1,size=7,colour="purple") + # point settings

theme_stata() +


# lable names
labs(x='fitted values', y='residuals', title='Residuals vs Fitted values') + 

# title-label
theme(plot.title = element_text(hjust=.5, size=23, face='bold')) +

# x-label
theme(axis.title.x = element_text(hjust=.5, size=18, face='bold')) +

# y-label
theme(axis.title.y = element_text(hjust=.5, size=18, face='bold')) +

# add hor line
geom_hline(yintercept=0, col = "black", size=1) 






### normalirty1 ###

#upload data
ggplot(data=df, aes(sample=residuals)) +

# qq plot
stat_qq() +

# add line
geom_qq_line()+

scale_color_brewer(palette="Dark2")+

theme_stata() +

# lable names
labs( title='Q-Q Plot')+

# title-label
theme(plot.title = element_text(hjust=.5, size=23, face='bold')) +

# y-label
theme(axis.title.y = element_text(hjust=.5, size=18, face='bold'))  +
# x-label
theme(axis.title.x = element_text(hjust=.5, size=18, face='bold')) 




### normalirty2 ###

#upload data
ggplot(data=df, aes(x=residuals))+

#histogram
geom_histogram(bins = 15, fill = 'purple', color = 'black')+

# lable names
labs(x='residuals', y='Frequency',title='histogram')+

theme_stata() +

# title-label
theme(plot.title = element_text(hjust=.5, size=23, face='bold')) +

# y-label
theme(axis.title.y = element_text(hjust=.5, size=18, face='bold')) +

# x-label
theme(axis.title.x = element_text(hjust=.5, size=18, face='bold')) 




### normalirty3 ###

shapiro.test(residuals)



#### independence assumption #####
dwtest(model, alternative=c("two.sided"))



head(df,3)

##### standarize residuals #####

# need to be  > 3 std

#upload data
ggplot(data=df, aes(x=PT.yhat., y=stand_residuals)) + 

theme_stata() +

#point settings
geom_point(shape=1,size=4,colour="purple") + # point settings

# lable names
labs(x='Fitted Values', y='Standardized Residuals', title='Standardized Residuals vs Fitted Values') + 

# title-label
theme(plot.title = element_text(hjust=.5, size=23, face='bold')) +

# x-label
theme(axis.title.x = element_text(hjust=.5, size=18, face='bold')) +

# y-label
theme(axis.title.y = element_text(hjust=.5, size=18, face='bold')) +

# add hor line
geom_hline(yintercept=0, col = "black", size=1)+

# add hor line
geom_hline(yintercept=3, linetype='dashed',col = "blue", size=1)+

# add hor line
geom_hline(yintercept=-3,linetype='dashed' ,col = "blue", size=1)+

# add label to potential outliers under conditions
geom_label(data=df %>% filter(abs(stand_residuals)>3),aes(label=observations))


#new_df = df[order(stand_residuals),]
#tail(new_df,6)



##### Jacknfive residual #####

#Bonferroni critical value
bonferroni=qt(.025/n,df=n-2,lower.tail = FALSE)


#upload data
ggplot(data=df, aes(x=PT.yhat., y=jackknife_residuals)) + 

theme_stata() +

#point settings
geom_point(shape=1,size=4,colour="purple") + # point settings

# lable names
labs(x='Fitted Values', y='Jackknife Residuals', title='Jackknife Residuals vs Fitted Values') + 

# title-label
theme(plot.title = element_text(hjust=.5, size=23, face='bold')) +

# x-label
theme(axis.title.x = element_text(hjust=.5, size=18, face='bold')) +

# y-label
theme(axis.title.y = element_text(hjust=.5, size=18, face='bold')) +

# add hor line 
geom_hline(yintercept=0, col = "black", size=1)+

# add hor line bonferroni
geom_hline(yintercept=bonferroni, linetype='dashed',col = "blue", size=1)+

# add hor line bonferroni
geom_hline(yintercept=-bonferroni,linetype='dashed' ,col = "blue", size=1)+

# add label to potential outliers under conditions
geom_label( data=df %>% filter(abs(stand_residuals)>bonferroni),aes(label=observations))

bonferroni



### cook distance ###

ggplot(data=df, aes(x=observations, y=cooks_distance)) + 

#point settings
geom_point(shape=1,size=4,colour="purple") + # point settings

theme_stata() +

# lable names
labs(x='Obervations', y='Cook Distance', title='Cook Distance vs Observations') + 

# title-label
theme(plot.title = element_text(hjust=.5, size=23, face='bold')) +

# x-label
theme(axis.title.x = element_text(hjust=.5, size=18, face='bold')) +

# y-label
theme(axis.title.y = element_text(hjust=.5, size=18, face='bold')) +

# add hor line 
geom_hline(yintercept=0, col = "black", size=1)+

# add hor cook distance thresh hold
geom_hline(yintercept=1, linetype='dashed',col = "blue", size=1)+


# add label to potential outliers under conditions
geom_label( data=df %>% filter(cooks_distance>1),aes(label=observations))



### leverage points graph ###

# Outlier = Leverage > 2*p/n, p = rank of X

X = model.matrix(model)
p = as.integer(rankMatrix(X))
c.thes = 2*p/n

#upload data
ggplot(data=df, aes(x=observations, y=leverage)) + 

#point settings
geom_point(shape=1,size=4,colour="purple") + # point settings

theme_stata() +

# lable names
labs(x='Observations', y='Leverage', title='Leverage vs Observations') + 

# title-label
theme(plot.title = element_text(hjust=.5, size=23, face='bold')) +

# x-label
theme(axis.title.x = element_text(hjust=.5, size=18, face='bold')) +

# y-label
theme(axis.title.y = element_text(hjust=.5, size=18, face='bold')) +

# add hor line 
geom_hline(yintercept=0, col = "black", size=1)+

# add hor line for thres hold
geom_hline(yintercept=c.thes, linetype='dashed',col = "blue", size=1)+

# add label to potential outliers under conditions
geom_label( data=df %>% filter(leverage>c.thes),aes(label=observations))

c.thes



summary(model)





# before
summary(model)



### confindence int for population ###1

# Confidence interval
newdata=data.frame(PE=.20,CAS=4,LRSIZ=.20)

predict(model,newdata,inteval='confidence',interval="confidence",level=.95) 

#we are 95% confindent that the expected _____ is between lb and hb when x2 is __, x6 is ___, and x11 is ____.

### confindence int for population ###2

# Confidence interval
newdata=data.frame(PE=.50,CAS=30,LRSIZ=.50)

predict(model,newdata,inteval='confidence',interval="confidence",level=.95) 

#we are 95% confindent that the expected _____ is between lb and hb when x2 is __, x6 is ___, and x11 is ____.

### confindence int for population ###3

# Confidence interval
newdata=data.frame(PE=.75,CAS=20,LRSIZ=.45)

predict(model,newdata,inteval='confidence',interval="confidence",level=.95) 

#we are 95% confindent that the expected _____ is between lb and hb when x2 is __, x6 is ___, and x11 is ____.



### prediction int for an indivual y-value ###1

# prediction interval
newdata=data.frame(PE=.2,CAS=4,LRSIZ=.20)

predict(model,newdata,inteval='prediction',interval="prediction",level=.95) 

#we are 95% confindent that the y _____ is between lb and ub when x2 is __, x6 is ___, and x11 is ____.

### prediction int for an indivual y-value ###2

# prediction interval
newdata=data.frame(PE=.5,CAS=30,LRSIZ=.50)

predict(model,newdata,inteval='prediction',interval="prediction",level=.95) 

#we are 95% confindent that the y _____ is between lb and ub when x2 is __, x6 is ___, and x11 is ____.

### prediction int for an indivual y-value ###3

# prediction interval
newdata=data.frame(PE=.75,CAS=20,LRSIZ=.45)

predict(model,newdata,inteval='prediction',interval="prediction",level=.95) 

#we are 95% confindent that the y _____ is between lb and ub when x2 is __, x6 is ___, and x11 is ____.


