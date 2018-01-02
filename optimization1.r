library(igraph)
library(dplyr)
require(data.table)
library(ggpubr)

m2<-read.csv(file="label_6y.csv",header=TRUE, sep=",")
m2<-m2[,c("person_uid","label_6y")]
#remember we need to do upto 2013 Spring


CalculateMaximizedDensity<-function()
{
  maxIm=read.csv(file="MaxImprove.csv",header=TRUE, sep=",")
  m1=read.csv(file="ForWholeNetwork.csv",header=TRUE, sep=",")
  m1<-m1[,c("c1_person_uid","c2_person_uid","weight")]
  el=as.matrix(m1) # coerces the data into a two-column matrix format that igraph likes
  el[,1]=as.character(el[,1])
  el[,2]=as.character(el[,2])
  if(nrow(el)<=1)
    {
      next
    }
  g=graph.edgelist(el[,1:2],directed = FALSE)
  g<-igraph::simplify(g,remove.multiple = TRUE, remove.loops = TRUE, edge.attr.comb="min")
  E(g)$weight=as.numeric(el[,3])
  ids<-V(g)$name
  df <- data.frame(matrix(ncol = 2, nrow = 0))
  names(df)<-c("person_uid","EgoMaxGraphDensity")
  
  for(i in ids)
    {
    t1<-calculateforEachV(g,i,maxIm)
    cc<-data.frame(person_uid=i,EgoMaxGraphDensity=t1,row.names=NULL)
    df <- rbind(df, cc)
  }
  df[is.na(df)] <- 0
}

calculateforEachV<-function(g,v,maximproveValues)
{
  inducedGraphInit<-ego(g,1,nodes = v)
  your_subgraph <- induced_subgraph(g, vids=inducedGraphInit[[1]])
  edges<-as.data.frame(cbind(get.edgelist(your_subgraph),E(g)$weight))
  setnames(edges, old=c("V1","V2","V3"), new=c("c1_person_uid", "c2_person_uid","weight"))
  A <- get.adjacency(your_subgraph)
  triangle_matrix <- as.matrix(Matrix::tcrossprod(A))  # scalar product of rows
  E(your_subgraph)$num_triangles <- triangle_matrix[ends(your_subgraph, E(your_subgraph))]
  edges$countTriangles<-E(your_subgraph)$num_triangles
  
  
  
  
  merged<-merge(edges,maximproveValues, by=c("c1_person_uid","c2_person_uid"),all.x = TRUE)
  merged<-merged[!is.na(merged$choose),]
  
  OnlyRS<-subset(merged,merged$choose==1)
  NotRS<-subset(merged,merged$choose==0)
  meanR<-mean(OnlyRS$extraWeight)
  OnlyRS$extraWeight <- (0.5/meanR)*OnlyRS$extraWeigh
  OnlyRS$weight.y<-OnlyRS$countTriangles * OnlyRS$extraWeight
  OnlyRS <- OnlyRS[with(OnlyRS, order(-weight.y)), ]
  total<-nrow(OnlyRS)
  SelectedRS<-head(OnlyRS,500)
  NotSelected<- tail(OnlyRS,total-500)
  
  SelectedRS$weight.x<-as.numeric(as.character(SelectedRS$weight.x)) + SelectedRS$extraWeight
  NotSelected$weight.x<-as.numeric(as.character(NotSelected$weight.x))
  NotRS$weight.x<-as.numeric(as.character(NotRS$weight.x))
  
  df <- rbind(SelectedRS,NotSelected)
  df<-rbind(df,NotRS)
  noVertex<-vcount(your_subgraph)
  #print(edge_attr(your_subgraph, 'weight', index = E(your_subgraph)))
  t1<-2*sum(df$weight.x)/(noVertex*(noVertex-1))
  
  }


multmerge = function(mypath){
  filenames=list.files(path=mypath, full.names=TRUE)
  datalist = lapply(filenames, function(x){read.csv(file=x,header=T)})
  Reduce(function(x,y) {rbind(x,y)}, datalist)}


is.finite.data.frame <- function(obj){
  sapply(obj,FUN = function(x) all(is.finite(x)))
}

calculateMaxImprove<-function()
{
  mypath="C:/Users/nnur/Desktop/backup/FreshStart/all_test/"
  datalist<-multmerge(mypath)
  datalist<-subset(datalist,datalist$academic_period<201280)
  datalist<-datalist%>%group_by(c1_person_uid,c2_person_uid,WhichRule) %>% summarise(weight=sum(cumilativeNumClassesCount))
  keep<-c("c1_person_uid","c2_person_uid","weight","WhichRule")
  datalist<-unique(datalist[keep])
  
  dd<-data.frame(person_uid=V(g)$name,colors=V(g)$color,row.names=NULL)
  mrg<-merge(datalist,dd,by.x = c("c1_person_uid"),by.y =  c("person_uid"),all= TRUE)
  colnames(mrg)[colnames(mrg)=="colors"] <- "c1_color"	
  mrg<-merge(mrg,dd,by.x = c("c2_person_uid"),by.y =  c("person_uid"),all= TRUE)
  colnames(mrg)[colnames(mrg)=="colors"] <- "c2_color"
  mrg<-mrg[!is.na(mrg$c1_color),]
  mrg<-mrg[!is.na(mrg$c2_color),]
  
  mrg$choose<-ifelse(mrg$c1_color=="green" & mrg$c2_color=="green",0,1)
  
  mrgB<- data.frame (table(mrg$c1_person_uid, mrg$c2_person_uid))
  colnames(mrgB)[colnames(mrgB)=="Var1"] <- "c1_person_uid"
  colnames(mrgB)[colnames(mrgB)=="Var2"] <- "c2_person_uid"
  
  mrg<-merge(mrg, mrgB, by=c("c1_person_uid","c2_person_uid"),  all.x = TRUE)
  
  mrg$extraWeight<-ifelse(mrg$choose==0,0,ifelse(mrg$Freq==1,2,ifelse(mrg$Freq==2,1,0)))
  
  mrgc<-mrg[c("c1_person_uid","c2_person_uid","weight","extraWeight","choose")]
  write.csv("MaxImprove.csv",x=mrgc,row.names = FALSE)
  
  
  
}

# choosePair<-function(g,v1,v2)
# {
#   if(vertex_attr(g,"color",index = V(v1))=="green" & vertex_attr(g,"color",index = V(v2))=="green")
#     choose
# }


# EgoGraphDensity<-function()
# {
#   test<-read.csv(file="AllRulesExp.csv",header=TRUE, sep=",")
#   academicPeriods<-sort(unique(test$academic_period))
#   ##
#   df <- data.frame(matrix(ncol = 3, nrow = 0))
#   names(df)<-c("person_uid","academic_period","EgoGraphDensity")
#   for(ap in academicPeriods)
#   {
#     fi<-subset(test,test$academic_period<=ap)
#     m1<-fi[,c("c1_person_uid","c2_person_uid","weight")]
#     el=as.matrix(m1) # coerces the data into a two-column matrix format that igraph likes
#     el[,1]=as.character(el[,1])
#     el[,2]=as.character(el[,2])
#     if(nrow(el)<=1)
#     {
#       next
#     }
#     g=graph.edgelist(el[,1:2],directed = FALSE) 
#     g<-igraph::simplify(g,remove.multiple = TRUE, remove.loops = TRUE, edge.attr.comb="min")
#     E(g)$weight=as.numeric(el[,3])
#     ids<-V(g)$name
#     for(i in ids)
#     {
#       inducedGraphInit<-ego(g,1,nodes = i)
#       your_subgraph <- induced_subgraph(g, vids=inducedGraphInit[[1]])
#       t1 <- edge_density(your_subgraph, loops = FALSE)
#       cc<-data.frame(person_uid=i,academic_period=ap,EgoGraphDensity=t1,row.names=NULL)
#       df <- rbind(df, cc)
#     }
#   }
#   df[is.na(df)] <- 0
#   write.csv("EgoGraphDensity.csv",x=df,row.names = FALSE)
# }
