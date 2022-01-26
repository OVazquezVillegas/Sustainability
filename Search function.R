

##############################
### STEP 5: KEYWORD SEARCH ###
##############################

# Input values
file = 'output'
output = 'data_searched_full'
thesaurus = 'ramirez_thesaurus'
cores = 1

###
direct=getwd()
setwd(paste(direct,'Analysis',sep='/'))
library(textstem);library(tm);library(parallel)
data=as.matrix(read.csv(paste(file,".csv",sep="")))
words=as.matrix(read.csv(paste(thesaurus,'.csv',sep='')))

# The search function
Search=function(j){
  if(words[j,'Mode']=='stemmed'){
    if(words[j,'type.1']==1){
      if(length(grep(paste("\\b", tolower(words[j,'stemmed']), "\\b", sep=""),datas_stemmed))!=0){paste(grep(paste("\\b", tolower(words[j,'stemmed']), "\\b", sep=""),datas_stemmed),j,
                                                                                                        unlist(lapply(datas_stemmed[grep(paste("\\b", tolower(words[j,'stemmed']), "\\b", sep=""),datas_stemmed)],function(k){
                                                                                                          length(unlist(gregexpr(paste("\\b", tolower(words[j,'stemmed']), "\\b", sep=""),k)))})),sep='#')}}
    else{
      x=table(unlist(lapply(unlist(strsplit(words[j,'stemmed'],'#')),function(h){grep(paste("\\b",tolower(h),"\\b",sep=""),datas_stemmed)})))
      if(length(x[x==length(unlist(strsplit(words[j,'stemmed'],'#')))])!=0){paste(names(x[x==length(unlist(strsplit(words[j,'stemmed'],'#')))]),j,
                                                                                  apply(matrix(unlist(lapply(unlist(strsplit(words[j,'stemmed'],'#')),function(h){unlist(lapply(datas_stemmed[as.numeric(names(x[x==length(unlist(strsplit(words[j,'stemmed'],'#')))]))],
                                                                                                                                                                                function(k){length(unlist(gregexpr(paste("\\b",h,"\\b",sep=""),k)))}))})),byrow=T,ncol=length(unlist(strsplit(words[j,'stemmed'],'#')))),1,min),sep='#')}}}
  else{
    if(words[j,'type.1']==1){
      if(length(grep(paste('\\b',tolower(words[j,'corrected']),sep=""),datas))!=0){paste(grep(paste('\\b',tolower(words[j,'corrected']),sep=""),datas),j,
                                                                                         unlist(lapply(datas[grep(paste('\\b',tolower(words[j,'corrected']),sep=""),datas)],function(k){
                                                                                           length(unlist(gregexpr(paste('\\b',tolower(words[j,'corrected']),sep=""),k)))})),sep='#')}}
    else{
      x=table(unlist(lapply(unlist(strsplit(words[j,'corrected'],'#')),function(h){grep(paste('\\b',tolower(h),sep=""),datas)})))
      if(length(x[x==length(unlist(strsplit(words[j,'corrected'],'#')))])!=0){paste(names(x[x==length(unlist(strsplit(words[j,'corrected'],'#')))]),j,
                                                                                    apply(matrix(unlist(lapply(unlist(strsplit(words[j,'corrected'],'#')),function(h){unlist(lapply(datas[as.numeric(names(x[x==length(unlist(strsplit(words[j,'corrected'],'#')))]))],
                                                                                                                                                                                    function(k){length(unlist(gregexpr(paste('\\b',h,sep=""),k)))}))})),byrow=T,ncol=length(unlist(strsplit(words[j,'corrected'],'#')))),1,min),sep='#')}}}
}

# Prepare thesaurus and data
words[,'corrected']=unlist(lapply(words[,'corrected'], function(x) gsub('”', '\\\\b', x)))
words[,'corrected']=unlist(lapply(words[,'corrected'], function(x) gsub('\\*', '', x)))
datas=data[,c('TI','AB','DE')]
data[,'UT']=substring(data[,'UT'],1,30)
datas=gsub("  "," ",gsub('[[:digit:]]+', '', gsub('  ',' ',gsub('[[:punct:] ]+',' ',tolower(unlist(mclapply(1:dim(datas)[1],function(i){paste(datas[i,1],datas[i,2],datas[i,3])},mc.cores=cores)))))))
datas=gsub("  "," ",unlist(mclapply(datas,function(i){paste(unlist(strsplit(i,' '))[!unlist(strsplit(i,' '))%in%stopwords()],collapse=" ")},mc.cores=cores)))
datas_stemmed=gsub("  "," ",unlist(mclapply(datas,function(i){paste(stem_words(unlist(strsplit(i,' '))[!unlist(strsplit(i,' '))%in%stopwords()]),collapse=" ")},mc.cores=cores)))

# Search each publication for the keywords 
search=matrix(unlist(strsplit(unlist(mclapply(1:dim(words)[1],Search,mc.cores=cores)),"#")),byrow=T,ncol=3)
m=matrix(unlist(strsplit(unlist(lapply(1:dim(search)[1],function(i){paste(data[as.numeric(search[i,1]),'UT'], words[as.numeric(search[i,2]),2],words[as.numeric(search[i,2]),1],words[as.numeric(search[i,2]),3],search[i,3],sep='##')})),'##')),byrow=T,ncol=5)
colnames(m)=c('UT','KeyWord','SDG','Type','Freq')
write.csv(m,paste(output,"Words_Searched.csv",sep='_'),row.names=F)

# Determine the SDG(s) for each publication
datas=xtabs(as.numeric(m[,'Freq'])~m[,'UT']+m[,'SDG'])
datas=cbind(datas,rowSums(datas))
colnames(datas)=c(paste('SDG', c(2,3,4,5,6,8,12,13), sep = '.'),'FreqT');class(datas)='numeric'
data=data[data[,'UT']%in%rownames(datas),]
datas=datas[unlist(mclapply(1:dim(data)[1],function(i){which(rownames(datas)==data[i,'UT'])},mc.cores=cores)),]
data=data.frame(data,datas)
SDG=unlist(mclapply(1:dim(data)[1],function(i){ifelse(sum(sort(data[i,paste('SDG.',c(2,3,4,5,6,8,12,13), sep='')]/data[i,'FreqT'],decreasing=T)[1])>.75,
                                                      paste(names(sort(data[i,paste('SDG.',c(2,3,4,5,6,8,12,13), sep='')]/data[i,'FreqT'],decreasing=T))[1],collapse='#'),
                                                      ifelse(sum(sort(data[i,paste('SDG.',c(2,3,4,5,6,8,12,13), sep='')]/data[i,'FreqT'],decreasing=T)[1:2])>.60,
                                                             paste(names(sort(data[i,paste('SDG.',c(2,3,4,5,6,8,12,13), sep='')]/data[i,'FreqT'],decreasing=T))[1:2],collapse='#'),
                                                             ifelse(sum(sort(data[i,paste('SDG.',c(2,3,4,5,6,8,12,13), sep='')]/data[i,'FreqT'],decreasing=T)[1:3])>.50,
                                                                    paste(names(sort(data[i,paste('SDG.',c(2,3,4,5,6,8,12,13), sep='')]/data[i,'FreqT'],decreasing=T))[1:3],collapse='#'),
                                                                    paste(names(sort(data[i,paste('SDG.',c(2,3,4,5,6,8,12,13), sep='')]/data[i,'FreqT'],decreasing=T))[1:3],collapse='#'))))},mc.cores=cores))

data=cbind(data,SDG)

fields = c('UT', 'ACOUSTICS', 'ASTRONOMY & ASTROPHYSICS', 'CRYSTALLOGRAPHY', 'OPTICS')
delete=NULL
for (index in grep(paste(fields,collapse="|"),data$SC)){field=data[index,'SC']
if(length(unlist(strsplit(field,";")))==1){delete=append(delete,data[index,'UT'])}
else if(length(grep(paste(fields,collapse="|"),unlist(strsplit(field,";"))))==length(unlist(strsplit(field,";")))){
  delete=append(delete,data[index,'UT'])}}
data=subset(data,!UT%in%delete)

# Write output 
write.csv(data,paste(output,'.csv',sep=''),row.names=F)
setwd(direct)