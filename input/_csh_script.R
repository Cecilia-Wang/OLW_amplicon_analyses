#Function csh and csh.table (csh contains functions for plotting and analyzing occupancy-frequency distributions, whereas csh.table provides an output in table format of the occupancy-frequency distribution)
# require package
'csh'<-function(x,name= "",binwidth=1,family=gaussian,link="log",scale=scale_y_log10(),
                ...)
{
  library(ggplot2)
  library(vegan)
  library(scales)
  # x=t(x)
  # x is the taxa table
  x[x>0] = 1 # convert to present and absent table
  P1<-ggplot(data=data.frame(apply(x, 1, sum)[which(apply(x, 2, sum) >0)]),aes(x=data.frame(apply(x, 2, sum)[which(apply(x, 2, sum) >0)])[,1]))+geom_histogram(binwidth=binwidth,fill="darkblue",col="white")+ylab(paste("Frequency","(Number of ASVs)",sep="\n"))+xlab(paste("Occupancy","(Number of sample occupied)",sep="\n"))+ggtitle(name)+scale +theme_bw()+scale_x_continuous(breaks= pretty_breaks())+geom_text(stat = "count", aes(label=..count..), vjust=-0.1,size = 3)+theme(axis.title =element_text(size = 12),legend.text = element_text(size = 12,hjust = 1), legend.title = element_text(size = 12))
  print(P1)
  table.out=as.data.frame(table(apply(x, 2, sum)[which(apply(x, 2, sum) >0)]))
  colnames(table.out)= c("Occupancy","Frequency")
  print(table.out)
  (ratio=(table.out[nrow(table.out),2])/(table.out[1,2]))
  write.table(table.out,file=paste(name,"occupancy_frequency.txt",sep="_"),sep="\t",row.names=F)
  test.input=as.data.frame(table(apply(x, 2, sum)[which(apply(x, 2, sum) >0)]))
  MOS.test=MOStest(as.numeric(test.input[,1]), as.numeric(test.input[,2]),family=family(link = link))
  print(MOS.test)
  capture.output(MOS.test,file=paste(name,"MOS_test.txt",sep="_"))
  par(mfrow=c(2,2))
  P2<-plot(MOS.test)
  print(P2)
  return(P1)
}
'csh.table'<-function(x)
{
  x=t(x)
  x[which(x > 0)] = 1
  table.out=as.data.frame(table(apply(x, 2, sum)[which(apply(x, 2, sum) >0)]))
  print(table.out)
}		
#################
# for dataset x, calculate the sum number of each row if the same number of column has more than one count. 
# row names are sample names and col names are ASV IDs
