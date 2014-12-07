#########################################################################
#Calculation of Kendall-Theil Robust Linear Regression
# Input data set:
#	-A matrix with two columns, independent variable in first column, dependent...
#		variable in second column
# Further Reading:
#	Helsel, D. R. and Hirsch, R. M. (2002) "Statistical Methods in Water...
#		Resources". IN "Hydrologic Analysis and Interpretation", chap. A3, vol. iv...
#		of "Techniques of Water-Resources Investigations Reports" (United States...
#		Geological Survey), pp. 1-510.
#	Kendall, M. G. (1938) "A New Measurement of Rank Correlation". Biometrika 30 (1-2)...
#		81-93.
#	Sen, P. K. (1968) "Estimates of the Regression Coefficient Based on Kendall's Tau"...
#		Journal of the American Statistical Association 63 (324): 1379-89.
#	Theil, H. (1950) "A Rank-Invariant Method of Linear and Polynomial Regression...
#		Analysis, III". Proceedings of the Koninklijke Nederlandse Akademie van...
#		Wetenschappen 53 (9): 1397-412.
# Author: Manuel Weinkauf (manuel.weinkauf@uni-tuebingen.de)
# Version: 1.1
# Date: 16 October 2012
#########################################################################

Kendall<-function(Data,Output="Result.txt",Export=FALSE){
        #Setting up matrix for different slopes
        Slopes<-matrix(NA,sum((dim(Data)[1]-1):1),1)
        
        #Setting up temporary-pairs matrix
        n<-dim(Data)[1]-1
        c<-1
        Temp<-matrix(NA,2,2)
        
        #Setting up results matrix
        Res<-matrix(NA,6,1)
        colnames(Res)<-c("Results")
        rownames(Res)<-c("Dip","Upper CI Dip","Lower CI Dip","Intercept","Tau","p")
        
        #Calculating Slopes
        for (j in 1:n){
                for (i in j:n){
                        Temp[1,1]<-Data[j,1]
                        Temp[1,2]<-Data[j,2]
                        Temp[2,1]<-Data[(i+1),1]
                        Temp[2,2]<-Data[(i+1),2]
{if (Temp[1,1]==Temp[2,1]){SL<-NA}
 else if(Temp[1,1]<Temp[2,1]){SL<-(Temp[2,2]-Temp[1,2])/(Temp[2,1]-Temp[1,1])}
 else{SL<-(Temp[1,2]-Temp[2,2])/(Temp[1,1]-Temp[2,1])}
}
Slopes[c,1]<-SL
c<-c+1
                }
        }
SlopeSet<-as.data.frame(Slopes)
colnames(SlopeSet)<-"Dip"
attach(SlopeSet)
SlopeSet<-SlopeSet[order(Dip),]
detach(SlopeSet)
SlopeSet<-as.data.frame(SlopeSet)
colnames(SlopeSet)<-"Dip"

#Calculating dip of regression line
Res[1]<-median(SlopeSet$Dip,na.rm=TRUE)
Np<-sum((dim(Data)[1]-1):1)
N<-dim(Data)[1]
Z<-1.96
Ru<-round(((Np+(Z*(sqrt((N*(N-1)*(2*N+5))/18))))/2)+1)
Rl<-round(((Np-(Z*(sqrt((N*(N-1)*(2*N+5))/18))))/2))
Res[2]<-SlopeSet[Ru,1]
Res[3]<-SlopeSet[Rl,1]
Res[4]<-median(Data[,2])-(Res[1,1]*median(Data[,1]))

#Calculating significance
#Calculating S and Tau
M<-0
P<-0
for (j in 1:n){
        for (i in j:n){
                Temp[1,1]<-Data[j,1]
                Temp[1,2]<-Data[j,2]
                Temp[2,1]<-Data[(i+1),1]
                Temp[2,2]<-Data[(i+1),2]
{if (Temp[1,1]>Temp[2,1] & Temp[1,2]>Temp[2,2]){P<-P+1}
 else if (Temp[1,1]>Temp[2,1] & Temp[1,2]<Temp[2,2]){M<-M+1}
 else if (Temp[2,1]>Temp[1,1] & Temp[2,2]>Temp[1,2]){P<-P+1}
 else if (Temp[1,1]==Temp[2,1] | Temp[1,2]==Temp[2,2]){}
 else {M<-M+1}
}
        }
}
S<-P-M
Res[5]<-S/Np

#Counting Ties
XMin<-min(Data[,1])
XMax<-max(Data[,1])
YMin<-min(Data[,2])
YMax<-max(Data[,2])
XVals<-matrix(0,((XMax-XMin)+1),2)
YVals<-matrix(0,((YMax-YMin)+1),2)
XVals[,1]<-XMin:XMax
YVals[,1]<-YMin:YMax
XStart<-XMin-(XMin-1)
YStart<-YMin-(YMin-1)
for (i in XMin:XMax) {
        for (j in 1:(dim(Data)[1])) {
                if (Data[j,1]==i) {XVals[XStart,2]<-XVals[XStart,2]+1}
        }
        XStart<-XStart+1
} 
for (i in YMin:YMax) {
        for (j in 1:(dim(Data)[1])) {
                if (Data[j,2]==i) {YVals[YStart,2]<-YVals[YStart,2]+1}
        }
        YStart<-YStart+1
} 
TieList<-rbind(XVals,YVals)
TieList<-as.vector(TieList[,2])
TieMin<-min(TieList)
if(TieMin==0 | TieMin==1){TieMin<-2}
TieMax<-max(TieList)
{if (TieMax>1) {Ties<-TRUE}
 else {Ties<-FALSE}
}
if (Ties==TRUE) {
        TieCount<-matrix(0,((TieMax-TieMin)+1),3)
        TieCount[,1]<-TieMin:TieMax
        TieStart<-TieMin-(TieMin-1)
        for (i in TieMin:TieMax) {
                for (j in 1:(length(TieList))) {
                        if (TieList[j]==i) {TieCount[TieStart,2]<-TieCount[TieStart,2]+1}
                }
                TieStart<-TieStart+1
        }
        for (i in 1:(dim(TieCount)[1])) {
                TieCount[i,3]<-TieCount[i,2]*TieCount[i,1]*(TieCount[i,1]-1)*(2*TieCount[i,1]+5)	
        }
        Corr<-sum(TieCount[,3])
}

#Calculating Sigma and Z
{if (Ties==FALSE) {SigS<-sqrt((N*(N-1)*(2*N+5))/18)}
 else {SigS<-sqrt(((N*(N-1)*(2*N+5))-Corr)/18)}
}

{if (S>0){ZS<-(S-1)/SigS}
 else if (S<0){ZS<-(S+1)/SigS}
 else {ZS=0}
}


{if (ZS>=0){Q<-pnorm(ZS)}
 else {ZS<-ZS*-1;Q<-pnorm(ZS)}
}
{if (N>10) {Res[6]<-2*(1-Q)}
 else {Res[6]<-paste("Too few values, consult Helsel and Hirsch (2002, App. B8) for S = ",
                     S, ", n = ", N)}
}

#Plotting data
#	opar <- par()
#	par(font.sub=3)
#	plot(Data[,1],Data[,2],type="p",main="Kendall-Theil Robust Linear Regression",sub=paste("Significance of correlation p = ",Res[6,1]),xlab=colnames(Data)[1],ylab=colnames(Data)[2],col="cornflowerblue",pch=16)
#	curve(Res[1,1]*x+Res[4,1],add=TRUE,col="darkgreen",lwd=2)

#Writing output table
if (Export==TRUE) {write.table(Res,Output,sep="\t")}
return(Res)
}
