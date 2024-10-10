row<-c('row1','row2')
col<-c('col1','col2','col3')
matrix1<-matrix(1:6,nrow=2,ncol=3,dimnames = list(row,col))
matrix2<-matrix(7:12,nrow=2,ncol=3,dimnames=list(row,col))
matrix1
matrix2
matrix1+matrix2
matrix1[1,]
matrix2[2,3]
