# create a list having 4 kinds of datatypes: string, vector, logical and numeric. After printing data items of these list. add an item to this existing list. enter any random string find out whether the string is present in given list or not. Remove third item from the existing list.
a<-list("Ram",c(1,2,3),TRUE,23.45)
a
a[5]=12
a
a%in%"Ram"
a[-3]


# Create a 2x4 matrix with a random numbers and transpose this matrix. multiply,

# create a dataframe with columns for name,age,gender add a new column for occupation(5 values). write a program  to filter rows where a numeric column is greater than specified value.


a<-matrix(1:8,nrow=2,ncol=4)
b<-matrix(9:16,nrow=2,ncol=4)
a
dim(a)
t(a)
t(b)
a*b

a<-data.frame(name=c("Ram","Sai","kiran","varun","soumya"),
              age=c(19,20,34,21,29),
              gender=c("Male","Male","Male","Male","Female"))
a
occupations=c("Employee","Teacher","Farmer","Daily Wager","Businessman")
cbind(a,occupations)
b<-a[a[2]>25]
b



# transpose
a <- matrix(1:8, nrow=2, ncol=4)
b <- matrix(9:16, nrow=2, ncol=4)
a
transpose_matrix <- function(mat) {
  nrow <- ncol(mat)
  ncol <- nrow(mat)
  result <- matrix(0, nrow=nrow, ncol=ncol)
  
  for (i in 1:nrow) {
    for (j in 1:ncol) {
      result[i, j] <- mat[j, i]
    }
  }
  return(result)
}

a_transposed <- transpose_matrix(a)
b_transposed <- transpose_matrix(b)

print(a_transposed)
print(b_transposed)
