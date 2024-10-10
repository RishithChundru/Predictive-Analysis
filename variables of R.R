a<-10
a
class(a)
typeof(a)
b<-23.4
b
class(b)
typeof(b)
c<-10L
c
class(c)
typeof(c)
d<-"hi"
d
class(d)
typeof(d)
e<-3+4i
e
class(e)
typeof(e)
f<-TRUE
f
class(f)
typeof(f)

# Assigning
a<-b<-10
a
b
cat("the value of a is",a,"the value of b is",b)
paste(a,b)
print(c(a,b))


a<-c(20L,TRUE)
a
class(a)
# 1) Character
# 2) Complex
# 3) Numeric
# 4) Integer
# 5) Logical


## Arithmetic operations
a<-c(1,2,3,4,5)
b<-c(6,7,8,9,0)
print(c(a+b,a-b,a*b,a/b,a%/%b,a%%b,a^b))
print(a-b)
print(a*b)
print(a/b)
print(a%/%b)
print(a%%b)
print(a^b)


# consider three vectors and one contains the patients names(3) ans second stores temp and third stores flu status.print the temp of last two patients, exclude temp of second patient and print temp of those patients where value is true then print names of patients with certain range.

a<-c('Sai','Ram','Jay')
b<-c(34.5,36.7,35.4)
c<-c(TRUE,TRUE, FALSE)
b[2:3]
b[-2]
a[1:2]
d<-b[c==TRUE]
print(d)


# Relational Operators
a<-c(TRUE,TRUE,FALSE,FALSE)
b<-c(TRUE,FALSE,TRUE,FALSE)
print(a&b)
print(a|b)
print(!a)
print(a&&b)
print(a||b)
  

# WAP to find a number is  even or odd
{
a<-as.numeric(readline())
if(a%%2==0){
  print("Even")
}else{
  print("Odd")
}
}

# wap to find roots of quadratic equation
{
a<-1
b<-2
c<-3
if(((b*b)-4*a*c)==0){
  r1<--b/2*a
  r2<--b/2*a
  print(r1)
  print(r2)
  print("roots are real and equal")
}else if(((b*b)-4*a*c)>0){
  r1<-(-b-(a**1/2))/2*a
  r2<-(-b+(a**1/2))/2*a
  print("Roots are real and distinct")
}else{
  print("Roots are imaginary")
}
}
