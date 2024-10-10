{
a<-readline("Enter a number: ");
b=1;
if(a==0){
  b=1;
}else{
for(i in 1:a){
  b=b*i;
}
}
print(b)
}
