#R CHEAT_Sheet

if (condition){
Do something
} else {
Do something different
}


if (i > 3){
  print('Yes')
} else {
  print('No')
}

for (i in 1:4){
  j <- i + 10
}

while (i < 5){
  print(i)
  i <- i + i
}


square <- function(x){

  squared <- x*x
  return(squared)
}
