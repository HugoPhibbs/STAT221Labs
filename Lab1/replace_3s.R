x = c(1, 2, 3, 6, 9)

for (i in 1:length(x)){
  if (x[i] % 3 == 0 && x[i] >= 3){
    x[i] = NA
  }
}

print(x)

