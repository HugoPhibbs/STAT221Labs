fibonacci = function(n){
  f_n_2_less = 0
  f_n_1_less = 1
  
  if (n==0){
    return(f_n_2_less)
  }
  else if (n==1){
    return(f_n_1_less)
  }
  
  f_n = 0
  
  for (i in 2:n){
    f_n = f_n_2_less + f_n_1_less
    print(f_n)
    f_n_2_less = f_n_1_less
    f_n_1_less = f_n
  }
  return(f_n)
}

print(fibonacci(7))

