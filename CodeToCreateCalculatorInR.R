#calculatorr


user_input1 <- as.integer(readline(prompt = "Enter a number: "))
user_input2 <- as.integer(readline(prompt = "Enter a number: "))
user_input3 <- (readline(prompt = "Enter the arithmetic operation: "))


for(input_1 in user_input1){
  for(input_2 in user_input2){
    for(input_3 in user_input3){
      if(input_3 == "+"){
        A <- input_1 + input_2
        print(A)
        
      } else if(input_3== "-"){
        B <- input_1 - input_2
        print(B)
    } else if(input_3=="*"){
        C <- input_1 * input_2
        print(C)
        
      }else if(input_3=="/"){
        D <- input_1/input_2
        print(D)
      }else if(input_3=="sqrt"){
        E <- sqrt(input_1)
        print(E)
  }
}
        
}
      
}  




 
  







