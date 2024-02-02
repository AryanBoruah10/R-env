

#Print all the Fibonacci numbers smaller the number you entered
n <- as.numeric(readline(prompt = "Enter a number: "))

a=0
b=1
c=0

while (c<=n) {
  print(c)
  c <- a+b
  a <- b
  b <- c
  
}