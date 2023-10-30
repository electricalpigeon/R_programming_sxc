#1. Write a program in python to input any +ve integer(2-2000). Print all prime numbers <=n.
is_prime=function(n){
n1<-floor(n/2)
flag<-1
if(n<2){
flag<-0
return(flag)
}
for (i in 2:n1){
r<-n%%i
	if(r==0){
	flag<-0
	break
	}
}
return(flag)
}
#main program starts
n<-as.integer(readline(prompt="Enter value of n(2-100)="))
for ( i in 2:n){
flag<-is_prime(i)
	if(flag==1){
	cat(i,' is Prime','\n')
	}
}



#2. Write a program in python to input any +ve integer(2-2000). Calculate and print all Fibonacci numbers<=n.
n<-as.integer(readline(prompt="Enter value of n(2-100)="))
n1=0
n2=1
cat(n1,"\n")
cat(n2,"\n")
sum=n1+n2
while(sum<=n){
	cat(sum,'\n')
	n1=n2
	n2=sum
	sum=n1+n2
	}



#3. Write a program in python to input any positive integer ‘n’ (2-999999999). Calculate and print all prime fibonacci numbers <=n.
is_prime=function(n){
for(i in 1:n){
	if(n%%i==0){
	ctr=ctr+1		
}}
if (ctr==2){
	return (1)}
else{return (0)}}
n<-as.integer(readline(prompt="Enter value of n(2-999999999)="))
ctr=0
n1=0
n2=1
sum=n1+n2
while(sum<=n){
	if (is_prime(sum)==1){
	cat(sum,'\n')}
	n1=n2
	n2=sum
	sum=n1+n2}



#4. Input value of ‘x’ (0.1-2.0). Calculate value of the following series:ex=1+x/1!+x^2/2!+x^3/3!+x^4/4!+……..
# Function to calculate the factorial of a number (as numeric)
factorial <- function(n) {
  if (n == 0) {
    return(1)
  } else {
    return(as.numeric(n * factorial(n - 1)))
  }
}

# Function to calculate e^x
calculate_ex <- function(x, terms) {
  result <- 1  # Initialize result with the first term (x^0/0!)
  
  for (i in 1:terms) {
    term <- (x^i) / factorial(i)  # Calculate the current term
    result <- result + term  # Add the current term to the result
  }
  
  return(result)
}

# Input value of 'x' (0.1-2.0)
x <- as.numeric(readline(prompt = "Enter the value of 'x' (between 0.1 and 2.0): "))

if (x >= 0.1 && x <= 2.0) {
  # Input the number of terms
  terms <- as.integer(readline(prompt = "Enter the number of terms: "))
  
  if (terms > 0) {
    # Calculate the value of e^x with the specified number of terms
    result <- calculate_ex(x, terms)
    
    cat("e^", x, " is approximately ", result, " using ", terms, " terms.\n")
  } else {
    cat("Please enter a valid positive number of terms.\n")
  }
} else {
  cat("Please enter a valid value of 'x' between 0.1 and 2.0.\n")
}



#5. Input value of ‘x’(0-360). Calculate and print sinx=x-x^3/3!+x^5/5!+x^7/7!+……
# Function to calculate the factorial of a number (as numeric)
factorial <- function(n) {
  if (n == 0) {
    return(1)
  } else {
    return(as.numeric(n * factorial(n - 1)))
  }
}

# Function to calculate sinx
flag=1
calculate_sinx <- function(x, terms) {
  result <- 0  
  x<-(pi*x/180)
  for (i in 1:terms) {
    term <- flag*(x^(2*i-1))/ factorial(2*i-1)  # Calculate the current term
    result <- result + term  # Add the current term to the result
    flag=flag*(-1)
  }
  
  return(result)
}

# Input value of 'x' (0 to 360)
x <- as.numeric(readline(prompt = "Enter the value of 'x' (between 0 and 360): "))


if (x >= 0 && x <= 360) {
  # Input the number of terms
  terms <- as.integer(readline(prompt = "Enter the number of terms: "))
  
  if (terms > 0) {
    # Calculate the value of sin x with the specified number of terms
    result <- calculate_sinx(x, terms)
    
    cat("sin ", x, " is approximately ", result, " using ", terms, " terms.\n")
  } else {
    cat("Please enter a valid positive number of terms.\n")
  }
} else {
  cat("Please enter a valid value of 'x' between 0 and 360.\n")
}



#6. Input value of ‘x’(0-360). Calculate and print cosx=1-x^2/2!+x^4/4!+x^6/6!+……
# Function to calculate the factorial of a number (as numeric)
factorial <- function(n) {
  if (n == 0) {
    return(1)
  } else {
    return(as.numeric(n * factorial(n - 1)))
  }
}

# Function to calculate cosx
flag=-1
calculate_cosx <- function(x, terms) {
  result <- 1  
  x<-(pi*x/180)
  for (i in 1:terms) {
    term <- (x^(2*i))/ factorial(2*i)  # Calculate the current term
    result <- result + flag*term  # Add the current term to the result
    flag=flag*(-1)
  }
  
  return(result)
}

# Input value of 'x' (0 to 360)
x <- as.numeric(readline(prompt = "Enter the value of 'x' (between 0 and 360): "))

if (x >= 0 && x <= 360) {
  # Input the number of terms
  terms <- as.integer(readline(prompt = "Enter the number of terms: "))
  
  if (terms > 0) {
    # Calculate the value of cos x with the specified number of terms
    result <- calculate_cosx(x, terms)
    
    cat("cos ", x, " is approximately ", result, " using ", terms, " terms.\n")
  } else {
    cat("Please enter a valid positive number of terms.\n")
  }
} else {
  cat("Please enter a valid value of 'x' between 0 and 360.\n")
}



#7. Input value of ‘x’(0-360). Calculate and print tanx=sinx/cosx [ Note: expression for sinx and cosx is given in Q.No.5,6 above].
#8. Calculate the value of PI. Where PI = 3½ * 2 (1 – (3^-1)/3 + (3^-2)/5 – (3^-3)/7 + (3^-4)/9 – (3^-5)/11 …….) → PI= 3.1415926535897932384
#9. Write a program which will print those 3 digit numbers where the sum of the cubes of the digits:1^3+5^3+3^3=153 
#10. Input n,r where n,r>=0 and r<=n. Calculate Binomial coefficient nCr= n!/(r! (n-r)!)
