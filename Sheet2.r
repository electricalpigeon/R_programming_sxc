#1...Write a program to input 10 arbitrary numbers to store in a list arr[] and also number to searched in arr[]. Apply
#Binary Search algorithm to search a number in arr[] and display whether the number found or not found.

binarySearch = function(arr,item) {
    low <- 1; high <- length(arr)
    while (low <= high){
        mid <- as.integer(round((low + high) / 2)) 
        if (abs(arr[mid] - item) ==0) {
            return(mid)
        } else if (arr[mid] < item) {
            low <- mid + 1
        } else {
            high <- mid - 1
        }
    }
    return(0)
}
arr<-integer()
for (i in 1:10){
a<-as.integer(readline("Enter number="))
arr<-append(arr,a)}
item<-as.integer(readline("Enter number to be searched="))
sorted_arr <- sort(arr)

cat("Array ", arr, "\nSorted array ",sorted_arr,"\nitem = ", item, "\n")
index <- binarySearch(sorted_arr, item)
if (index!=0){
    cat("Element is present at index ", index, "\n")
}else{
    cat("element not found")
}


#2... Write a program to sort numbers using Merge-Sort algorithm.
# function to merge two sorted arrays
merge <- function(a, b) {
      # create temporary array
    temp <- numeric(length(a) + length(b))
   
      # take two variables which initially points to 
      # starting of the sorted sub arrays 
      # and j which points to starting of starting 
      # of temporary array 
    astart <- 1 
      bstart <- 1 
      j <- 1
    for(j in 1 : length(temp)) {
         # if a[astart] < b[bstart]
        if((astart <= length(a) && 
            a[astart] < b[bstart]) || 
            bstart > length(b)) {
              # insert a[astart] in temp and increment 
              # astart pointer to next
            temp[j] <- a[astart]
            astart <- astart + 1
        } 
      else {
            temp[j] <- b[bstart]
            bstart <- bstart + 1          
        }
    }
    temp
}
 
# function to sort the array
mergeSort <- function(arr) {
   
      # if length of array is greater than 1, 
      # then perform sorting
    if(length(arr) > 1) {
       
          # find mid point through which 
          # array need to be divided
        mid <- ceiling(length(arr)/2)
       
          # first part of array will be from 1 to mid
        a <- mergeSort(arr[1:mid])
       
         # second part of array will be 
          # from (mid+1) to length(arr)
        b <- mergeSort(arr[(mid+1):length(arr)])
       
          # merge above sorted arrays
        merge(a, b)
    }
  # else just return arr with single element
  else {
        arr
    }
}
 

arr <- integer()
n<-as.integer(readline("Enter number of elements"))
for (i in 1:n){
el<-as.integer(readline("Enter the element "))
arr<-append(arr,el)}
 
# call mergeSort function
result <- mergeSort(arr)
 
# print result
print(result)


#3...Write a program to sort numbers using Insertion Sort algorithm.
# insertion sort function to sort array
insertion_sort <- function(x)
{
      # calculate the length of array
    n <- length(x) 
      # outer loop
    for (i in 2 : (n))
    {
          # store first element as key
        key = x[i]
        j   = i - 1
          # compare key with elements for 
          # its correct position 
        while (j > 0 && x[j] > key)
        { 
          x[j + 1] = x[j] 
          j = j - 1 
        }
      # Place key at its correct position
      x[j + 1] = key
    }
      # return sorted array
    x
}
 

arr <- integer()
n<-as.integer(readline("Enter number of elements"))
for (i in 1:n){
el<-as.integer(readline("Enter the element "))
arr<-append(arr,el)}
 
# call insertionSort function
result <- insertion_sort(arr)
 
# print result
print(result)


#4...Write a program to implement Towers of Hanoi algorithm to move ‘n’ disks from peg-1 to peg-2.

#Write a function to implement towers of hanoi algorithm
n<<-0 # defining n as global parameter.
hanoi=function(p1,p2,p3,nd){
	if(nd==1){
		n<<-n+1
		cat("Step-",n," : Move Disk-",nd," from peg-",p1," to peg-",p2,"\n")
		return(n)
	}else{ hanoi(p1,p3,p2,(nd-1))
		n<<-n+1
		cat("Step-",n," : Move Disk-",nd," from peg-",p1," to peg-",p2,"\n")
		hanoi(p3,p2,p1,(nd-1))
		}
	}

#5...Write a program to implement recursive call to calculate n-th member of a fibonacci series.
#Fibonacci Function
fib=function(a){
n1=0
n2=1
if (a==1){ return(0)}
if (a==2){return(1)}
s=0
for (i in 3:a){
s=n1+n2
n1=n2
n2=s}
return(s)
}
#display fibonacci function
a<-as.integer(readline(" Enter your n: "))
print(fib(a))


#6...Write a program to store ‘n’ arbitrary integers in a list a[]. Calculate the frequency of all number and display all distinct numbers and its corresponding frequency.

# Function to calculate and display frequency of distinct numbers
calculate_frequency <- function(numbers) {
  unique_numbers <- unique(numbers)  # Get distinct numbers
  freq <- sapply(unique_numbers, function(x) sum(numbers == x))  # Calculate frequency
  
  cat("Distinct numbers and their frequencies:\n")
  for (i in 1:length(unique_numbers)) {
    cat("Number:", unique_numbers[i], " Frequency:", freq[i], "\n")
  }
}

# Input the number of integers 'n'
n <- as.integer(readline("Enter the number of integers (n): "))

# Create an empty list to store integers
a <- vector("integer", n)

# Input 'n' integers
for (i in 1:n) {
  a[i] <- as.integer(readline(paste("Enter integer #", i, ": ")))
}

# Calculate and display the frequency of distinct numbers
calculate_frequency(a)


#7...Write a program to convert any +ve number(1-3000) and convert the number into corresponding Roman numerals.


# Function to convert a positive number to Roman numerals
number_to_roman <- function(num) {
  if (num < 1 || num > 3000) {
    return("Number out of range (1-3000)")
  }

  roman_numerals <- c("M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I")
  arabic_numbers <- c(1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1)

  roman_numeral <- ""
  i <- 1

  while (num > 0) {
    if (num >= arabic_numbers[i]) {
      roman_numeral <- paste(roman_numeral, roman_numerals[i], sep = "")
      num <- num - arabic_numbers[i]
    } else {
      i <- i + 1
    }
  }

  return(roman_numeral)
}

# Input from the user
number <- as.integer(readline("Enter a positive number (1-3000): "))

# Convert the number to Roman numerals
roman_numeral <- number_to_roman(number)

# Display the result
cat("Roman numeral:", roman_numeral, "\n")


#8...Write a program to input any character and convert the character into bits and display character and its corresponding bits.
# Input a character from the user
char_input <- readline("Enter a character: ")

# Convert the character to its binary representation
binary_representation <- intToBits(charToRaw(char_input))

# Display the character and its corresponding bits
cat("Character:", char_input, "\n")
cat("Binary Representation:", paste0(as.integer(binary_representation), collapse = ""), "\n")


#9...Write a program in python to input ‘n’ (where n>=1 and n<=10) numbers. Apply Bubble Sort method to
#sort those numbers and display sorted list. The program must also display how many comparisons required
#to sort 10 elements. Extend the code to sort ‘n’ names in alphabetical order.


# Function to perform Bubble Sort on a numeric vector
bubbleSort <- function(arr) {
  n <- length(arr)
  comparisons <- 0
  for (i in 1:(n - 1)) {
    for (j in 1:(n - i)) {
      if (arr[j] > arr[j + 1]) {
        temp <- arr[j]
        arr[j] <- arr[j + 1]
        arr[j + 1] <- temp
      }
      comparisons <- comparisons + 1
    }
  }
  return(list(sorted_list = arr, comparisons = comparisons))
}

# Input 'n' numbers from the user
n <- as.integer(readline("Enter the value of n (between 1 and 10): "))
if (n < 1 || n > 10) {
  cat("n should be between 1 and 10.\n")
} else {
  numbers <- numeric(n)
  names <- character(n)

  for (i in 1:n) {
    numbers[i] <- as.numeric(readline(paste("Enter number ", i, ": ")))
  }

  for (i in 1:n) {
    names[i] <- readline(paste("Enter name ", i, ": "))
  }

  # Sort the numeric vector using Bubble Sort
  result_numeric <- bubbleSort(numbers)
  cat("Sorted numbers:", result_numeric$sorted_list, "\n")
  cat("Number of comparisons required to sort numbers:", result_numeric$comparisons, "\n")

  # Sort the names in alphabetical order
  sorted_names <- sort(names)
  cat("Sorted names:", sorted_names, "\n")
}


