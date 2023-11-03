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


#4...
