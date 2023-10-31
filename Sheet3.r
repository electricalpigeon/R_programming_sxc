#1. q1as3.py: Input any sentence. Reverse it. Display modified sentence.
x <-readline(prompt = "Enter a sentence")
for (i in nchar(x):1){
cat(substring(x,i,i))}


#2. q2as3.py: Input any sentence/string. Print whether it is Palindrome/Not a Palindrome.
x <- readline(prompt = "Enter a sentence")
x <- tolower(gsub("[[:punct:][:space:]]", "", x))  # Remove punctuation and spaces and convert to lowercase
rev <- ""
for (i in nchar(x):1) {
  t <- substring(x, i, i)
  rev <- paste(rev, t, sep = "")
}

if (rev == x) {
  cat(x, "is a palindrome\n")
} else {
  cat(x, "is not a palindrome\n")
}

#6. q6as3.py : Write a program to calculate number of vowels in a sentence.
x <- readline(prompt = "Enter a sentence")
x <- tolower(gsub("[[:punct:][:space:]]", "", x))  # Remove punctuation and spaces and convert to lowercase
vowelCount <- 0

for (i in 1:nchar(x)) {
  t <- substring(x, i, i)
  if (t == "a" || t == "e" || t == "i" || t == "o" || t == "u") {
    vowelCount <- vowelCount + 1
  }
}

cat("The number of vowels in the sentence is:", vowelCount, "\n")


#7. q7as3.py: Write a program to calculate number of consonants in a sentence ‘x’
x <- readline(prompt = "Enter a sentence")
x <- tolower(gsub("[[:punct:][:space:]]", "", x))  # Remove punctuation and spaces and convert to lowercase
consonantCount <- 0

for (i in 1:nchar(x)) {
  t <- substring(x, i, i)
  if (t != "a" && t != "e" && t != "i" && t != "o" && t != "u") {
    consonantCount <- consonantCount + 1
  }
}

cat("The number of consonants in the sentence is:", consonantCount, "\n")


#8. q8as3.py: Input any sentence and extract all words from that sentence and display on screen.
x <- readline(prompt = "Enter a sentence ")
x<-paste(x," ")
wrd=""

for (i in 1:nchar(x)) {
  t <- substring(x, i, i)
  
  if ((t >="A" && t<="Z") || (t>="a" && t<="z")) {
    wrd<-paste(wrd,t,sep="")
  }
  if(t ==" "){
cat(wrd,"\n")
wrd="" }
}
