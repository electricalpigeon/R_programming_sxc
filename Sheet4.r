#1...: Write a program in R to copy an Input file to one output  file
fileold<-readline("Enter Your Input File Name=")
filenew<-readline("Enter Your output File Name=")
file.copy(fileold,filenew)
print("File Copy is over...")
#End of program


#2... Write a program which will split one file into two(2) output files.
# Define the input file and output file names
input_file <- "input.txt"
output_file1 <- "output1.txt"
output_file2 <- "output2.txt"

# Read the content of the input file
input_content <- readLines(input_file)

# Calculate the number of lines in the input file
total_lines <- length(input_content)

# Calculate the number of lines for each output file
lines_output1 <- ceiling(total_lines / 2)
lines_output2 <- total_lines - lines_output1

# Split the input content into two parts
content_output1 <- input_content[1:lines_output1]
content_output2 <- input_content[(lines_output1 + 1):total_lines]

# Write the split content to the output files
writeLines(content_output1, output_file1)
writeLines(content_output2, output_file2)

# Print a message to confirm the split
cat("Input file split into two output files:", output_file1, "and", output_file2, "\n")




#4... : Write a program in R to display number of new lines in a text file
file1<-readline("Enter your input file name=")
fp1<-file(file1,"r")
x<-readLines(fp1) # Reding data from file1 and storing in 'x'
nol<-length(x)
cat("Number of New Lines in ",file1,"=",nol,"\n")
close(fp1)
#End of program


#6... : Write a program in R to reverse the content any file. display the reverse data on screen
file1<-readline("Enter your input file name=")
fp1<-file(file1,"r")
x<-readLines(fp1)
nol<-length(x)
n<-file.info(file1)$size # n=size of input file(in number of bytes)
cat("original content=\n",x)
cat("\nreverse content=\n")
for(i in n:1){
y<-substring(x,i,i)
cat(y)
}
cat("\n")
cat("Size of ",file1,"=",n," Bytes", " Number of Lines=",nol,"\n")
close(fp1)
#end of program


#7a : Write  a progrm in R to convert all small letters to capital letters  in any program/file. Write the updataed data in some output file.
file1<-readline("Enter Input File Name=")
file2<-readline("Enter Output File Name=")
fp1<-file(file1,"r")
fp2<-file(file2,"w")
x<-readLines(fp1)
y<-toupper(x) # To convert all small letters to capital letters
n<-file.info(file1)$size # n=size of input file
writeLines(y,fp2) # Transferring modified data on output file
cat("Modified data==>\n",y)
cat("Size of ",file1,"=",n," Bytes\n")
close(fp1)
close(fp2)

#7b... : Write  a progrm in R to convert all capital letters to small letters  in any program/file. Write the updataed data in some output file.
file1<-readline("Enter Input File Name=")
file2<-readline("Enter Output File Name=")
fp1<-file(file1,"r")
fp2<-file(file2,"w")
x<-readLines(fp1)
y<-tolower(x) # To convert all capital letters to small letters
n<-file.info(file1)$size # n=size of input file
writeLines(y,fp2) # Transferring modified data on output file
cat("Modified data==>\n",y)
cat("Size of ",file1,"=",n," Bytes\n")
close(fp1)
close(fp2)

#8...: Write a program to extract words form any text file and store in another file. Display all words and also number of words in that file.
file1<-readline("Enter your Input File Name=")
file2<-readline("Enter your output File Name=")
fp1<-file(file1,"r")
fp2<-file(file2,"w")
x<-readLines(fp1) # To read entire file in x
n<-file.info(file1)$size # n=Size of input file
#To extract words from input file
nlength<-length(x) # nlength=Number of rows in input file
nw<-0
word<-""
for(i in 1:nlength){
nn<-nchar(x[i])
flag<-1# flag=1 means we read 1st alphabet of a word
	for(j in 1:nn)
	{
	x1<-substring(x[i],j,j)
	y1<-toupper(x1) # converting charcater to upper case
		if(y1>="A" && y1<="Z"){
			if(flag==1){
			nw<-nw+1
			flag<-0
			}
		word<-paste(word,x1,sep="")
		#printing of word starts
		} else if(flag==0){
		cat(word,"\n")
		writeLines(word,fp2)
		word<-""
		flag<-1
		}
	}
}
cat("\nTotal number of words=",nw,"\n")
close(fp1)
close(fp2)


#10(a)...: Write a program to input (i) File name, (ii) Pattern to be searched, (iii) new
#Pattern to be substituted. Search the entire file for the given pattern and change it by new pattern.
#q10aas4rprogram.r : Write a program to modify old pattern by a new pattern in any prorgam/file.
file1<-readline("Enter Input File Name=")
file2<-readline("Enter Output File Name=")
oldpat<-readline("Enter your Old pattern to be modified=")
newpat<-readline("Enter your New pattern to be substituted=")
fp1<-file(file1,"r")
fp2<-file(file2,"w")
x<-readLines(fp1) # x=old data
y<-gsub(oldpat,newpat,x)
writeLines(y,fp2)
print("File updateion is over...")
close(fp1)
close(fp2)

#10.(b) ... Create a telephone directory which should contain (i) Name , (ii) email,
#(iii) mobile. Fields must be separated by,(csv file)..add header at the top of file

file1<-readline("Enter your telephone Directory Name=")
fp1<-file(file1,"w")      # opening file1 in write mode
cnt<-0                          # cnt=number of records added onto output file
header<-"Name, Email, Mobile"
writeLines(header,fp1)    # Writing header on output file
  while(TRUE)
  {
  name1<-readline("Enter Name=")
  email1<-readline("Enter Email=")
  mobile1<-readline("Enter Mobile#=")
  x<-paste(name1,email1,mobile1,sep=",")
  writeLines(x,fp1)
  cnt<-cnt+1
  choice<-readline("Do you want to add any more record(Y/N?) :")
    if(choice!='Y' && choice!='y'){
    break
    }
  }
close(fp1) # Closing connection between program and the external file
cat("Total number of records added=",cnt,"\n")
#End of Program

