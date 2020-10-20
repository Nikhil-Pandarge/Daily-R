c(1,2) == 1
costs <- c(3,15,3,10)
costs[costs>5]

john <- list("studentId" = 9, "Age" = 18, "Courses" = c("DS","R"))
john[3]
john$Courses
john["Courses"]

array("student" = c("john", "mary"), "id" = c(1, 2))


chance_perception <- 0.80 

if(chance_perception < 0.5)
  {
  print("Bring an Umbrella")
}else{
  print("Dont!")
}
char_vec <- c("Nikhil","Nagesh","Pandarge")

nchar(char_vec)
char_vec.nchar()
length(nchar)
length(char_vec)
numberOfCharacters[char_vec]

hw <- c("Hello","World")
paste("Hello","World")
paste(hw,collapse = " ")

value_of_Date <- c("2020-01-01")
value_Date <- as.Date(value_of_Date)
typeof(value_Date)


library(swirl)

install.packages("swirl")

install_course("The R Programming Environment")

swirl()
