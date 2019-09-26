# Exemples de syntax R
# par Andrew 
# 19 Sept 2019

5 + 2
7 * 2
8 ^3

x <- 4

x * 5

y = 8
y + x

its_my_vector <- 1:7

its_my_vector * 3

paste("Andrew", "MacDonald")
paste("my fave number is", 3.14)


print_a_number <- function(number){
  number_to_print <- paste("this is number", number)
  print(number_to_print)
}

print_a_number(4)

print_a_number(its_my_vector)


