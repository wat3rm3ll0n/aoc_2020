# Part 1
input<- scan("day_1/input.txt")

check_expenses <- function(expenses, entry_count, goal){
  # Create all combinations of m size, select the two that sum to the goal amount, and multiply them together
  solution <- prod(combn(expenses, entry_count)[, which(colSums(combn(expenses, entry_count)) == goal)])
}

print(check_expenses(input, 2, 2020))

# Part 2
print(check_expenses(input, 3, 2020))
