# Iterative and Recursive Algorithms

## The Change Problem
### Task 1
* Convert some amount of money into the fewest number of coins.

* Input:
    * an amount of money `M` to be returned

* Output:
    * the minimum number of coins whose total value will be equal to `M`

* Resources:
    * coins in denominations of 50 (`pd`), 20(`dc`), 10(`ds`), 5(`p`), 2(`d`) and 1(`j`) CZK

* Condition:
    * `50 * pd + 20 * dc + 10 * ds + 5 * p + 2 * d + 1 * j = M`, where `pd`, `dc`, `ds`, `p`, `d` and `j` must have the smallest possible value

Detailed description:
```
ReturnCoins(M)
1   Give the customer the integer result of dividing M by 50 in 50 CZK coins.
2   Let remainder be the remaining amount due the customer.
3   Give the customer the integer result of dividing remainder by 20 in 20 CZK coins.
4   Let remainder be the remaining amount due the customer.
5   Give the customer the integer result of dividing remainder by 10 in 10 CZK coins.
6   Let remainder be the remaining amount due the customer.
7   Give the customer the integer result of dividing remainder by 5 in 5 CZK coins.
8   Let remainder be the remaining amount due the customer.
9   Give the customer the integer result of dividing remainder by 2 in 2 CZK coins.
10  Give the customer remainder after division in 1 CZK coins.
```

* Write pseudocode according to previous detailed description of the algorithm.
```
ReturnCoins(M)
1   pd ← M div 50
2   remainder ← M mod 50
3   dc ← remainder div 20
4   remainder ← remainder mod 20
5   ds ← remainder div 10
6   remainder ← remainder mod 10
7   p ← remainder div 5
8   remainder ← remainder mod 5
9   d ← remainder div 2
10  j ← remainder mod 2
11  return (pd, dc, ds, p, d, j)
```

* Implement the algorithm in R as a function `ReturnCoins()`.
ReturnCoins <- function(M) {
  pd <- M %/% 50
  remainder <- M %% 50
  
  dc <- remainder %/% 20
  remainder <- remainder %% 20
  
  ds <- remainder %/% 10
  remainder <- remainder %% 10
  
  p <- remainder %/% 5
  remainder <- remainder %% 5
  
  d <- remainder %/% 2
  j <- remainder %% 2
  
  result <- list(
    "50_CZK" = pd,
    "20_CZK" = dc,
    "10_CZK" = ds,
    "5_CZK" = p,
    "2_CZK" = d,
    "1_CZK" = j,
    "Total_coins" = pd + dc + ds + p + d + j
  )
  
  return(result)
}

ReturnCoins(137)


### Task 2
* Convert some amount of money `M` into given denominations, using the smallest possible number of coins.

* Input:
    * an amount of money `M` to be returned
    * an array of `d` denominations `c = (c1, c2, ... , cd)` in descending order `(c1 > c2 > ··· > cd)`

* Output:
    * integer values `i1, i2, ... , id`

* Condition:
    * `c1 * i1 + c2 * i2 + ··· + cd * id = M`, where `i1 + i2 + ··· + id` is as small as possible

* Write a pseudocode for the change problem for any currency. Hint: use array indexing.
```
UniversalReturnCoins(M, c[1..d])
1   for k ← 1 to d
2       i[k] ← M div c[k]
3       M ← M mod c[k]
4   return i[1..d]
```

* Implement the pseudocode in R as a separate function `UniversalReturnCoins()`.
UniversalReturnCoins <- function(M, c) {
  d <- length(c)
  i <- numeric(d)  # store number of coins for each denomination
  
  for (k in 1:d) {
    i[k] <- M %/% c[k]   # number of coins of this denomination
    M <- M %% c[k]       # remaining amount
  }
  
  result <- data.frame(
    Denomination = c,
    Coins = i
  )
  
  result$Total_Value <- result$Denomination * result$Coins
  result$Total_Coins <- sum(i)
  
  return(result)
}

denominations <- c(50, 20, 10, 5, 2, 1)
UniversalReturnCoins(137, denominations)

* Find input values for which the algorithm will not work correctly, meaning the output will be incorrect.
denominations <- c(4, 3, 1)
UniversalReturnCoins(6, denominations)


## The Most Chocolate Path
### Task 3
* In R, implement a recursive function `Chocolate()` according to the following pseudocode.

* Input:
    * a matrix with integer values (number of chocolate bars)
    * an index of current row
    * an index of current column

* Output:
    * the maximum number of chocolate bars, that can be collected

* Solve the same problem iteratively.

```
Chocolate(M, r, c)
1   if r = number of rows in M
2     return M[r, c]
3   else
4     bars ← M[r, c]
5     down ← Chocolate(M, r + 1, c)
6     diagonal ← Chocolate(M, r + 1, c + 1)
7     return max(down, diagonal) + bars
```
# recursive
Chocolate <- function(M, r, c) {
  rows <- nrow(M)
  cols <- ncol(M)
  
  # Base case: if we’re at the last row
  if (r == rows) {
    return(M[r, c])
  }
  
  # If we move straight down
  down <- Chocolate(M, r + 1, c)
  
  # If we move diagonally down-right (check boundary)
  diagonal <- 0
  if (c + 1 <= cols) {
    diagonal <- Chocolate(M, r + 1, c + 1)
  }
  
  bars <- M[r, c]
  return(bars + max(down, diagonal))
}

M <- matrix(
  c(3, 2, 1,
    5, 10, 4,
    7, 6, 8),
  nrow = 3, byrow = TRUE
)

Chocolate(M, 1, 1)

# iterative
ChocolateIterative <- function(M) {
  rows <- nrow(M)
  cols <- ncol(M)
  
  # Create a copy of M to store the maximum chocolates from each cell
  dp <- M
  
  # Process rows from bottom-2 to top
  for (r in (rows - 1):1) {
    for (c in 1:cols) {
      down <- dp[r + 1, c]
      diagonal <- if (c + 1 <= cols) dp[r + 1, c + 1] else 0
      dp[r, c] <- M[r, c] + max(down, diagonal)
    }
  }
  
  # The maximum number of chocolates starting from the top-left
  return(dp[1, 1])
}

ChocolateIterative(M)



## The Towers of Hanoi
### Task 4
* In R, implement a function `HanoiTowers()` according to pseudocode. 

* Input:
    * a number of discs
    * an index of starting peg
    * an index of a peg, where all disks will be to moved to

* Output:
    * a sequence of steps to solve the towers of Hanoi problem

```
HanoiTowers(n, fromPeg, toPeg)
1   if n = 1
2     output "Move disc from peg fromPeg to peg toPeg"
3     return
4   unusedPeg ← 6 – fromPeg – toPeg
5   HanoiTowers(n – 1, fromPeg, unusedPeg)
6   output "Move disc from peg fromPeg to peg toPeg"
7   HanoiTowers(n – 1, emptyPeg, toPeg)
8   return
```

HanoiTowers <- function(n, fromPeg, toPeg) {
  # Base case: only one disc
  if (n == 1) {
    cat("Move disc from peg", fromPeg, "to peg", toPeg, "\n")
    return()
  }
  
  # Determine the auxiliary peg
  unusedPeg <- 6 - fromPeg - toPeg  # since 1 + 2 + 3 = 6
  
  # Step 1: move n-1 discs from 'fromPeg' to 'unusedPeg'
  HanoiTowers(n - 1, fromPeg, unusedPeg)
  
  # Step 2: move the remaining largest disc
  cat("Move disc from peg", fromPeg, "to peg", toPeg, "\n")
  
  # Step 3: move the n-1 discs from 'unusedPeg' to 'toPeg'
  HanoiTowers(n - 1, unusedPeg, toPeg)
}

HanoiTowers(3, 1, 3)


<details>
<summary>Download files from GitHub</summary>
<details>
<summary>Basic Git settings</summary>

>* Configure the Git editor
>    ```bash
>    git config --global core.editor notepad
>    ```
>* Configure your name and email address
>    ```bash
>    git config --global user.name "Zuzana Nova"
>    git config --global user.email z.nova@vut.cz
>    ```
>* Check current settings
>    ```bash
>    git config --global --list
>    ```
>
</details>

* Create a fork on your GitHub account. 
  On the GitHub page of this repository find a <kbd>Fork</kbd> button in the upper right corner.
  
* Clone forked repository from your GitHub page to your computer:
```bash
git clone <fork repository address>
```
* In a local repository, set new remote for a project repository:
```bash
git remote add upstream https://github.com/mpa-prg/exercise_04.git
```

#### Send files to GitHub
Create a new commit and send new changes to your remote repository.
* Add file to a new commit.
```bash
git add <file_name>
```
* Create a new commit, enter commit message, save the file and close it.
```bash
git commit
```
* Send a new commit to your GitHub repository.
```bash
git push origin main
```

</details>