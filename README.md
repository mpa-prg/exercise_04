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
* Implement the algorithm in R as a function `ReturnCoins()`.

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
* Implement the pseudocode in R as a separate function `UniversalReturnCoins()`. 
* Find input values for which the algorithm will not work correctly, meaning the output will be incorrect.

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
7   HanoiTowers(n – 1, unusedPeg, toPeg)
8   return
```


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
