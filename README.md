# Advent of Code 2019

Solutions for [Advent of Code 2019](https://adventofcode.com/2019/) in Clojure. 

I was going to use Haskell this year, and had learned enough of it to start working through last year's problems, but quickly ran out of steam on it. I feel like the strictness of Haskell makes it unnecessarily difficult to solve these types of puzzles. 


Plus, I find that the nature of Clojure more closely aligns with how I think about solving these kinds of problems, and the REPL workflow makes it really easy to iteratively come to a working solution.

## Usage

Solutions are designed to be run from the REPL. Each namespace exposes a `-main` function that reads the input, runs the code for each part, and prints the solution. The input is read from `resources/day##.txt`. My personal input is included in the repo, but can be swapped for your own for running locally.
