# SML AST Checker

Based largely off of [sml-style-check](https://github.com/jluningp/sml-style-check)
and further improvements seen in [here](https://github.com/15-150/15150/tree/4483dee0cc26c374e28633383553decf8f1531dd/scripts/onefifty/targets/grader/style-grader).

Currently there are two main utilities that can be used. A style checker, which
essentially walks the AST of a given file and looks for specific patterns, and
a TailRecursion checker, which takes an inputed AST and identifies all the
(expressions variable) declarations and classifies them as to whether they use
recursion, tail recursion, or no recursion.

The style checker can be used in two different ways:
- One can use the visible Grader structures to implement the style grader into
and autograding framework.
- One can use the `Top` and `Run` and the `./smlnj` to check files before they
are used in the REPL.

## Setup

This is currently used and tested using Standard ML of New Jersey v110.99, it
can potentially work on some other version, but these are untested.

We use the smackage packages [numbers](https://github.com/ProjectSavanna/ml-numbers.git)
and [autograding](https://github.com/ProjectSavanna/autograding.git). Here is a
brief guide to setting up smackage (with these packages):

1. Run the following commands:

    ```
    git clone https://github.com/standardml/smackage.git
    cd smackage
    make smlnj
    bin/smackage
    ```

    If this all works correctly, this should display:

    ```
    Smackage 1.4.5
    Usage: smackage <command> [args]
    ...
    ```

2. In `~/.bashrc` or `~/.zshrc`, put

    ```
    export PATH="$HOME/.smackage/bin:$PATH
    ```

    Then restart your terminal (or use `source`).

3. Run these commands:

    ```
    bin/smackage refresh
    bin/smackage make smackage smlnj
    bin/smackage make smackage install
    which smackage
    ```

4. Make a file in your home directory called `.smlnj-pathconfig` and put the
following inside the file:

    ```
    SMACKAGE .smackage/lib
    ```

5. Set-up the required packages:

    ```
    smackage source typeclasses git https://github.com/ProjectSavanna/typeclasses.git
    smackage source result git https://github.com/ProjectSavanna/result.git
    smackage source numbers git https://github.com/ProjectSavanna/ml-numbers.git
    smackage source autograding git https://github.com/ProjectSavanna/autograding.git
    ```

6. Run these, so that smackage can install the autograding package and all the
relevant dependencies:

    ```
    smackage get autograding
    ```

7. As a check, run `smlnj '$SMACKAGE/numbers/v1/sources.cm'`. When evaluating
`Natural.fromInt 5` you should get this result:

    ```
    Standard ML of New Jersey (64-bit) v110.99 [built: Thu Dec 24 11:47:23 2020]
    - Natural.fromInt 5;
    val it = Succ (Succ (Succ (Succ (Succ Zero)))) : Natural.t
    ```

8. In the future, for updates, run `smackage refresh` and `smackage update`.

## Style Checker

The style checker is organised into a number of rules which we can then check
depending on what is considered acceptable style for any given homework problem.
It can be loaded in use `sources.cm`.

A list of all the existing style rules can be found in the `AllRules` structure
defined (here)[main/rules/all_rules.sml], specificially the `datatype rule`
defines a list of all the rules supported.
  - Each of these rules is associated with a (checker)[checkers], which defines
  how to actually check for the rule
  - If a new rule is to be added, a new checker should be written and the `rule`
  datatype should be expanded, along with it being registered in the lists
  further down in the `AllRules` structure.

The functors `Rules`, `EnableRules`, and `DisableRules`, provide different ways
to create a structure ascribing to `RULES`, which can then be passed into the
`CustomStyleGrader` functor to create a grader.

If the style checker needs to be used with the REPL, there also exists the
functor `Run`, which takes in a structure ascribing to `RULES`, and `Top` which
provides an example of how one might use `Run`.

The `Top` structure provides `val run : string list -> int`, which takes in a
list of files (possibly `cm` files) and evaluates to the number of issues,
printing out associated information. It also provides a
`val export : unit -> unit` function which dumps a heap image called
`limit-sml.amd64-darwin` (the extension my differ depending on your OS) which
can be passed a list of files to check. The rules used by this can be adjusted
by changing how the `Use` structure is defined within `Top`.

The `./smlnj` script passes your files into the heap image (the name may need
to be updated) and evaluates them. If no style errors exists, then it passes
them into the `sml` REPL (with `rlwrap` if installed).

## Tail Recursion Checker

The Tail Recursion checker identifies all the (expression-level) declarations
within the file, and defines whether they use recursion, tail recursion, or are
non recursive. It can be used through the `tailrec.cm` file.

For general usage, the structure `BasicClassifier` is exported. This defines two
main driving functions, `classifyAst` and `simpleClassifyAst`. They both take in
an `Ast.dec` and output a list of bound variables and thier classifications as
NonRecursive, Recursive, or TailRecursive (the first function also outputs the
declaration where it is bound as well as region information).

For testing/playing around, it is likely better to use the `testing.cm` file and
then defining:
  ```sml
  val test = BasicClassifier.simpleClassifyAst o Parse.parseFile
  val results = test "tests/tailrec/basic.sml"
  ```
