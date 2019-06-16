# Idris 

## Install
* `cabal update; cabal install idris --force-reinstall`
	* Used force to be able to install, even though some dependencies may get broken.
* Find location idris installed (~/.cabal/bin in my case)
	* Either add location to $PATH or add alias (in ~/.bash_aliases)

## Getting started
Interactive environment: start with `idris`; similar to `ghci`

Commands in interactie environment:
* `:?` for all supported commands
* `:t` type check
* `:c` compile
* `:q` quit
* `:e` to open vim

Commands following `idris [file]`:
* `-o [prog]` to compile to file named `prog`
* `--check` to type check file and dependencies
* `--package [pack]` to add package as dependency

## Atom interactive editing
* `CTRL` `ALT` `A` for add-clause; generating initial pattern matching clauses
* `CTRL` `ALT` `C` for case-split; generating a case split for selected pattern variable
* `CTRL` `ALT` `S` for proof-search; attempting to generate solution to selected hole
* `CTRL` `ALT` `D` for documentation; generating documentation for selected variable
* `CTRL` `ALT` `R` for typechecking

