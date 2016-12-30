Lambda Calculus
===============

This is a simple Lambda Calculus parser and evaluator based on Stephen Diehl's
[Write you a Haskell](http://dev.stephendiehl.com/fun/003_lambda_calculus.html#full-source).

## Example

```console
$ cabal run
λ > (\ x . x) 1
 => λ x . x
 => 1
 => x
1
λ > (\f x . x) 1 0
  => λ f x . x
  => 1
 => λ f x . x 1
 => 0
 => x
0
```

## Installation

### Dependencies

- [Haskell](https://www.haskell.org)
- [cabal](https://www.haskell.org/cabal/)

Install these using:

- Mac OS - Homebrew: `brew install ghc cabal-install`
- Debian (Ubuntu, Mint, etc.): `sudo apt-get install haskell-platform`

### Build and install project

Simply run the following once `cabal` is installed:

```console
git clone git@github.com:Jiggins/LambdaCalculus.git
cd LambdaCalculus
cabal install
```

This will create an executable in either `~/Library/Haskell/bin/` (Mac OS only)
or `~/.cabal/bin/`.

### Run the project

```console
$ cabal run
```
