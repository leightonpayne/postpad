
<!-- README.md is generated from README.Rmd. Please edit that file -->

# postpad

<!-- badges: start -->
<!-- badges: end -->

## Installation

You can install the development version of postpad from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("leightonpayne/postpad")
```

## Installation (NeSI)

First, activate the latest `R` module:

``` bash
module R
```

The package `devtools` can’t be installed on NeSI for some reason, so
install `remotes` instead. This, and the following packages, will be
installed in a user library (the default is `~/R/<toolchain>/<version>`,
e.g.  `~/R/gimkl-2022a/4.2`). You can do the following in an interactive
R session or by calling `Rscript` as below:

``` bash
Rscript -e 'install.packages("remotes")'
```

Then use `remotes` to install `postpad` from GitHub:

``` bash
Rscript -e 'remotes::install_github("leightonpayne/postpad")'
```

> TLDR; Installing the dependency `stringi` will fail. This can either
> be ignored every time `postpad` is installed/updated, or you can
> install `stringi` once using
> `install.packages("stringi", configure.args="--disable-pkg-config")`.

`remotes` will also install all dependencies into the user library. One
of the dependencies it tries to install is `stringi`, which fails
because `stringi` requires the (C/C++ library
`ICU`)\[<https://icu.unicode.org/>\], which for some reason is either
not installed or not accessible. This is annoying because every time
`postpad` is updated with
`remotes::install_github("leightonpayne/postpad")`, it will try to build
and install `stringi`, which takes a couple of minutes to fail.
**Technically, this does not really matter, as `stringi` is already
installed as part of the `R` module (at least the latest version
`R/4.2.1-gimkl-2022a`).** So you can choose to just ignore this error
every time you update `postpad`. Alternatively, you can force `stringi`
to use the `ICU4C 69.1` library that is conveniently (shipped with the
package)\[<https://stringi.gagolewski.com/install.html>\], by installing
stringi with the command:
`install.packages("stringi", configure.args="--disable-pkg-config")`.
