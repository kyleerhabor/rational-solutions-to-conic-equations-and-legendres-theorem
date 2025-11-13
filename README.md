# Rational Solutions to Conic Equations and Legendre’s Theorem

A script that determines when a conic of the form $aX^2 + bY^2 = cZ^2$ defined over $Q$ has a rational solution.

## Install

1. [Install Clojure.](https://clojure.org/guides/install_clojure)

2. Clone the repository:
```sh
git clone https://github.com/kyleerhabor/rational-solutions-to-conic-equations-and-legendres-theorem.git
```

3. Run the program:
```sh
clojure -M -m kyleerhabor.rational-solutions-to-conic-equations-and-legendres-theorem.core --help

# [a]X^2 + [b]Y^2 = [c]Z^2
clojure -M -m kyleerhabor.rational-solutions-to-conic-equations-and-legendres-theorem.core -a [a] -b [b] -c [c]
```
