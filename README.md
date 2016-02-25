# Cuckoo Search Clojure

A Clojure library designed to solve optimization problems by Cuckoo Search Algorithm.

## Usage


```clojure
(ns my-project.core
  (:gen-class)
  (:require [cuckoo-search-clojure.cuckoo-search :refer :all]))


(search (create-cuckoo-search
        15 ; nd
        25 ; eggs
        (partial random -5 5) ; gen
        (fn [{solution :solution}] ; fun: d-dimensional sphere function sum_j=1^d (u_j-1)^2, with a minimum at (1,1, ...., 1)
          (reduce #(+ % (* (- %2 1) (- %2 1))) 0 solution))
        (fn [{fitness :fitness}] (<= fitness 1.0e-5)) ; stop function: when fitness(best-nest) < 1.0e-5
        -5 ; lb
        5 ; ub
        0.25 ; pa
        ))
```



Copyright © 2016

