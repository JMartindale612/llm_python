# -*- coding: utf-8 -*-

import pandas as pd
import numpy as np

a = 5
a

print("Hello world!")

data = [np.random.standard_normal() for i in range(7)]

b = "another way of writing a string"

x = -5

if x < 0:
    print("It's negative!")

## Data structures -----

tup = tuple([4, 0, 2])

tup = tuple('test_string')
tup

nested_tup = (4, 5, 6), (7, 8)
print(nested_tup)
# Lists are standard, that's what every other script is called

dictionary = {"a": "some value", 
              "b": [1, 2, 3, 4]}


df = pd.read_csv("data/fdt_creativity.csv")
