# -*- coding: utf-8 -*-

import pandas as pd
import numpy as np

df = pd.read_csv("data/fdt_creativity.csv")

df_new = df.copy()

## Initial attempts at scoring

df['dt_Achiev'] = df.filter(like='dt_Achiev_').mean(axis=1, skipna=True)

df['dt_Achiev']

reverse_names = [col for col in df.columns if "rev" in col]

df['dt_Activ_01rev']

for item in reverse_names:
    df[item] = 6 - df[item]