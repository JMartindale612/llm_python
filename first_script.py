# -*- coding: utf-8 -*-

import pandas as pd
import numpy as np

df = pd.read_csv("data/fdt_creativity.csv")


df2 = pd.read_csv("data/TweetDataset_AntiBrexit_Jan-Mar2022.csv")

df_new = df.copy()

## Initial attempts at scoring

reverse_names = [col for col in df.columns if "rev" in col]

df['dt_Activ_01rev']

for item in reverse_names:
    df[item] = 6 - df[item]

scale_names = [
    'dt_Achiev','dt_Activ','dt_Altru','dt_Angry','dt_Anxiet','dt_Assert',
    'dt_Compet','dt_Complianc','dt_Delib','dt_Depress','dt_Dutif',
    'dt_Excite','dt_Fantasy','dt_Gregar','dt_Impuls','dt_Indiff',
    'dt_Modest','dt_Order','dt_SelfAss','dt_SelfDisc','dt_Shame',
    'dt_Straight','dt_Tender','dt_Trust','dt_Vulner','dt_Warmth'
    ]

# Redundnacy scales:
# 

for scale_name in scale_names:
    df[scale_name] = df.filter(like=f"{scale_name}_").mean(axis=1, skipna=True)

# There are only 26 facets created here, so we are missing the full FDT. Worth checking with the 
# Sample B data, did we collect those further items and could it be worth
# scoring up in R and then removing to here

df_sent = df.filter(like=f"mc_Revstrat")
df_test = df['dt_Achiev':'dt_Warmth']


### Text analysis ----

from transformers import pipeline
import tf_keras as keras

# Load a pre-trained sentiment analysis model
classifier = pipeline("sentiment-analysis")

# Test with a review
review = "The movie was not ok."
result = classifier(review)

print(result)  