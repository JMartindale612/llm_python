### DONALD

## Data import and cleaning ----

import pandas as pd
import numpy as np
import re as re
from sklearn.model_selection import train_test_split
from transformers import pipeline
import pdb

df = pd.read_csv("data/donald_representative_domain-group_txt.csv")

# def rstr(df): return df.shape, df.apply(lambda x: [x.unique()])
# rstr(df)

df

# df['type'] = df['type'].astype('category')
# df['domain'] = df['domain'].astype('category')
df['domain_group'] = df['domain_group'].astype('category')
# df[['type', 'domain', 'domain_group']] = df[['type', 'domain', 'domain_group']].apply(lambda x: x.astype('category'))

print(df.dtypes)

domain_group_counts = df['domain_group'].value_counts()
print(domain_group_counts)

# If want to subset use the below:

# subset = df.groupby('domain_group').apply(lambda x: x.sample(n=400 // len(df['domain_group'].unique()), random_state=42)).reset_index(drop=True)
# df_test = df.loc[~df.index.isin(subset.index)].reset_index(drop=True)

## Text analysis ----

subset = df.groupby('domain_group').apply(lambda x: x.sample(n=200 // len(df['domain_group'].unique()), random_state=42)).reset_index(drop=True)

domain_group_counts = subset['domain_group'].value_counts()
print(domain_group_counts)

nostalgia_definition = """Definition
- Davis (1979) nostalgia "always occurs in the context of present fears, discontents, anxieties, or uncertainties"
- Sentimental, wistful or bittersweet affection or longing for the past (Sedikides & Wildschut, 2018)

Nostalgia is measured by:
- [I like to] Imagine I was living in the past
- [I like to] Imagine I was there in the simple and pure times
- [I like to] Imagine I was participating in the traditions and rituals of the past
- [I like to] Fantasise about the past
- [I like to get] Lost in the time period(s) shown
- [I like to get] Nostalgic for the time period(s) shown
"""

new_text = "This is the text of my article that I want the model to score."

nostalgia_examples = """
Below are some examples:

1) Text: "Things were better in the past, and it is sad that a lot of the things that made the past great are now gone. I do not think that things will improve in the future."
   Label: 0.90  (High nostalgia-driven populism)

2) Text: "We did things differently in the past, some of it for better and some of it for worse. It is important to remember things as they were, that some things have improved and some things have gotten worse."
    Label: 0.50 (High nostalgia-driven populism)

2) Text: "Things were worse in the past, and we are constantly improving on things to make them better now. There is no point being sentimental, we always have to look forward."
   Label: 0.10  (Low nostalgia-driven populism)

Now please read this new text and provide a number from 0.0 to 1.0 indicating how strongly it exhibits the phenomenon:
"""

prompt = f"""{nostalgia_definition}

Below are some examples where we assigned a numeric score from 0.0 (no nostalgia-driven populism) 
to 1.0 (strong nostalgia-driven populism). 

{nostalgia_examples}

Now please read the following text and provide a single numeric SCORE from 0.0 to 1.0:

{new_text}
SCORE:
"""

df

pd.set_option('display.max_colwidth', None)
print(df.iloc[0])

df.iloc[0] 


## Text approach ----

model_name = "gpt2"
generator = pipeline("text-generation", model=model_name)

generation = generator(
    prompt,
    max_new_tokens=50,      # Limit how many tokens we generate
    num_return_sequences=1,  # Only want one completion
    do_sample=False          # Using greedy/beam search might be more stable for short answers
)

model_output = generation[0]["generated_text"]

def extract_score(text_output):
    # A simple regex searching for a pattern like "SCORE: 0.xx"
    pattern = r"SCORE:\s*([0-1]\.\d+|\d(\.\d+)?)"
    match = re.search(pattern, text_output)
    if match:
        return float(match.group(1))
    else:
        return None

predicted_score = extract_score(model_output)

scores = []
for i, row in df.iterrows():
    new_text = row["text"]  # e.g. text for the i-th row
    
    prompt = f"""{nostalgia_definition}

Below are some examples where we assigned a numeric score from 0.0 (no nostalgia-driven populism) 
to 1.0 (strong nostalgia-driven populism).

{nostalgia_examples}

Now please read the following text and provide a single numeric SCORE from 0.0 to 1.0:

{new_text}
SCORE:
"""

    out = generator(prompt, max_new_tokens=50, do_sample=False)
    score_text = out[0]["generated_text"]
    predicted_score = extract_score(score_text)
    scores.append(predicted_score)


df["populism_score"] = scores
df.to_csv("my_articles_with_scores.csv", index=False)