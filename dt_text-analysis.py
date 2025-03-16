# Dark Triad text analysis ----

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

## First analysis 

from transformers import pipeline
import tf_keras as keras

# Load a pre-trained sentiment analysis model
classifier = pipeline("sentiment-analysis")

# Test with a review
review = "The movie was not ok."
result = classifier(review)

print(result)  

## Second analysis

from transformers import AutoTokenizer, AutoModelForSequenceClassification

model_name = "bert-base-uncased"  # Pre-trained BERT model
num_labels = 3  # Positive, Neutral, Negative

tokenizer = AutoTokenizer.from_pretrained(model_name)
model = AutoModelForSequenceClassification.from_pretrained(model_name, num_labels=num_labels)

## Third analysis

# Define your dataset (movie reviews + labels)
movie_reviews = [
    "This movie was absolutely wonderful.",  # Positive
    "I really loved the film!",  # Positive
    "The movie was alright.",  # Neutral
    "I didn't enjoy the film that much.",  # Negative
    "This movie was not to my taste."  # Negative
]

labels = [2, 2, 1, 0, 0]  # Map labels: Negative=0, Neutral=1, Positive=2

# Tokenize the dataset
inputs = tokenizer(movie_reviews, padding=True, truncation=True, return_tensors="pt")

## Fourth analysis

from transformers import Trainer, TrainingArguments
import torch
from torch.utils.data import Dataset

class MovieReviewDataset(Dataset):
    def __init__(self, encodings, labels):
        self.encodings = encodings
        self.labels = labels

    def __len__(self):
        return len(self.labels)

    def __getitem__(self, idx):
        item = {key: torch.tensor(val[idx]) for key, val in self.encodings.items()}
        item["labels"] = torch.tensor(self.labels[idx])
        return item

dataset = MovieReviewDataset(inputs, labels)

# Define training arguments
training_args = TrainingArguments(
    output_dir="./results",
    per_device_train_batch_size=4,
    num_train_epochs=3,
    logging_dir="./logs",
)

trainer = Trainer(
    model=model,
    args=training_args,
    train_dataset=dataset,
)

# Train the model
trainer.train()

## Fifth analysis

new_review = ["I really loved the film!"]

# Tokenize new review
new_inputs = tokenizer(new_review, padding=True, truncation=True, return_tensors="pt")

# Predict
outputs = model(**new_inputs)
predicted_class = torch.argmax(outputs.logits, dim=1).item()

# Label mapping
label_map = {0: "Negative", 1: "Neutral", 2: "Positive"}
print(f"Predicted Sentiment: {label_map[predicted_class]}")




### DONALD

df = pd.read_csv("data\donald_reduced.csv")

def rstr(df): return df.shape, df.apply(lambda x: [x.unique()])

rstr(df)

df