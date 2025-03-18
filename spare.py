


# Generate
generation = generator(
    prompt,
    max_new_tokens=50,      # Limit how many tokens we generate
    num_return_sequences=1,  # Only want one completion
    do_sample=False          # Using greedy/beam search might be more stable for short answers
)

model_output = generation[0]["generated_text"]
print("=== RAW MODEL OUTPUT ===")
print(model_output)

import re

def extract_score(text_output):
    # A simple regex searching for a pattern like "SCORE: 0.xx"
    pattern = r"SCORE:\s*([0-1]\.\d+|\d(\.\d+)?)"
    match = re.search(pattern, text_output)
    if match:
        return float(match.group(1))
    else:
        return None

predicted_score = extract_score(model_output)
print("Predicted score:", predicted_score)

scores = []
for i, row in df.iterrows():
    new_text = row["article_text"]
    
    prompt = f"""{nostalgia_definition}

    {nostalgia_examples}

    TEXT: "{new_text}"
    SCORE:
    """
    
    out = generator(prompt, max_new_tokens=50, do_sample=False)
    score_text = out[0]["generated_text"]
    predicted_score = extract_score(score_text)
    scores.append(predicted_score)

df["populism_score"] = scores
df.to_csv("my_articles_with_scores.csv", index=False)







## 

max_tokens_per_chunk = 700  # somewhat less than 1024 to leave room for prompt + examples

def chunk_text(text, max_tokens=700):
    # This is a naive approach; a more robust approach uses the model tokenizer
    words = text.split()
    for i in range(0, len(words), max_tokens):
        yield " ".join(words[i:i+max_tokens])

scores = []
for i, row in df.iterrows():
    article_text = row["article_text"]

    article_scores = []
    for chunk in chunk_text(article_text, max_tokens_per_chunk):
        prompt = f"""{nostalgia_definition}

Below are some examples ... etc ...

TEXT: "{chunk}"
SCORE:
"""

        out = generator(prompt, max_new_tokens=50, do_sample=False)
        score_text = out[0]["generated_text"]
        predicted_score = extract_score(score_text)
        if predicted_score is not None:
            article_scores.append(predicted_score)

    # Combine chunk scores, e.g., average them
    if len(article_scores) > 0:
        scores.append(sum(article_scores) / len(article_scores))
    else:
        scores.append(None)

df["populism_score"] = scores




model_name = "shhossain/all-MiniLM-L6-v2-sentiment-classifier"

generator = pipeline(
    task="text-generation",
    model=model_name,
    torch_dtype="float16",      # FP16 for GPU memory efficiency
    device_map="auto",          # automatically place model on GPU if available
    trust_remote_code=True      # MPT models require this to use custom code
)