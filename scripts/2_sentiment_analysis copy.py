import sys
import pandas as pd
from transformers import pipeline

# Check that input and output file names are provided
if len(sys.argv) != 3:
    print("Usage: python sentiment_analysis.py input_file.csv output_file.csv")
    sys.exit(1)

input_file = sys.argv[1]
output_file = sys.argv[2]

# Load the dataset
df = pd.read_csv(input_file)

# Check that there's a 'comment' column
if 'comment' not in df.columns:
    print("Error: CSV file must contain a 'comment' column.")
    sys.exit(1)

# Load the Italian sentiment model
sentiment_pipeline = pipeline(
    "text-classification",
    model="MilaNLProc/feel-it-italian-sentiment",
    tokenizer="MilaNLProc/feel-it-italian-sentiment"
)

# Function to classify each comment safely
def safe_sentiment(comment):
    try:
        result = sentiment_pipeline(
            comment,
            truncation=True,
            max_length=512
        )
        return result[0]['label']
    except Exception as e:
        print(f"Error processing comment: {e}")
        return "ERROR"

# Apply the sentiment analysis
df['sentiment'] = df['comment'].astype(str).apply(safe_sentiment)

# Save the output
df.to_csv(output_file, index=False)

print(f"Sentiment analysis complete. Results saved to: {output_file}")


# 🔹 1. Emotion Classification with feel-it-emotion
from transformers import pipeline
import pandas as pd

# Load your comments
df = pd.read_csv("your_comments.csv")

# Load FEEL-IT Emotion model
emotion_pipeline = pipeline(
    "text-classification",
    model="MilaNLProc/feel-it-italian-emotion",
    tokenizer="MilaNLProc/feel-it-italian-emotion"
)

# Apply to each comment
def classify_emotion(text):
    try:
        result = emotion_pipeline(text, truncation=True, max_length=512)
        return result[0]['label']
    except Exception as e:
        return "ERROR"

df['emotion'] = df['comment'].astype(str).apply(classify_emotion)
df.to_csv("with_emotions.csv", index=False)


# 🔹 2. Toxicity or Hate Speech Detection
from transformers import AutoModelForSequenceClassification, AutoTokenizer, pipeline

tokenizer = AutoTokenizer.from_pretrained("samfc/ItHateBERT")
model = AutoModelForSequenceClassification.from_pretrained("samfc/ItHateBERT")

hate_pipeline = pipeline("text-classification", model=model, tokenizer=tokenizer)

# Apply to comments
df['hate_label'] = df['comment'].apply(lambda x: hate_pipeline(x)[0]['label'])


# 🔹 3. Topic-Specific Lexicons (Misogyny, Victim-Blaming, etc.)
victim_blaming_keywords = [
    "se l'è cercata", "era provocante", "non doveva uscire da sola", 
    "doveva denunciare prima", "che ci faceva lì", "è colpa sua"
]

def contains_victim_blaming(comment):
    comment = comment.lower()
    return any(phrase in comment for phrase in victim_blaming_keywords)

df['victim_blaming'] = df['comment'].astype(str).apply(contains_victim_blaming)



