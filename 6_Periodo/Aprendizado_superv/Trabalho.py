import json
import pandas as pd
from pathlib import Path



def load_transcript(path):
    df = pd.read_json(path, lines=True)
    val = (pd.json_normalize(df.pop('value'))
             .rename(columns=lambda c: c.strip().lower().replace(' ', '_')))
    df = df.join(val)
    for c in ('amount', 'reward'):
        if c in df.columns:
            df[c] = pd.to_numeric(df[c], errors='coerce')
    if 'offer_id' not in df.columns:
        pass
    return df

df_transcript = load_transcript(r'Predicting_Starbucks_Offers/data/transcript.json')
df_portfolio = pd.read_json(r'Predicting_Starbucks_Offers/data/portfolio.json', orient='records', lines=True)
df_profile = pd.read_json(r'Predicting_Starbucks_Offers/data/profile.json', orient='records', lines=True)


df_completo = pd.merge(df_transcript, df_profile, 
                       left_on='person', right_on='id', 
                       how='inner')

df_completoo = pd.merge(df_completo, df_portfolio, 
                       left_on='value.offer id', right_on='id', 
                       how='inner')

print(df_completoo)