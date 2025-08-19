import json
import pandas as pd
from pathlib import Path

portfolio = Path("Predicting_Starbucks_Offers/data/portfolio.json")

def load_config(caminho):
    with open(caminho, "r", encoding="utf-8") as f:
        linhas = [json.loads(l) for l in f]

    return pd.DataFrame(linhas)
    
df = load_config(portfolio)

print(df.head(5))

