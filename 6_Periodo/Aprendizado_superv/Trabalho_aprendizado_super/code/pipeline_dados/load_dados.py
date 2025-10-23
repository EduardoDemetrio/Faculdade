import pandas as pd
from pathlib import Path

def load_data():
    nb_dir = Path.cwd().parent
    data_dir = nb_dir / "data"
    df_transcript = pd.read_json(data_dir / "transcript.json", lines=True)
    df_profile    = pd.read_json(data_dir / "profile.json",    lines=True)
    df_portfolio  = pd.read_json(data_dir / "portfolio.json",  lines=True)
    return {"portfolio": df_portfolio, "profile": df_profile, "transcript": df_transcript}