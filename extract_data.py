import os
from hashlib import sha256
from pathlib import Path

import pandas as pd
from dotenv import load_dotenv

load_dotenv()
RAW_DATA_PATH = Path(os.getenv("RAW_DATA_PATH"))

RELEVANT_COLS = ["Kollégista", "Tanév", "Kurzus neve", "Eredmény", "Félév", "Típus"]

df = (
    pd.read_excel(RAW_DATA_PATH, sheet_name="összes kollégista-kurzus")
    .filter(RELEVANT_COLS, axis=1)
    .rename(
        columns={
            "Kollégista": "name",
            "Tanév": "academic_year",
            "Kurzus neve": "course",
            "Eredmény": "grade",
            "Félév": "semester",
            "Típus": "course_type",
        }
    )
    .assign(
        name=lambda _df: _df["name"].apply(lambda v: sha256(v.encode()).hexdigest())
    )
)

df.to_csv("course_data_hashed.csv", encoding="utf-8", index=False)
