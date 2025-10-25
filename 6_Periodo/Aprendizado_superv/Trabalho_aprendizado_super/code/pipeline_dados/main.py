from pathlib import Path
from pipeline.ingest import load_data
from pipeline.transform import transform_data
from pipeline.validate import validate_data

def main():
    raw_data = load_data()
    transformed = transform_data(raw_data)
    validate_data(transformed)
    print("✅ Pipeline executada com sucesso!")

if __name__ == "__main__":
    main()