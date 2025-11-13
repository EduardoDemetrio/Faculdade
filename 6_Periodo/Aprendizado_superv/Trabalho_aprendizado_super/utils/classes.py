import pandas as pd
import numpy as np
from sklearn.base import BaseEstimator, TransformerMixin
from pandas.api.types import is_integer_dtype, is_bool_dtype
 
class FeatureBuilder(BaseEstimator, TransformerMixin):
    def __init__(self, map_gender=True, drop_raw=True):
        self.map_gender = map_gender
        self.drop_raw = drop_raw
        self.raw_cols_to_drop = [
            "age","income","time_total",'became_member_on'
        ]

    def fit(self, X, y=None):
        return self

    def transform(self, X):
        data = pd.DataFrame(X).copy()

        for c in data.columns:
            if is_integer_dtype(data[c].dtype) and "Int" in str(data[c].dtype):
                data[c] = data[c].astype('float64')         
            if str(data[c].dtype) == 'boolean' or is_bool_dtype(data[c].dtype):
                data[c] = data[c].fillna(False).astype(bool)

        if self.map_gender and 'gender' in data.columns:
            data['gender'] = data['gender'].map({'F': 0, 'M': 1, 'O':2})

        if 'age' in data.columns:
            data['age_bin'] = pd.cut(
                data['age'], bins=[0, 25, 40, 90, np.inf],
                labels=['0-24', '25-39', '40-89', '90+'], 
                include_lowest=True, right=False
            ).astype(str)

        if 'income' in data.columns:
            data['income_bin'] = pd.cut(
            data['income'], bins=[0, 40000, 60000, 80000, 100000, np.inf],
            labels=['0-40000', '40000-60000', '60000-80000', '80000-100000', '100000+'],
            include_lowest=True, right=False
            ).astype(str)

        if 'time_total' in data.columns:
            data['time_total_bin'] = pd.cut(
                data['time_total'], bins=[0, 120, 240, 480, 600, np.inf],
                labels=['0-120', '120-240', '240-480', '480-600', '600+'],
                include_lowest=True, right=False
            ).astype(str)
            
        if 'month' in data.columns:

            def get_season(month):
                if month in [12, 1, 2]:
                    return 'Inverno'
                elif month in [3, 4, 5]:
                    return 'Primaveira'
                elif month in [6, 7, 8]:
                    return 'Verao'
                elif month in [9, 10, 11]:
                    return 'Outono'
                else:
                    return np.nan

            data['season'] = data['month'].apply(get_season)

        if self.drop_raw:
            data = data.drop(columns=self.raw_cols_to_drop, errors='ignore')

        return data