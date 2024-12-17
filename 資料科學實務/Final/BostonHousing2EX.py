# ---------------------------------
# 1. 匯入必要的套件（固定部分）
# ---------------------------------
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns

from sklearn.metrics import mean_squared_error, r2_score
from sklearn.model_selection import KFold
from sklearn.svm import SVR
from sklearn.ensemble import RandomForestRegressor
from xgboost import XGBRegressor

from sklearn.preprocessing import StandardScaler

import warnings
warnings.filterwarnings('ignore')

sns.set(style="whitegrid")
plt.rcParams['figure.figsize'] = (14, 8)

# ---------------------------------
# 2. 資料載入與預處理（固定部分）
# ---------------------------------
df = pd.read_csv('BostonHousing2.csv')

target = 'cmedv'
features = ['town', 'tract', 'lon', 'lat', 'crim', 'zn', 'indus', 
            'chas', 'nox', 'rm', 'age', 'dis', 'rad', 'tax', 
            'ptratio', 'b', 'lstat']

X = df[features]
y = df[target]

# ---------------------------------
# 3. 特徵處理函數（固定部分）
# ---------------------------------
def label_encoding(train_df, test_df, column):
    unique_labels = train_df[column].unique()
    label_mapping = {label: idx for idx, label in enumerate(unique_labels)}
    train_df[f'{column}_label'] = train_df[column].map(label_mapping)
    test_df[f'{column}_label'] = test_df[column].map(label_mapping).fillna(-1)
    train_df = train_df.drop(column, axis=1)
    test_df = test_df.drop(column, axis=1)
    return train_df, test_df

# ---------------------------------
# 4. 模型初始化（固定部分）
# ---------------------------------
random_state = 42

baseline_models = {
    'SVM': SVR(),
    'RandomForest': RandomForestRegressor(random_state=random_state),
    'XGBoost': XGBRegressor(random_state=random_state, objective='reg:squarederror')
}

overall_results = {
    'Model': [],
    'Train_MSE': [],
    'Train_RMSE': [],
    'Train_R²': [],
    'Test_MSE': [],
    'Test_RMSE': [],
    'Test_R²': []
}

# ---------------------------------
# 5. 5-Fold Cross Validation 設定與執行（固定部分）
# ---------------------------------
kf = KFold(n_splits=5, shuffle=True, random_state=random_state)

np.random.seed(random_state)
shuffled_indices = np.random.permutation(len(X))
X_shuffled = X.iloc[shuffled_indices].reset_index(drop=True)
y_shuffled = y.iloc[shuffled_indices].reset_index(drop=True)

baseline_predictions = {
    'SVM': {'y_true': [], 'y_pred': []},
    'RandomForest': {'y_true': [], 'y_pred': []},
    'XGBoost': {'y_true': [], 'y_pred': []}
}

for model_name, model in baseline_models.items():
    train_mse_list = []
    train_rmse_list = []
    train_r2_list = []
    
    test_mse_list = []
    test_rmse_list = []
    test_r2_list = []
    
    for train_idx, test_idx in kf.split(X_shuffled):
        X_train, X_test = X_shuffled.iloc[train_idx].copy(), X_shuffled.iloc[test_idx].copy()
        y_train, y_test = y_shuffled.iloc[train_idx].copy(), y_shuffled.iloc[test_idx].copy()
        
        train_df = X_train.copy()
        train_df[target] = y_train.values
        test_df = X_test.copy()
        test_df[target] = y_test.values
        
        train_df, test_df = label_encoding(train_df, test_df, 'town')
        
        X_train_processed = train_df.drop(target, axis=1)
        y_train_processed = train_df[target]
        X_test_processed = test_df.drop(target, axis=1)
        y_test_processed = test_df[target]
        
        scaler = StandardScaler()
        X_train_scaled = scaler.fit_transform(X_train_processed)
        X_test_scaled = scaler.transform(X_test_processed)
        
        model.fit(X_train_scaled, y_train_processed)
        
        y_pred_test = model.predict(X_test_scaled)
        y_pred_train = model.predict(X_train_scaled)
        
        mse_test = mean_squared_error(y_test_processed, y_pred_test)
        rmse_test = np.sqrt(mse_test)
        r2_test = r2_score(y_test_processed, y_pred_test)
        
        mse_train = mean_squared_error(y_train_processed, y_pred_train)
        rmse_train = np.sqrt(mse_train)
        r2_train = r2_score(y_train_processed, y_pred_train)
        
        train_mse_list.append(mse_train)
        train_rmse_list.append(rmse_train)
        train_r2_list.append(r2_train)
        
        test_mse_list.append(mse_test)
        test_rmse_list.append(rmse_test)
        test_r2_list.append(r2_test)
    
    avg_train_mse = np.mean(train_mse_list)
    avg_train_rmse = np.mean(train_rmse_list)
    avg_train_r2 = np.mean(train_r2_list)
    
    avg_test_mse = np.mean(test_mse_list)
    avg_test_rmse = np.mean(test_rmse_list)
    avg_test_r2 = np.mean(test_r2_list)
    
    overall_results['Model'].append(model_name)
    overall_results['Train_MSE'].append(avg_train_mse)
    overall_results['Train_RMSE'].append(avg_train_rmse)
    overall_results['Train_R²'].append(avg_train_r2)
    overall_results['Test_MSE'].append(avg_test_mse)
    overall_results['Test_RMSE'].append(avg_test_rmse)
    overall_results['Test_R²'].append(avg_test_r2)
    
    print(f"\n模型 {model_name} - 平均評估指標:")
    print(f"Train - MSE: {avg_train_mse:.4f}, RMSE: {avg_train_rmse:.4f}, R²: {avg_train_r2:.4f}")
    print(f"Test  - MSE: {avg_test_mse:.4f}, RMSE: {avg_test_rmse:.4f}, R²: {avg_test_r2:.4f}")
    
    baseline_predictions[model_name]['y_true'].extend(y_test_processed.values)
    baseline_predictions[model_name]['y_pred'].extend(y_pred_test)

# ---------------------------------
# 6. 基準模型的整體 R² 計算（固定部分）
# ---------------------------------
baseline_r2 = {}
for model_name in baseline_models.keys():
    r2 = r2_score(baseline_predictions[model_name]['y_true'], baseline_predictions[model_name]['y_pred'])
    baseline_r2[model_name] = r2
    print(f"\n基準模型 {model_name} 的整體 R²: {r2:.4f}")

# ---------------------------------
# 7. 自訂部分開始（需修改部分）
# ---------------------------------
# ----------------------------------------------------------------

selected_model_name = "RandomForest"  # 可以是 "SVM", "RandomForest", "XGBoost"

# 在此區域內完成模型的選擇與訓練
# 僅可在此區域內添加或修改程式碼

# ----------------------------------------------------------------
# 自訂部分結束

# ---------------------------------
# 8. 結果比較與展示（可選）
# ---------------------------------
comparison_df = pd.DataFrame(overall_results)
# 假設完成後會將其結果添加到 comparison_df 中
for model_name, r2 in baseline_r2.items():
    comparison_df.loc[comparison_df['Model'] == model_name, '基準R²'] = r2
    
print("\n模型比較結果:")
print(comparison_df[['Model', '基準R²']])