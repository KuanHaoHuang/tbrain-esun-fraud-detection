# T-brain 2019 秋季賽：Top 1% Solution
* 競賽名稱：玉山人工智慧公開挑戰賽 2019 秋季賽 真相只有一個『信用卡盜刷偵測』
* 競賽網址：https://tbrain.trendmicro.com.tw/Competitions/Details/10

## 參賽成績
* Team: **StarRingChild**
* Rank: **11th** / 1,366 Teams (Top 1%) 
* Private Leaderboard F1_Score: **0.679**

## Git Repo 說明
* 「創意做法擂台戰」所分享之做法海報、及閃電講簡報在 doc
* sample_data 內數據僅供測試、並非競賽真實資料
  * 2020-05-16 sample_data 已經移除，原始資料請洽詢主辦單位
* 主要分析流程請執行 script.R
  * 重複實驗時，設定 CLEAN_START 以清除中介檔案

## 比賽技巧簡述
* 以使用者刷卡歷程為中心、並依照近遠期區別設計 Features
* 根據該使用者是否在 training / testing set 皆有出現，區分 coldstart / warmstart 訓練方法
* 紀錄每次訓練超參數、挑選稍低 Validation 準確度的模型加入 Ensemble
  * (Code 尚未納入 Repo)
* 最初 Feature 數量超過 700 個，為增加訓練效率，用簡單抽樣方法、並依據 Feature Importance 篩選 Feature

## Dependencies
* R version 3.5.3
* library(tidyverse)
* library(mlr)
* library(xgboost)
* library(matgrittr)
