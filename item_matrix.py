#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Nov  1 11:49:25 2017

@author: kelly
"""

import csv
import numpy as np
import math
import pandas as pd
import os
import scipy
import time
import timeit

from sklearn.metrics.pairwise import cosine_similarity
#import datetime

def sigmoid(x,cycle):
    res = 1 / (1 + np.exp(3 * np.log(2)*(x - 3 * cycle)/cycle))
    return res

os.chdir("/Users/kelly/Documents/91_intern/totalyear")

#shoplist = (1317,2202,156,348,360,15,1327,1993,2332,334,272,2190,49,2131,123,770,3975)
#for shop in shoplist:
localtime = time.asctime( time.localtime(time.time()) )
print ("本地时间为 :", localtime)

start = timeit.default_timer()

shop = input("測試店家：") # 目前記得是排行 之後應該要改成店的編號
file_in = "./total_1108_20shop/"+shop + "_total.csv"
order_all = pd.read_csv(file_in)
cycle_file_in = 'total_customercycle.csv'

cycle_all = pd.read_csv(cycle_file_in)

order_all.loc[:,['DateId','ShopID']]
# select py position 使用 iloc
#print(df[df.A > 8])

order_all['DateId'] = pd.to_datetime(order_all['DateId'],format='%Y%m%d')


# Time-Based
c_filter = cycle_all['ShopID'] == int(order_all.loc[1,['ShopID']])
cycle = float(cycle_all[c_filter]['Period'])

time_max = max(order_all['DateId'])
order_all['time_minus'] = (time_max - order_all['DateId']).dt.days
order_all['weight'] = sigmoid(order_all['time_minus'],cycle) * order_all['Quantity']

# filter放在後面比較對（應比加總）
filter = order_all['weight'] >= 10**(-6) 
all = order_all[filter]
user_item_weight = all.loc[:,['MemberId','SalePageId','weight']]

user_item_weight = user_item_weight.groupby(['MemberId','SalePageId']).sum()

js_user_item_old = user_item_weight.unstack(level=-1).to_sparse()

localtime = time.asctime( time.localtime(time.time()) )
print ("本地时间为 :", localtime)

# 轉Sparse
"""
indices = np.nonzero(~np.isnan(js_user_item_old))
sps = scipy.sparse.coo_matrix((js_user_item_old[indices], indices), shape=js_user_item_old.shape)
js_user_item = scipy.sparse.csr_matrix(js_user_item_old .values)
similarities = cosine_similarity(js_user_item.T)
"""

js_user_item = js_user_item_old.fillna(0)
localtime = time.asctime( time.localtime(time.time()) )
print ("本地时间为 :", localtime)
js_user_item = scipy.sparse.csr_matrix(js_user_item.values)
localtime = time.asctime( time.localtime(time.time()) )
print ("本地时间为 :", localtime)
similarities = cosine_similarity(js_user_item.T)
#print(js_user_item.density)
localtime = time.asctime( time.localtime(time.time()) )
print ("本地时间为 :", localtime)
# Sparse .to_sparse(fill_value=0)
"""
js_item_sim_1 = js_user_item.T.dot(js_user_item)
d = np.diag(js_item_sim_1)[np.newaxis]
len = np.sqrt(np.dot(d.T,d))
cos = similarities/len

"""
m_filter = (similarities == 1)
similarities[m_filter] = 0

matrix = pd.DataFrame(data = similarities,
             index = js_user_item_old.columns.get_level_values(1),
             columns = js_user_item_old.columns.get_level_values(1)).to_sparse(fill_value=0)
localtime = time.asctime( time.localtime(time.time()) )
print ("本地时间为 :", localtime)
# Save
pickle_name = '../item2item/'+shop+'.pkl'
matrix.to_pickle(pickle_name)

pkl_size = os.stat(pickle_name).st_size

stop = timeit.default_timer()

print ("時間 (s)：",( stop - start ), "pickle size : ", (pkl_size/1000)) 
# Load
#np.load('../item2item/'+shop+'.npy')
#matrix = pd.read_pickle(('../item2item/'+shop+'.pkl'))

