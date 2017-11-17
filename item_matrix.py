#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Nov  1 11:49:25 2017

@author: kelly
"""

#import csv
import numpy as np
#import math
import pandas as pd
import os
#import scipy
import time
import timeit

from sklearn.metrics.pairwise import cosine_similarity
from scipy.sparse import coo_matrix
#import datetime

def sigmoid(x,cycle):
    res = 1 / (1 + np.exp(3 * np.log(2)*(x - 3 * cycle)/cycle))
    return res

os.chdir("C:/Users/Kelly Wang/Documents/total_1108")
run_time = []
size = []

#shop = 1317
shoplist = (1317,2202,156,348,360,15,1327,1993,2332,334,272,2190,49,2131,123,770,3975)
for shop in shoplist:
    localtime = time.asctime( time.localtime(time.time()) )
    print ("本地时间为 :", localtime)
    
    start = timeit.default_timer()
    
    #shop = input("測試店家：") # 目前記得是排行 之後應該要改成店的編號
    file_in = str(shop) + "_total.csv"
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
    
    time_max = max(order_all['DateId']) #kaka
    order_all['time_minus'] = (time_max - order_all['DateId']).dt.days # kaka
    order_all['weight'] = sigmoid(order_all['time_minus'],cycle) * order_all['Quantity']
    
    # filter放在後面比較對（應比加總）
    filter = order_all['weight'] >= 10**(-6) 
    all = order_all[filter]
    user_item_weight = all.loc[:,['MemberId','SalePageId','weight']]
    
    row_list = list(set(all['MemberId']))
    col_list = list(set(all['SalePageId']))
    
    a = pd.Series(row_list)
    row = pd.Series(a.index.values,index=a)
    
    b = pd.Series(col_list)
    col = pd.Series(b.index.values,index=b)
    
    
    user_item_weight['rid'] = user_item_weight['MemberId'].map(row)
    user_item_weight['cid'] = user_item_weight['SalePageId'].map(col)
    #user_item_weight['rid'] = (row_list.index(user_item_weight['MemberId']))
    #user_item_weight['rid'] = [row_list.index(i) for i in user_item_weight['MemberId'] ] # kakakaka
    #user_item_weight['cid'] = [col_list.index(i) for i in user_item_weight['SalePageId'] ]
    user_item_weight = user_item_weight.loc[:,['cid','rid','weight']]
    user_item_weight = user_item_weight.groupby(['rid','cid']).sum()
    
    #js_user_item_old = user_item_weight.unstack(level=-1).to_sparse()
    
    row = np.array(user_item_weight.index.get_level_values(0))
    col = np.array(user_item_weight.index.get_level_values(1))
    value = np.array(user_item_weight.values.ravel())
    js_user_item = coo_matrix((value, (row,col))).tocsr()
    # , shape=(147584, 1475)
    
    localtime = time.asctime( time.localtime(time.time()) )
    print ("本地时间为 :", localtime)
    
    # 轉Sparse
    """
    indices = np.nonzero(~np.isnan(js_user_item_old))
    sps = scipy.sparse.coo_matrix((js_user_item_old[indices], indices), shape=js_user_item_old.shape)
    js_user_item = scipy.sparse.csr_matrix(js_user_item_old .values)
    similarities = cosine_similarity(js_user_item.T)
    """
    
    #js_user_item = js_user_item_old.fillna(0)
    #localtime = time.asctime( time.localtime(time.time()) )
    #print ("本地时间为 :", localtime)
    #js_user_item = scipy.sparse.csr_matrix(js_user_item.values)
    #localtime = time.asctime( time.localtime(time.time()) )
    #print ("本地时间为 :", localtime)
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
                 index = col_list,
                 columns = col_list).to_sparse(fill_value=0)
    localtime = time.asctime( time.localtime(time.time()) )
    print ("本地时间为 :", localtime)
    # Save
    pickle_name = './result/'+str(shop)+'.pkl'
    matrix.to_pickle(pickle_name)
    
    pkl_size = os.stat(pickle_name).st_size
    
    stop = timeit.default_timer()
    
    print ("時間 (s)：",( stop - start ), "pickle size : ", (pkl_size/1000)) 
    run_time.append( stop - start )
    size.append(pkl_size/1000)


print(run_time)
print(size)
# Load
#np.load('../item2item/'+shop+'.npy')
#matrix = pd.read_pickle(('../item2item/'+shop+'.pkl'))

