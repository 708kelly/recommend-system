# -*- coding: utf-8 -*-

"""
Created on Tue Oct 17 17:21:06 2017

@author: Joe Chiao
"""
import numpy as np
import pandas as pd
import sys
import csv
import os
#%%
def rec_items(shopid, *args):
    args = args[0].split(',')
    temp2 = []
    for i in args:
        temp2.append(int(i))
    input_file = "shopid_" + str(shopid) + "_rules.npy"  
    rules = np.load("./fp/"+input_file)
    items = rules.item().keys() ##將rules的keys轉成list of tuples
    rec_list = []
    temp = []
    if len(temp2) == 1: ##檢查input是否只有1個商品
        for i in items: ##尋找input是否有在rules中
            i = list(i) ##將tuple轉成list
            if temp2 == i:
                temp.append(rules.item()[tuple(i)][0]) 
    else: ##input超過1個商品
        for i in items: ##尋找input是否有在rules中
            i = list(i) ##將tuple轉成list
            if sorted(temp2) == i: ##如果rules有該input，將該rule推薦的商品加入到temp
                temp.append(rules.item()[tuple(i)][0])
            for salepage in temp2: ##將input中的每個元素拆開，找出各元素的推薦商品
                if salepage == i:
                    temp.append(rules[tuple(i)][0])      
    for itemlist in temp: ##將推薦出來的商品，若有複數頻，切開
        for item in itemlist:
            rec_list.append(item)
    rec_list = list(pd.Series(rec_list).drop_duplicates()) ##移除重複的推薦商品
    print("Rec")
    print(rec_list)
    if len(rec_list) == 0 :
        rec = 0
        rec_list.append(rec)
    output_file = "shopid_" + str(shopid) + "_rec_items.csv"
    f = open("./fp/fp_result/"+output_file,"w")
    w = csv.writer(f)
    w.writerow(rec_list)
    f.close()
#%%
salepageid = sys.argv[2]
shopid = sys.argv[1]
#print(shopid)
#print(type(shopid))
#print(int(shopid))
#print(os.getcwd())
rec_items(int(sys.argv[1]), salepageid)

#輸入型式:python rec_algorithm.py 1993 2350073,2782640,3020350
