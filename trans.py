# -*- coding: utf-8 -*-
"""
Created on Sat Jan  9 16:27:49 2021

@author: 鞠依依
"""


import pandas as pd
import numpy as np

df = pd.DataFrame(pd.read_excel('C:/Users/11485/Downloads/hospital.xls'))
data_set = np.array(df['name'])
data_set_lists = data_set.tolist()


from urllib import request
import re

import urllib.parse as urp

def __get_location1__(name):
        my_ak = '3Zasg9xNjONqep4sQ2Eyr1650099YGVg'    # 需要自己填写自己的AK


        qurey = urp.quote(name)
        try:
            url = 'http://api.map.baidu.com/place/v2/search?query='+qurey+'&tag='+'&region='+urp.quote('北京')+'&output=json&ak='+my_ak
            #print(url)
            req = request.urlopen(url)
            res = req.read().decode()
            lat = pd.to_numeric(re.findall('"lat":(.*)',res)[0].split(',')[0])
            lng = pd.to_numeric(re.findall('"lng":(.*)',res)[0])



            return lat,lng  #经度和纬度
        except:
            return 0,0

data_list=[]
for data_set_list in data_set_lists:
    lat,lng=__get_location1__(data_set_list)
    data_list.append([lat,lng])
df = pd.DataFrame(data_list,columns=['lat','lng'])
df.to_excel('C:/Users/11485/Downloads/1.xls',sheet_name='test')
print(df)
