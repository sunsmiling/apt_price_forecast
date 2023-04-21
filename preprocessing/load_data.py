import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
import seaborn as sns
import requests
from os import name
import xml.etree.ElementTree as et
import pandas as pd
import bs4
from lxml import html
from urllib.parse import urlencode, quote_plus, unquote

import warnings
warnings.filterwarnings(action='ignore') 

sns.set(rc={'figure.figsize':(11, 4)})
plt.rcParams['font.family'] = 'AppleGothic'
plt.rcParams['axes.unicode_minus'] = False

df_2018 = pd.read_excel('apt_price_forecast/data/2018_아파트(전월세)_실거래가.xlsx')
df_2019 = pd.read_excel('apt_price_forecast/data/2019_아파트(전월세)_실거래가.xlsx') 
df_2020 = pd.read_excel('apt_price_forecast/data/2020_아파트(전월세)_실거래가.xlsx') 
df_2021 = pd.read_excel('apt_price_forecast/data/2021_아파트(전월세)_실거래가.xlsx') 
df_2022 = pd.read_excel('apt_price_forecast/data/2022_아파트(전월세)_실거래가.xlsx') 
df_total = pd.concat([df_2018, df_2019, df_2020, df_2021, df_2022])

df_total =  df_total[df_total.전월세구분 == '전세'].reset_index(drop=True)
df_total = df_total.replace(',','', regex=True)
df_total = df_total.replace('  ','', regex=True)
df_total['보증금(만원)'] = df_total['보증금(만원)'].apply(pd.to_numeric,errors='coerce')
df_total['전용면적(㎡)'] = df_total['전용면적(㎡)'].apply(pd.to_numeric,errors='coerce')
df_total['단위면적당전세가'] = df_total['보증금(만원)'] / df_total['전용면적(㎡)']
df_total = df_total[{'단위면적당전세가','시군구','단지명','도로명','계약년월','계약일','층','건축년도'}]
df_total.to_csv('apt_price_forecast/data/df_total_cleaning.csv', index=False) 

####
kaptcode = pd.read_csv('apt_price_forecast/data/20230401_gangnam_code.csv') 
gangnam_kaptcode_list = kaptcode['APT_code'].tolist()

encoding = 'lIlqKHjraRXmeKaO7l0VyXhpIGx3wbe8gfPFrEiLt%2Bl7Ze57iOsdffcawVBm1yKjgOOmfyAnHg98VAwkk4kQQQ%3D%3D'
decoding = 'lIlqKHjraRXmeKaO7l0VyXhpIGx3wbe8gfPFrEiLt+l7Ze57iOsdffcawVBm1yKjgOOmfyAnHg98VAwkk4kQQQ=='

## Detail info
url = 'http://apis.data.go.kr/1613000/AptBasisInfoService1/getAphusDtlInfo'
params = {'serviceKey' : decoding, 'kaptCode' : 'A13593908' }

response = requests.get(url, params=params)
content = response.text 

"""
serviceKey: 서비스키
kaptCode: 단지코드
resultCode: 결과코드
resultMsg: 결과메시지
kaptName: 단지명
codeMgr: 일반관리방식
kaptMgrCnt: 일반관리인원
kaptCcompany: 일반관리 계약업체
codeSec: 경비관리방식
kaptdScnt: 경비관리인원
kaptdSecCom: 경비관리 계약업체
codeClean: 청소관리방식
kaptdClcnt: 청소관리인원
codeGarbage: 음식물처리방법
codeDisinf: 소독관리방식
kaptdDcnt: 소독관리 연간 소독횟수
disposalType: 소독방법
codeStr: 건물구조
kaptdEcapa: 수전용량
codeEcon: 세대전기계약방식
codeEmgr: 전기안전관리자법정선임여부
codeFalarm: 화재수신반방식
codeWsupply: 급수방식
codeElev: 승강기관리형태
kaptdEcnt: 승강기대수
kaptdPcnt: 주차대수(지상)
kaptdPcntu: 주차대수(지하)
codeNet: 주차관제.홈네트워크
kaptdCccnt: CCTV대수
welfareFacility: 부대.복리시설
kaptdWtimebus: 버스정류장 거리
subwayLine: 지하철호선
subwayStation: 지하철역명
kaptdWtimesub: 지하철역 거리
convenientFacility: 편의시설
educationFacility: 교육시설

""" 

col_list =["kaptCode","kaptName","kaptdPcnt","kaptdPcntu",
"welfareFacility","educationFacility","convenientFacility",
"kaptdWtimebus","kaptdWtimesub",
"subwayStation","subwayLine","subwayStation"
]

detail_df = pd.DataFrame(columns=col_list)
i = 0

for code in gangnam_kaptcode_list:
    params ={'serviceKey' : decoding, 'kaptCode' : code } 
    response = requests.get(url, params=params)
    content = response.text
    xml_obj = bs4.BeautifulSoup(content,'lxml-xml')
    rows = xml_obj.findAll('item')
    kaptCode = rows[0].find("kaptCode").string
    kaptName = rows[0].find("kaptName").string
    kaptdPcnt = rows[0].find("kaptdPcnt").string if not pd.isna(rows[0].find("kaptdPcnt")) else 'NA'
    kaptdPcntu = rows[0].find("kaptdPcntu").string if not pd.isna(rows[0].find("kaptdPcntu")) else 'NA'
    welfareFacility = rows[0].find("welfareFacility").string if not pd.isna(rows[0].find("welfareFacility")) else 'NA'
    educationFacility = rows[0].find("educationFacility").string if not pd.isna(rows[0].find("educationFacility")) else 'NA'
    convenientFacility = rows[0].find("convenientFacility").string if not pd.isna(rows[0].find("convenientFacility")) else 'NA'
    kaptdWtimebus = rows[0].find("kaptdWtimebus").string if not pd.isna(rows[0].find("kaptdWtimebus")) else 'NA'
    kaptdWtimesub = rows[0].find("kaptdWtimesub").string if not pd.isna(rows[0].find("kaptdWtimesub")) else 'NA'
    subwayStation = rows[0].find("subwayStation").string if not pd.isna(rows[0].find("subwayStation")) else 'NA'
    subwayLine = rows[0].find("subwayLine").string if not pd.isna(rows[0].find("subwayLine")) else 'NA'
    detail_df.loc[i] = [kaptCode,kaptName,kaptdPcnt,kaptdPcntu,
    welfareFacility,educationFacility,convenientFacility,
    kaptdWtimebus,kaptdWtimesub,
    subwayStation,subwayLine
    ]
    i= i+1


detail_df.to_csv('apt_price_forecast/data/apt_detail.csv', index=False, encoding="utf-8-sig")


## Basic info
url = 'http://apis.data.go.kr/1613000/AptBasisInfoService1/getAphusBassInfo'

col_list2 =["kaptCode","kaptName","kaptAddr","kaptDongCnt","kaptdaCnt",
"kaptAcompany","kaptBcompany","codeAptNm","doroJuso","hoCnt","codeHallNm",
"kaptUsedate","kaptMparea_60","kaptMparea_85","kaptMparea_135","kaptMparea_136","bjdCode"
]

base_df = pd.DataFrame(columns=col_list2)
i = 0

for code in gangnam_kaptcode_list:
    params ={'serviceKey' : decoding, 'kaptCode' : code } 
    response = requests.get(url, params=params)
    content = response.text
    xml_obj = bs4.BeautifulSoup(content,'lxml-xml')
    rows = xml_obj.findAll('item')


    kaptCode = rows[0].find("kaptCode").string
    kaptName = rows[0].find("kaptName").string
    kaptAddr = rows[0].find("kaptAddr").string if not pd.isna(rows[0].find("kaptAddr")) else 'NA'
    kaptDongCnt = rows[0].find("kaptDongCnt").string if not pd.isna(rows[0].find("kaptDongCnt")) else 'NA'
    kaptdaCnt = rows[0].find("kaptdaCnt").string if not pd.isna(rows[0].find("kaptdaCnt")) else 'NA'
    kaptAcompany = rows[0].find("kaptAcompany").string if not pd.isna(rows[0].find("kaptAcompany")) else 'NA'
    kaptBcompany = rows[0].find("kaptBcompany").string if not pd.isna(rows[0].find("kaptBcompany")) else 'NA'
    codeAptNm = rows[0].find("codeAptNm").string if not pd.isna(rows[0].find("codeAptNm")) else 'NA'
    doroJuso = rows[0].find("doroJuso").string if not pd.isna(rows[0].find("doroJuso")) else 'NA'
    hoCnt = rows[0].find("hoCnt").string if not pd.isna(rows[0].find("hoCnt")) else 'NA'
    codeHallNm = rows[0].find("codeHallNm").string if not pd.isna(rows[0].find("codeHallNm")) else 'NA'
    kaptUsedate = rows[0].find("kaptUsedate").string if not pd.isna(rows[0].find("kaptUsedate")) else 'NA'
    kaptMparea_60 = rows[0].find("kaptMparea_60").string if not pd.isna(rows[0].find("kaptMparea_60")) else 'NA'
    kaptMparea_85 = rows[0].find("kaptMparea_85").string if not pd.isna(rows[0].find("kaptMparea_85")) else 'NA'
    kaptMparea_135 = rows[0].find("kaptMparea_135").string if not pd.isna(rows[0].find("kaptMparea_135")) else 'NA'
    kaptMparea_136 = rows[0].find("kaptMparea_136").string if not pd.isna(rows[0].find("kaptMparea_136")) else 'NA'
    bjdCode = rows[0].find("bjdCode").string if not pd.isna(rows[0].find("bjdCode")) else 'NA'

    base_df.loc[i] = [kaptCode,kaptName,kaptAddr,kaptDongCnt,kaptdaCnt,
    kaptAcompany,kaptBcompany,codeAptNm,doroJuso,hoCnt,codeHallNm,
    kaptUsedate,kaptMparea_60,kaptMparea_85,kaptMparea_135,kaptMparea_136,bjdCode
    ]
    i= i+1

base_df.to_csv('../data/apt_base.csv', index=False, encoding="utf-8-sig")

