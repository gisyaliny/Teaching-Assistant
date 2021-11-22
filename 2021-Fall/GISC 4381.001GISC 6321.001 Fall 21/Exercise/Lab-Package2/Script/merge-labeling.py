import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import os,glob,time


start_time = time.time()
excel_dir = '../'
image_dir = '../Data/img/Split-Raster'
pattern = '*.TIF'
img_lst = glob.glob(os.path.join(image_dir,pattern))
pattern  = '*.xlsx'
excel_lst = glob.glob(os.path.join(excel_dir,pattern))
df = pd.read_excel(excel_lst[0])
for table in excel_lst[1:]:
    df_new = pd.read_excel(table)
    df = pd.concat([df,df_new])
del df['Unnamed: 0']
df.drop_duplicates(inplace=True)

singular_lst = []
for x in df.file_name:
    img = plt.imread(os.path.join(image_dir,x))
    if img.shape != (64,64,3):
        singular_lst.append(x)

index_lst = [index for index,x in enumerate(df.file_name) if x in singular_lst]
index_lst[:5]

new_df = df.drop(df.index[index_lst])
new_df.to_excel('labeled-all-images.xlsx')

print('Task finished with %s seconds' %(round(time.time() - start_time, 2)))