import sys
import geopandas as gpd
import pandas as pd
import time,os,glob,multiprocessing
from osgeo import gdal
import numpy as np
from shapely import geometry

def chunk_it(seq, num):
    """divide origin list into several chunks based on the given `num`
    :param seq: the input list
    :param num: how many number of slices needed
    """
    avg = len(seq) / float(num)
    out = []
    last = 0.0
    while last < len(seq):
        out.append(seq[int(last):int(last + avg)])
        last += avg
    return out

building_footprint = r'./Data/shp/Cliped-Building-Footprints.shp'

class Spliter(multiprocessing.Process):
    # # read the shapefile
    buildings_df = gpd.read_file(building_footprint)
    buildings_df = buildings_df.to_crs(epsg = 26914)
    def __init__(self,raster_lst,index):
        super(Spliter,self).__init__()
        self.raster_lst = list(raster_lst)
        self.index = index

    def _to_excel(self):
        print(f'Processor{self.index} start the task!')
        results = {'file_name':[],'y_label':[]}
        for image in self.raster_lst:
            raster = gdal.Open(image)
            results['file_name'].append(os.path.basename(image))
            #     build the bounding box
            gt = raster.GetGeoTransform()
            pixelWidth,pixelHeight = gt[1],gt[5]
            cols = raster.RasterXSize
            rows = raster.RasterYSize
            xLeft = gt[0]
            yTop = gt[3]
            xRight = xLeft + cols * pixelWidth
            yBottom = yTop - rows * pixelHeight
            point_lst = [[xLeft,yTop],[xLeft,yBottom],[xRight,yBottom],[xRight,yTop]]
            poly = geometry.Polygon([[p[0], p[1]] for p in point_lst])
            #     intersects with building footprints
            results['y_label'].append(bool(Spliter.buildings_df.geometry.apply(lambda x:x.intersects(poly)).sum()))
        pd.DataFrame(results).to_excel('Labeled-proc'+str(self.index)+'.xlsx')
        print(f'Processor{self.index} finish the task!')
    
    def run(self):
        self._to_excel()


if __name__ == '__main__':
    
    raster_dir = 'Data/img/Split-Raster'
    pattern = '*.TIF'
    raster_lst = glob.glob(os.path.join(raster_dir,pattern))
    raster_lsts_chunk = chunk_it(raster_lst,15)

    start_time = time.time()

    for index,raster_lsts in enumerate(raster_lsts_chunk):
        p = Spliter(raster_lsts,index)
        p.start()
    p.join()

    print('Task finished with %s seconds' %(round(time.time() - start_time, 2)))

