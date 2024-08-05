import pandas as pd
import os
import tqdm


def get_positions(path):
    
    os.chdir(path)
    data = pd.read_csv('Topography/grille2.csv')
    data.rename(columns = {'block12' : 'block'}, inplace = True)
    data = data[['rlat', 'rlon', 'lat', 'lon', 'block']]
    data.rlat = round(data.rlat, 2)
    data.rlon = round(data.rlon, 2)

    dico_block = {}

    for block in tqdm.tqdm(data.block.unique()):

        dico_block[block] = []

        data_local = data.loc[data.block == block].copy()
        lat_min = min(data_local.rlat)
        lon_min = min(data_local.rlon)
        lat_max = max(data_local.rlat)
        lon_max = max(data_local.rlon)

        lat1 = data_local.loc[(data_local.rlat == lat_min) & (data_local.rlon == lon_min)].iloc[0]['lat']
        lon1 = data_local.loc[(data_local.rlat == lat_min) & (data_local.rlon == lon_min)].iloc[0]['lon']
        dico_block[block].append([lat1, lon1])

        lat1 = data_local.loc[(data_local.rlat == lat_min) & (data_local.rlon == lon_max)].iloc[0]['lat']
        lon1 = data_local.loc[(data_local.rlat == lat_min) & (data_local.rlon == lon_max)].iloc[0]['lon']
        dico_block[block].append([lat1, lon1])

        lat1 = data_local.loc[(data_local.rlat == lat_max) & (data_local.rlon == lon_max)].iloc[0]['lat']
        lon1 = data_local.loc[(data_local.rlat == lat_max) & (data_local.rlon == lon_max)].iloc[0]['lon']
        dico_block[block].append([lat1, lon1])

        lat1 = data_local.loc[(data_local.rlat == lat_max) & (data_local.rlon == lon_min)].iloc[0]['lat']
        lon1 = data_local.loc[(data_local.rlat == lat_max) & (data_local.rlon == lon_min)].iloc[0]['lon']
        dico_block[block].append([lat1, lon1])
    
    return dico_block


