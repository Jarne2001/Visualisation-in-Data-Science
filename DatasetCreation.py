import pandas as pd

# Specify path to file
path = 'C:/Users/jarne/Documents/School/Visualization in Data Science/Project/madrid.h5/madrid.h5'

partials = list()
# Iterate over every key in the h5 file (Credits to the Kaggle website)
# https://www.kaggle.com/code/diegovicente/a-short-introduction-to-hdf5-files
with pd.HDFStore(path) as data:
    stations = [k[1:] for k in data.keys() if k != '/master']
    for station in stations:
        df = data[station]
        df['station'] = station
        partials.append(df)
            
df2 = pd.concat(partials, sort=False).sort_index()
# Add year and month columns
df2['Year'] = df2.index.year
df2['Month'] = df2.index.month

df2 = df2.sort_values('Year').reset_index(drop=True)

# Calculate all the means per year for each main (CAQI) pollutant
df2.info()
df2.to_csv('all_pollutants.csv', index=False)
means_df2 = df2.groupby('year')[['PM25','NO_2','O_3','PM10','SO_2']].mean()
means_df2 = means_df2.reset_index()
means_df2.to_csv('all_means_pollutants.csv', index=False)

# Calculate all the means per month and per year for each main (CAQI) pollutant
means_df4 = df2.groupby(['Year', 'Month'])[['PM25','NO_2','O_3','PM10','SO_2']].mean(numeric_only=True).reset_index()
means_df4
means_df4.to_csv('complete_means_pollutants.csv', index=False)
