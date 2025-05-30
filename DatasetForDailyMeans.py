import pandas as pd

# Specify path to file
path = 'C:/Users/jarne/Documents/School/Visualization in Data Science/Project/madrid.h5/madrid.h5'

columns = ['CO','NO_2','O_3','PM10','SO_2']

dfs = []

# Loop over each station in the H5 file
with pd.HDFStore(path) as stations:
    keys = stations.keys()

for key in keys:
    df = pd.read_hdf(path, key)

    # Add a date and year column
    if isinstance(df.index, pd.DatetimeIndex):
        df = df.reset_index()
        df.rename(columns={df.columns[0]: 'date'}, inplace=True)
    else:
        df['date'] = pd.to_datetime(df['date'])

    df['year'] = df['date'].dt.year

    df_out = df[['year'] + [c for c in columns if c in df.columns]]

    # Remove all NA rows from the dataset and concatenate them together

    df_out = df_out.dropna(subset=columns, how='all')

    df_out['source'] = key.strip('/')

    dfs.append(df_out)

combined_df = pd.concat(dfs, ignore_index=True)
combined_df = combined_df.sort_values('year').reset_index(drop=True)

# Save dataset

combined_df.to_csv('combined_pollutants.csv', index=False)

# Take the means of each air pollutant per year group

means_df = combined_df.groupby('year')[['CO','NO_2','O_3','PM10','SO_2']].mean()

# Save the pollutant_means dataset

means_df = means_df.reset_index()
means_df.to_csv('pollutant_means.csv', index=False)

##
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
df2['Day'] = df2.index.day

df2 = df2.sort_values('Year').reset_index(drop=True)

df2

# Calculate all the means per year for each main (CAQI) pollutant
df2.info()
df2.to_csv('all_pollutants.csv', index=False)
means_df2 = df2.groupby('year')[['PM25','NO_2','O_3','PM10','SO_2']].mean()
means_df2 = means_df2.reset_index()
means_df2.to_csv('all_means_pollutants.csv', index=False)

# Calculate all the means per year, per month and per day for each main (CAQI) pollutant
means_df4 = df2.groupby(['Year', 'Month'])[['PM25','NO_2','O_3','PM10','SO_2']].mean(numeric_only=True).reset_index()
means_df4
means_df4.to_csv('complete_means_pollutants.csv', index=False)

df2['date'] = pd.to_datetime(df2[['Year','Month','Day']])

means_df5 = (df2.groupby('date')[['PM25','NO_2','O_3','PM10','SO_2']].mean(numeric_only=True).reset_index())

means_df5['Year']  = means_df5['date'].dt.year
means_df5['Month'] = means_df5['date'].dt.month
means_df5['Day']   = means_df5['date'].dt.day

means_df5

# 3) export
means_df5.to_csv('daily_means_pollutants.csv', index=False)
