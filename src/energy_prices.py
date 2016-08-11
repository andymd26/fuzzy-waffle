if __name__ == '__main__':
    import os
    import pandas as pd
    import numpy as np

    data_dir = "/Users/bloh356/Google Drive/Data/Data_Econ1"
    data = pd.read_csv(os.path.join(data_dir, "existcapacity_annual.csv"), header=0)

    data.columns = pd.Series(data.columns).str.replace(' ', '_')
    data.columns = [x.lower() for x in data.columns] # Use list comprehensions where possible
    data.sort(['producer_type', 'fuel_source', 'state_code', 'year'], inplace=1)

    data['summer_capacity_(megawatts)'] = data["summer_capacity_(megawatts)"].convert_objects(convert_numeric='force')
    data['summer_capacity_change'] = data.groupby(['state_code', 'producer_type', 'fuel_source'])\
        ['summer_capacity_(megawatts)'].transform(lambda x: x.diff())

    data_wide = pd.pivot_table(data, values='summer_capacity_change', index=['state_code', 'fuel_source'], \
                               columns='year').reset_index()
    # I use the pivot_table function where you'd use the cast function in R. Reset_index converts the multi-level index
    #   back into a dataframe.

    data_wide.to_csv(os.path.join(data_dir, "capacity_fuel_change_wide.csv"), index=0)
    data.to_csv(os.path.join(data_dir, "capacity_alldata.csv"), index=0)
    mask = (data_wide.state_code == "US")
    data_wide[mask].to_csv(os.path.join(data_dir, "uscapacity_fuel_change_wide.csv"))
    mask = (data.producer_type == "Electric Generators, Electric Utilities") & (data.state_code == "US") & \
           (data.fuel_source != 'All Sources')

    data_us = data[mask]
    data_us.to_csv(os.path.join(data_dir, "uscapacity_fuel_net.csv"), index=0)

    prices_1 = {'coal': [1.579, 1.506, 1.466, 1.445, 1.455, 1.447, 1.412, 1.385, 1.355, 1.318],
                'petroleum_liquids': [2.437, 3.011, 2.439, 2.893, 3.384, 2.548, 2.551, 2.433, 2.488, 2.679],
                'natural_gas': [2.351, 2.240, 2.263, 2.355, 2.321, 2.153, 2.328, 2.560, 2.230, 1.984],
                'year': [1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995]}
    # Source: Energy Information Administration/Electric Power Monthly June 1996, Table 26 (p39). Electric Utility
    #       Receipts of and Average Cost for Fossil Fuels (1985-1996).
    # Note_1: The prices are averages and are in cents per 10^6 Btu and are nominal (i.e., unadjusted for inflation)
    # Note_2: For continuity purposes with later datasets we use the Total Petroleum price instead of the Heavy Oil
    #       price
    prices_1df = pd.DataFrame(prices_1)

    prices_2 = {'coal': [1.29, 1.27, 1.25, 1.22, 1.20, 1.23, 1.22, 1.26, 1.34, 1.53, 1.69, 1.78, 2.05],
                'petroleum_liquids': [3.16, 2.88, 2.14, 2.53, 4.45, 3.92, 3.74, 4.68, 4.80, 7.17, 8.33, 9.24,\
                                      15.72],
                'natural_gas': [2.64, 2.76, 2.38, 2.57, 4.30, 4.49, 3.68, 5.59, 6.15, 8.32, 7.36, 7.47, 9.22],
                'year': [1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008]}
    # Source: Energy Information Administration/Electric Power Monthly January 2010, Table 4.2 (p73). Receipts, Average
    #       Cost, and Quality of Fossil Fuels: Electric Utilities (1995-2009)
    # Note_1: The prices are averages and are in dollars per 10^6 Btu and are nominal (i.e., unadjusted for inflation)
    # Note_2: For continuity purposes with later datasets we use the Petroleum liquid prices
    prices_2df = pd.DataFrame(prices_2)

    prices_3 = {'coal': [2.22, 2.27, 2.40, 2.43, 2.38, 2.39, 2.25],
                'petroleum_liquids': [10.44, 13.94, 20.30, 22.11, 21.09, 19.90, 11.30],
                'natural_gas': [5.50, 5.43, 5.00, 3.74, 4.49, 5.17, 3.52],
                'year': [2009, 2010, 2011, 2012, 2013, 2014, 2015]}
    # Source: Energy Information Administration/Electric Power Monthly May 2016, Table 4.2 (p73). Receipts, Average
    #       Cost, and Quality of Fossil Fuels: Electric Utilities (2006-2016)
    # Note_1: The prices are averages and are in dollars per 10^6 Btu and are nominal (i.e., unadjusted for inflation)
    # Note_2: For continuity purposes with later datasets we use the Petroleum liquid prices
    prices_3df = pd.DataFrame(prices_3)

    prices = pd.concat([prices_1df, prices_2df, prices_3df])

    test = pd.merge(data_us, prices, on='year')
    test['ts'] = test.year-1989
    test = test[test['summer_capacity_change'].notnull()]
    # Removes the first year (1990), which has a difference value of NaN

    #test_1 = pd.DataFrame(columns=test.columns)
    #for i in range(0, len(test)):
    #    if int(test.summer_capacity_change.iloc[[i]]) != 0:
    #        test_1 = test_1.append(pd.concat([test.iloc[[i]]]*(int(abs(test.summer_capacity_change.iloc[[i]]))*10)))
    #        print(int(abs(test.summer_capacity_change.iloc[[i]])))
    #    else:
    #        print(int(abs(test.summer_capacity_change.iloc[[i]])))
    #        print(i)
    #        continue

    df_list = []
    for row in test.iterrows():
        df_list.append(row * int(abs(row['summer_capacity_change'])))
    test2 = pd.concat(df_list)
    print(test2.head)
    #df_list = []
    #for row in test.itertuples():
    #    df_list.extend(row * int(abs(row[9])))
    #    print(int(abs(row[9])))
    #    #print(len(df_list))
    #    #print(row)
        #print(int(abs(row[9])))
        #df_list.append(row[1])
    #for 1st in [for row in test.itertuples()]
    #S = [row*10 for row in test.itertuples()]
    #int(abs(row[9]))
    #print(S[0][0])
    #T = pd.DataFrame().append(S)

    #df = pd.DataFrame(S, columns=test.columns).unstack()
    #flattened = [i for sub in [row*int(abs(row[9])) for row in test.itertuples()] for i in sub]
    #print(len(S))
    #df = pd.concat(S)



    #T = pd.concat(S)
    #test2 = pd.DataFrame(df_list)
    #print(len(test2))
    #print(pd.DataFrame(df_list).columns)
    #test.to_csv(os.path.join(data_dir, "test.csv"), index=0)
    #.to_csv(os.path.join(data_dir, "test2.csv"), index=0)











