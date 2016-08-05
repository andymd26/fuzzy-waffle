if __name__ == '__main__':
    # This python code scrapes the California energy almanac for power plant heat rate data
    
    import pandas as pd
    from bs4 import BeautifulSoup
    import requests

    year_var = range(2001, 2016)
    for i in year_var:
        url = "http://energyalmanac.ca.gov/electricity/web_qfer/Heat_Rates.php?goSort=annual.expr1&year={0}".format(i)
        # url is only slightly different between the years of the data set (i.e., year = i)
        r = requests.get(url)
        data = r.text
        soup = BeautifulSoup(data, "lxml")

        table = soup.find_all('table')[0]
        # first and only table on the page
        rows = table.find_all('tr')[1:]

        data = {
            'category': [],
            'year': [],
            'company.name': [],
            'plant.id': [],
            'plant.name': [],
            'net.mwh': [],
            'main.mmbtu': [],
            'heat.rate': []
        }
        # Create a data dictionary (begin each with an empty list)

        for row in rows:
            cols = row.find_all('td')
            data['category'].append(cols[0].get_text())
            data['year'].append(cols[1].get_text())
            data['company.name'].append(cols[2].get_text())
            data['plant.id'].append(cols[3].get_text())
            data['plant.name'].append(cols[4].get_text())
            data['net.mwh'].append(cols[5].get_text())
            data['main.mmbtu'].append(cols[6].get_text())
            data['heat.rate'].append(cols[7].get_text())
            # The for loop populates each of the column spaces with the associated text from each row of the table

        elecData = pd.DataFrame(data)
        data_dir = "C:/Users/bloh356/Documents/GitHub/fuzzy-waffle/data"
        elecData.to_csv(os.path.join(data_dir, "file_{}.csv").format(i), index=0)