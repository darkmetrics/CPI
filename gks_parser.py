import requests
import pandas as pd
import numpy as np

from bs4 import BeautifulSoup
from pathlib import Path
from datetime import datetime
from functools import reduce


class CPIparser:
    """
    Downloads CPI data files from Rosstat website and
    preprocesses them
    """

    def __init__(self,
                 url='https://rosstat.gov.ru/price',
                 save_path=Path('D:/Macro/CPI/data/')):
        # base url for prices on FFS to start with
        self.url = url
        # path to save xlsx files from FSS website
        self.save_path = save_path

    @staticmethod
    def get_file(url, save_path):
        out = requests.get(url)
        with open(save_path, 'wb') as f:
            f.write(out.content)

    def get_gks_data(self):
        """

        """
        # start webdriver
        base_page = requests.get(self.url)
        soup = BeautifulSoup(base_page.content, features="lxml")
        all_links = [el['href'] for el in soup.find_all('a', href=True)]

        # we are looking for 'Индексы потребительских цен' href
        price_url = next(filter(lambda x: x if 'Индексы потребительских цен' in x else None, all_links))
        weights_url = next(filter(lambda x: str(x) if 'vesa.xlsx' in x else None, all_links))
        price_url = 'https://rosstat.gov.ru' + price_url
        weights_url = 'https://rosstat.gov.ru' + weights_url

        # go to page with CPI tables
        cpi_page = requests.get(price_url)
        soup = BeautifulSoup(cpi_page.content, features="lxml")
        data_links = [el['href'] for el in soup.find_all('a', href=True)] + [weights_url]

        file_names = ['vesa' if 'vesa' in x else x.split('_')[1] for x in data_links]

        translations = {
            'ipc': 'cpi',
            'prod': 'foods',
            'neprod': 'nonfoods',
            'plat': 'services',
            'vesa': 'weights'
        }

        link_dict = dict(zip(file_names, data_links))

        # добавить многопоточную обработку, чтоб быстрее было
        # iterate over links with data and load files on PC
        for name, link in link_dict.items():
            fpath = self.save_path / str(translations[name] + '.xlsx')
            # add "override existing file?" Yes/No option
            print(fpath)
            if Path(fpath).is_file():
                change_date = datetime.strftime(datetime.fromtimestamp(
                    Path(fpath).stat().st_mtime),
                    format="%Y-%m-%d")
                print(f"Data for {translations[name]} already exists."
                      f" Last changes at {change_date}.")
                overwrite = str(input('Do you want to overwrite existing file? Yes/No: ')).lower()
                if 'y' not in overwrite and 'n' not in overwrite:
                    print('You have selected wrong option or made typo')
                # if we want to overwrite existing file
                if 'y' in overwrite:
                    CPIparser.get_file(link, fpath)
                    print(f'Data for {translations[name]} is overwritten')

            else:
                CPIparser.get_file(link, fpath)
                print(f'Data for {translations[name]} is written')

    @staticmethod
    def preprocess_file(file_path):

        # select rows to skip in raw xlsx files
        skip = list(range(3)) + [4]

        df = pd.read_excel(file_path, skiprows=skip, nrows=12, engine='openpyxl')

        df.rename(columns={df.columns.values[0]: 'month'}, inplace=True)
        df.drop(columns=list(filter(lambda x: 'Unnamed' in str(x),
                                    df.columns)), inplace=True)
        df = df.melt(id_vars=['month'],
                     value_vars=df.columns[1:],
                     var_name='year',
                     value_name='cpi')

        month_list = ['январь', 'февраль', 'март', 'апрель', 'май', 'июнь', 'июль',
                      'август', 'сентябрь', 'октябрь', 'ноябрь', 'декабрь']
        month_dict = dict(zip(month_list, list(range(1, 14))))

        df.month.replace(month_dict, inplace=True)
        df.index = pd.to_datetime(df[['year', 'month']].assign(DAY=1))
        df.index = df.index.to_period('M').to_timestamp('M')
        df.index.name = 'month'

        # clean_data_path = save_path / file_name
        # df['cpi'].to_csv(clean_data_path)
        df.drop(columns=['year', 'month'], inplace=True)
        df = df.apply(lambda x: (x - 100) / 100)
        print(f'Data for {file_path} is reformated')
        # добавить имена, как df['cpi']
        return df

    @staticmethod
    def preprocess_weights(file_path):

        skip = list(range(4)) + [5]

        df = pd.read_excel('data/weights.xlsx',
                           skiprows=skip,
                           nrows=50,
                           index_col=0,
                           engine='openpyxl')

        category_list = ['Продовольственные товары', 'Непродовольственные товары', 'Услуги']
        # check if categories we are looking for do not match exactly with their names in Excel file
        filter_list = [x for x in df.index.unique() if x.strip() in category_list]
        filtered = df[df.index.isin(filter_list)].T

        filtered.columns = [x.strip() for x in filtered.columns]
        filtered.rename(columns=dict(zip(category_list,
                                         ['foods_weight', 'nonfoods_weight', 'services_weight'])),
                        inplace=True)
        filtered = filtered.loc[np.tile(filtered.index, 12)]
        filtered['month'] = np.repeat(np.arange(1, 13), int(filtered.shape[0] / 12)).tolist()
        filtered['year'] = filtered.index

        filtered.index = pd.to_datetime(filtered[['year', 'month']].assign(DAY=1))
        filtered.index = filtered.index.to_period('M').to_timestamp('M')
        filtered[['foods_weight', 'nonfoods_weight', 'services_weight']] = \
            filtered[['foods_weight', 'nonfoods_weight', 'services_weight']].apply(lambda x: x / 100)

        filtered.drop(columns=['month', 'year'], inplace=True)
        print('reformated weights')
        return filtered.sort_index()

    def preprocess_files(self,
                         open_path='default',
                         save_path='default',
                         file_name='clean_cpi.csv'):
        # Здесь препроцессинг только одного файла, а надо все
        '''
        ! Works properly if in directory are only data files related to the project !
        '''
        # if no specific path to save data is supplied, save at the same path
        open_path = self.save_path if open_path == 'default' else open_path
        save_path = self.save_path if save_path == 'default' else save_path

        # разобраться, добавить в аргументы метода, добавить возможность других файлов
        # учесть, что для весов данные начинаются с другой даты
        # и поделить веса на 100
        # и потом в методе preprocess обработать сразу несколько файлов
        p = Path(open_path).glob('**/*')
        files = []
        for x in p:
            if x.is_file() and '.xlsx' in str(x):
                files.append(x)

        print(files)
        # sorted guarantees that we will start not from the weights file 
        # which has no observations from 1991 to 2006
        df_list = list(map(lambda x: \
                               CPIparser.preprocess_weights(x) if 'weights' in str(x)
                               else CPIparser.preprocess_file(x), files))

        merged = reduce((lambda left, right: pd.merge(left,
                                                      right,
                                                      how='left',
                                                      left_index=True,
                                                      right_index=True)), df_list)
        # rename columns after merging
        merged.columns = ['cpi', 'foods', 'nonfoods', 'services',
                          'foods_weight', 'nonfoods_weight', 'services_weight']
        merged = merged.apply(lambda x: round(x, 5))

        merged.to_csv(save_path / 'clean_data.csv')


if __name__ == '__main__':
    parser = CPIparser()
    # parser.get_gks_data()
    parser.preprocess_files()
