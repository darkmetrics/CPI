import requests
import pandas as pd
from selenium import webdriver
from pathlib import Path
from datetime import datetime
from functools import reduce


# Мне так и не удалось запустить для сайта Росстата в headless режиме
# но с запуском браузера всё работает на ура
# надо попробовать посмотреть на размер открываемого окна и другие варианты

class CPIparser:
    """
    Downloads CPI data files from Rosstat website and
    preprocesses them
    """

    def __init__(self,
                 chromepath=r'C:\Program Files\chromedriver.exe',
                 url='https://rosstat.gov.ru/price',
                 save_path=Path('D:/Macro/CPI/data/')):
        # path to executable Chrome on PC
        self.chromepath = chromepath
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
        Opens Chrome webdriver and loads data for CPI from GKS website
        """
        # start webdriver
        driver = webdriver.Chrome(executable_path=self.chromepath)
        # go to start url
        driver.get(self.url)
        # get urls for data files
        price_path = driver.find_element_by_xpath(
            '//a[contains(@href,"Индексы потребительских цен")]').get_attribute("href")
        weights_path = driver.find_element_by_xpath(
            '//a[contains(@href,"vesa")]').get_attribute("href")

        # над этим минут десять мучался, надо преобразовать ссылку в формат строки
        driver.get(str(price_path))
        links = [str(y.get_attribute('href')) for y in driver.find_elements_by_tag_name('a')] + [str(weights_path)]
        names = [' '.join(str(y.get_attribute('text')).split()).replace('-', '').strip()
                 for y in driver.find_elements_by_tag_name('a')] + ['weights']

        translations = {
            'на товары и услуги': 'cpi',
            'на продовольственные товары': 'foods',
            'на непродовольственные товары': 'nonfoods',
            'на услуги': 'services',
            'weights': 'weights'
        }

        link_dict = dict(zip(names, links))

        # добавить многопоточную обработку, чтоб быстрее было
        # iterate over links with data and load files on PC
        for name, link in link_dict.items():
            fpath = self.save_path / str(translations[name] + '.xlsx')
            # add "override existing file?" Yes/No option

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

        driver.close()

    @staticmethod
    def preprocess_file(file_path):
        print(file_path)
        # select rows to skip in raw xlsx files
        skip = list(range(3)) + [4]

        if 'weights' in file_path:
            skip = list(range(4)) + [5]

            df = pd.read_excel(file_path, skiprows=skip, nrows=50, engine='openpyxl')
            print(df.head(5))

            df.rename(columns={df.columns.values[0]: 'month'}, inplace=True)
            df.drop(columns=list(filter(lambda x: 'Unnamed' in str(x),
                                        df.columns)), inplace=True)
            df = df.melt(id_vars=['month'],
                         value_vars=df.columns[1:],
                         var_name='year',
                         value_name='cpi')

        else:
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
        print(f'Data for {file_path} is reformated')
        # добавить имена, как df['cpi']
        return df

    def preprocess_files(self,
                         open_path='default',
                         save_path='default',
                         file_name='clean_cpi.csv'):
        # Здесь препроцессинг только одного файла, а надо все
        '''
        '''
        # if no specific path to save data is supplied, save at the same path
        open_path = self.save_path if open_path == 'default' else open_path
        save_path = self.save_path if save_path == 'default' else save_path

        # разобраться, добавить в аргументы метода, добавить возможность других файлов
        # учесть, что для весов данные начинаются с другой даты
        # и поделить веса на 100
        # и потом в методе preprocess обработать сразу несколько файлов
        files = [x for x in Path(open_path).glob() if x.is_file() and '.xlsx' in x]
        print(files)
        # sort to start merging files not from 'weights.xlsx'
        # because weights.xlsx doesn't have data for 2000-2006
        pass


if __name__ == '__main__':
    parser = CPIparser()
    parser.get_gks_data()
    parser.preprocess_files()
