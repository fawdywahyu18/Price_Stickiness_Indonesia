import calendar
import requests
import pandas as pd
import numpy as np
import datetime
from tqdm.notebook import tqdm
import time

pd.options.mode.chained_assignment = None  # default='warn'

class EJPERBO:
    URL="https://siskaperbapo.jatimprov.go.id/harga/tabel"
    PASAR_ENDPOINT="https://siskaperbapo.jatimprov.go.id/harga/pasar.json/"
    ENDPOINT="https://siskaperbapo.jatimprov.go.id/harga/tabel.nodesign/"
    data = pd.DataFrame(dict(JENIS=[],NAMA=[],SATUAN=[],HARGA_KMRN=[],HARGA_SKRG=[],
                             PERUB_RP=[], PERUB_PERSEN=[], KAB=[], TANGGAL=[], PASAR=[]))
    market_data=[]

    def __init__(self, min_date, max_date, region):
        self.min_date=min_date
        self.max_date=max_date
        self.region=region
        self._market_parse(init=True)

    def _market_parse(self, init=False):
        with requests.session() as rs:
            rs.get(self.URL)
            rp=rs.get(self.PASAR_ENDPOINT+self.region)
        market_names=[rp.json()[i]['psr_nama'] for i in range(len(rp.json()))]
        market_id=[rp.json()[i]['psr_id'] for i in range(len(rp.json()))]
        if init:
            print("SISKAPERBO East Java Python Client (unofficial)")
            print("="*50)
            if self.region[-3:] == "kab":
                print("Selected region: ", self.region[:-3].capitalize())
            else:
                print("Selected region: ", self.region[:-4].capitalize())
            print("Time range: {} - {}".format(self.min_date, self.max_date))
            print("Available market: ", market_names)
            self.market_data=dict(m_names=market_names,m_id=market_id)
        else:
            return dict(m_names=market_names,m_id=market_id)

    def _time_parse(self, days, custom_range=False):
        if custom_range:
            min_date=custom_range[0]
            max_date=custom_range[1]
        else:
            min_date=self.min_date
            max_date=self.max_date
            
        min_date_l = [int(d) for d in min_date.split("-")]
        max_date_l = [int(d) for d in max_date.split("-")]
        min_date_dt = datetime.date(min_date_l[0], min_date_l[1], min_date_l[2])
        max_date_dt = datetime.date(max_date_l[0], max_date_l[1], max_date_l[2])
        time_dif = max_date_dt-min_date_dt
        num_days = time_dif.days
        if days=="all":
            time_list=[(min_date_dt+datetime.timedelta(i)).strftime("%Y-%m-%d")\
                   for i in range(num_days+1)]
        else:
            time_list=[]
            for i in range(num_days+1):
                date=min_date_dt+datetime.timedelta(i)
                if date.strftime("%A") in days:
                    time_list.append(date.strftime("%Y-%m-%d"))
        return time_list
    
    
    def _time_parse_month(self):
        start_date=int(self.min_date[-2:])
        start_month=int(self.min_date[5:7])
        start_year=int(self.min_date[:4])
        end_date=int(self.max_date[-2:])
        end_month=int(self.max_date[5:7])
        end_year=int(self.max_date[:4])

        year_list=np.arange(start_year, end_year+1, 1)

        months_start_end=[]
        for iy,year in enumerate(year_list):
            if iy==0 and len(year_list)!=1:
                initial_month=start_month
                final_month=12
            elif iy==0 and len(year_list)==1:
                initial_month=start_month
                final_month=end_month
            elif iy!=0 and iy==len(year_list)-1:
                initial_month=1
                final_month=end_month
            else:
                initial_month=1
                final_month=12
            for month in range(initial_month, final_month+1):
                range_mdate = calendar.monthrange(year, month)
                num_dates=range_mdate[1]

                if iy==0 and month==initial_month:
                    str_start=datetime.date(year,month,start_date).strftime("%Y-%m-%d")
                    str_end=datetime.date(year,month,num_dates).strftime("%Y-%m-%d")
                    months_start_end.append((str_start,str_end))
                elif iy==len(year_list)-1 and month==final_month:
                    str_start=datetime.date(year,month,1).strftime("%Y-%m-%d")
                    str_end=datetime.date(year,month,end_date).strftime("%Y-%m-%d")
                    months_start_end.append((str_start,str_end))
                else:
                    str_start=datetime.date(year,month,1).strftime("%Y-%m-%d")
                    str_end=datetime.date(year,month,num_dates).strftime("%Y-%m-%d")

                    months_start_end.append((str_start,str_end))

        return months_start_end

    def _single_query(self, payload, market_data):
        with requests.session() as rs:
            rs.get(self.URL)
            rp = rs.post(self.ENDPOINT, payload, allow_redirects=False)
            df=pd.read_html(rp.text)
            data=df[0]
            data.columns=["NO","NAMA_BAHAN_POKOK","SATUAN","HARGA_KEMARIN","HARGA_SEKARANG","PERUBAHAN_RP","PERUBAHAN_PERSEN"]

            bool_bhn=data['NO'].notnull()
            bhn_name=data['NAMA_BAHAN_POKOK']

            bhn_name_list=[]
            bhn_name_first=bhn_name[0]
            for bobh, bhnm in zip(bool_bhn, bhn_name):
                if bobh:
                    bhn_name_first=bhnm
                    bhn_name_list.append(bhn_name_first)
                else:
                    bhn_name_list.append(bhn_name_first)

            data.replace('-', np.NaN,inplace=True)
            data['NAMA_BAHAN_POKOK'] = data['NAMA_BAHAN_POKOK'].str.replace('- ','',regex=False)
            data['BHN_PKK']=bhn_name_list
            data['BHN_PKK'] = data['BHN_PKK'].str.replace('- ','',regex=False)
            data['SATUAN']=data['SATUAN'].str.lower()

            data['HARGA_KEMARIN'] = data['HARGA_KEMARIN'].astype(str).str.replace('.','',regex=False)
            data['HARGA_SEKARANG'] = data['HARGA_SEKARANG'].astype(str).str.replace('.','',regex=False)
            data['PERUBAHAN_RP'] = data['PERUBAHAN_RP'].astype(str).str.replace('.','',regex=False)
            data['PERUBAHAN_PERSEN'] = data['PERUBAHAN_PERSEN'].astype(str).str.replace('.','', regex=False)
            data['PERUBAHAN_PERSEN'] = data['PERUBAHAN_PERSEN'].astype(str).str.replace(',','.', regex=False)
            data['PERUBAHAN_PERSEN'] = data['PERUBAHAN_PERSEN'].astype(str).str.replace('%','', regex=False)
            
            data[['HARGA_KEMARIN', 'HARGA_SEKARANG','PERUBAHAN_RP','PERUBAHAN_PERSEN']] = \
                 data[['HARGA_KEMARIN', 'HARGA_SEKARANG','PERUBAHAN_RP','PERUBAHAN_PERSEN']].astype(float)
            data.insert(0, 'BHN_PKKS', data['BHN_PKK'])

            #get backup for NO nonull
            NO_nonull=data[data['NO'].notnull()]
            SATUAN_nonull=NO_nonull[NO_nonull['SATUAN'].notnull()]
            SATUAN_nonull.drop(columns=['NO', 'BHN_PKK'], inplace=True)

            data=data[data['NO'].isnull()]
            data.drop(columns=['NO', 'BHN_PKK'], inplace=True)

            #append
            data=data.append(SATUAN_nonull)

            data.columns=['JENIS','NAMA','SATUAN','HARGA_KMRN','HARGA_SKRG',"PERUB_RP","PERUB_PERSEN"]
            data['KAB'] = [payload['kabkota'][:-3].capitalize() if payload['kabkota'][-3:] == "kab" \
                           else payload['kabkota'][:4].capitalize() for i in range(len(data['JENIS']))]
            data['TANGGAL'] = [payload['tanggal'] for i in range(len(data['JENIS']))]

            #pasar
            market_index=np.where(np.array(market_data['m_id']) == payload['pasar'])[0][0]
            data['PASAR'] = [market_data['m_names'][market_index] for i in range(len(data['JENIS']))]
            return data

    def query(self, delay=2, market="all", days="all", custom_range=False):
        for date in tqdm(self._time_parse(days=days, custom_range=custom_range), \
                         desc="{}_{}".format(custom_range[0][2:4],calendar.month_name[int(custom_range[0][5:7])])):
            if market == "all":
                market_data=self.market_data
                for market_id, market_name in zip(market_data['m_id'], market_data['m_names']):
                    payload={"tanggal": date,
                             "kabkota": self.region,
                             "pasar": market_id}
                    element_day=self._single_query(payload, market_data)
                    self.data = self.data.append(element_day)
                    time.sleep(delay)
            else:
                market_data=self.market_data
                market_id, market_name = market_data['m_id'], market_data['m_names']
                market_id_index = np.where(np.array(market_name) == market)[0][0]
                payload={"tanggal": date,
                         "kabkota": self.region,
                         "pasar": market_id[market_id_index]}

                element_day=self._single_query(payload, market_data)
                self.data = self.data.append(element_day)
                time.sleep(delay)
    
    def query_by_month(self, request_delay=2, month_delay=60, market="all", days="all", max_try=2):
        for crange in tqdm(self._time_parse_month(), desc="Months"):
            failed=[]
            try:
                self.query(delay=request_delay, market=market, days=days, custom_range=crange)
                self.data.to_csv("{}_{}_{}.csv".format(self.region, crange[0].replace("-",""),\
                                                  crange[1].replace("-","")), index=False)
                self.data=pd.DataFrame(dict(JENIS=[],NAMA=[],SATUAN=[],HARGA_KMRN=[],HARGA_SKRG=[],
                             PERUB_RP=[], PERUB_PERSEN=[], KAB=[], TANGGAL=[], PASAR=[]))
            except:
                failed.append(crange)
            time.sleep(month_delay)
                
        print("failed download: ", failed)
        print("retrying...")
        tries=0
        while tries < max_try:
            for crange in failed:
                try:
                    self.query(delay=request_delay, market=market, days=days, custom_range=crange)
                    self.data.to_csv("{}_{}_{}.csv".format(self.region, crange[0].replace("-",""),\
                                                      crange[1].replace("-","")), index=False)
                    self.data=pd.DataFrame(dict(JENIS=[],NAMA=[],SATUAN=[],HARGA_KMRN=[],HARGA_SKRG=[],
                             PERUB_RP=[], PERUB_PERSEN=[], KAB=[], TANGGAL=[], PASAR=[]))
                    failed.remove(crange)
                except:
                    pass
            tries+=1
        print("query is finished")
        print("still failed after {} tries: {}".format(max_try, failed))