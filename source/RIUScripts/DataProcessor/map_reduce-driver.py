#!/usr/bin/env python3

import glob
import logging
import time
import datetime
import os
import hko_crawler as hko
import map_reduce as mr
import holiday_crawler as holiday
import multiprocessing as mp
import pandas as pd
import argparse
import copy

from tornado import web, ioloop, gen
from tornado.concurrent import run_on_executor
from tornado.log import enable_pretty_logging

from concurrent.futures import ThreadPoolExecutor
from functools import partial
from os import listdir
from os.path import isfile, join

MAX_WORKERS = mp.cpu_count()
DATA_PATH = ""


class IndexHandler(web.RequestHandler):
    @gen.coroutine
    def get(self):
        self.render("index1.html")


class ApiHandler(web.RequestHandler):
    executor = ThreadPoolExecutor(max_workers=MAX_WORKERS)

    @run_on_executor
    def get_holidays(self, period, type=None):
        begin = period[0]
        end = period[1]
        holiday_crawler = holiday.HolidayCrawler()
        begin_date = datetime.datetime.strptime(begin, "%Y%m%d").date()
        end_date = datetime.datetime.strptime(end, "%Y%m%d").date()
        df_out = pd.DataFrame()
        for year in range(begin_date.year, end_date.year + 1):
            weekend_df = holiday_crawler.get_weekend(year)
            holidays_df = holiday_crawler.get_holiday(year)
            if type == 'weekend':
                df_out = pd.concat([df_out, weekend_df])
            elif type == 'holiday':
                df_out = pd.concat([df_out, holidays_df])
            else:
                weekend_df = weekend_df[
                    ~weekend_df.isin({'Date': list(holidays_df['Date'])})['Date']]
                df_out = pd.concat([df_out, weekend_df, holidays_df])
        df_out = df_out[(df_out["Date"] >= begin_date) &
                        (df_out["Date"] <= end_date)]
        df_out = df_out.sort_values(by='Date')
        return df_out

    @run_on_executor
    def get_weather_data(self, aws, attributes=None, period=None,
                         dtype='d'):
        if period is None:
            begin = None
            end = None
        else:
            begin = period[0]
            end = period[1]
        if aws.upper() not in ["TC", "RAINSTORM"]:
            hko_crawler = hko.HKOClimatologicalInfoCrawler()
            if dtype.lower() == 'd':
                result_df = hko_crawler.get_daily_data(aws,
                                                       begin_date=begin, end_date=end)
            elif dtype.lower() == 'w':
                result_df = hko_crawler.get_weekly_data(aws,
                                                        begin_date=begin, end_date=end)
            elif dtype.lower() == 'm':
                result_df = hko_crawler.get_monthly_data(aws,
                                                         begin_date=begin, end_date=end)
            elif dtype.lower() == 'n':
                result_df = hko_crawler.get_monthly_climatological_normal()
            else:
                raise Exception("The type: %s is not available." % dtype)
            if attributes is not None:
                cols = result_df.columns
                if "Date" in cols:
                    attributes = attributes.append("Date")
                if "Day" in cols:
                    attributes = attributes.append("Day")
                if "Month" in cols:
                    attributes = attributes.append("Month")
                if "Year" in cols:
                    attributes = attributes.append("Year")
                attr_out = pd.Index([attr for attr in attributes if attr in list(result_df.columns)])
                return result_df[attr_out]
            else:
                return result_df
        else:
            warning_signal_crawler = hko.HKOTCWarningSignalsCrawler()
            result_df = warning_signal_crawler.extract_warning_data(aws, begin, end)
            return result_df

    @run_on_executor
    def split_apply_combine(self, bu, agg_type=None, keys=None,
                            attributes=None, period=None, period_unit='D',
                            cumqty_type=2):
        if bu == 107:
            data_path = os.path.join(DATA_PATH, '7Fans/txn/')
        elif bu == 106:
            data_path = os.path.join(DATA_PATH, 'Bauhaus/txn/')
        ddict = mr.read_ddict(os.path.join(data_path, 'ddict.json'))
        datetime_col = 'Timestamp'
        date_format = ddict['format'][datetime_col]
        files = [f for f in listdir(data_path) if isfile(join(data_path, f)) and 
                 '.csv.xz' in f and not(any([s in f for s in ['.tmp', '.bak', 'old-']]))]

        if agg_type is None:
            mapper_tmp = partial(mr.mapper, bu=bu, ddict=ddict,
                                 func=None, attributes=attributes, period=period,
                                 date_format=date_format)
            reducer_tmp = None
            keys_combine = None
            func_combine = None
        elif agg_type.lower() == "cumqty":
            mapper_tmp = partial(mr.mapper, bu=bu, ddict=ddict,
                                 func=partial(mr.item_qty, keys='MemberID',
                                              agg_type=cumqty_type), period=period,
                                 date_format=date_format)
            reducer_tmp = partial(mr.reducer, func=mr.cum_qty,
                                  vectorize=True)
            keys_combine = ['MemberID', 'Item']
            func_combine = None
        elif agg_type.lower() == "recency":
            if keys is None:
                keys = "MemberID"
                keys_combine = keys
            else:
                if isinstance(keys, str):
                    keys = [keys]
                keys_combine = copy.deepcopy(keys)
                keys_lower = [k.lower() for k in keys]
                if "date" in keys_lower:
                    keys[keys_lower.index("date")] = "Date"
                    keys_combine[keys_lower.index("date")] = "Date"
                elif "week" in keys_lower:
                    keys[keys_lower.index("week")] = "Week"
                    keys_combine[keys_lower.index("week")] = "Year-Week"
                elif "month" in keys_lower:
                    keys[keys_lower.index("month")] = "Month"
                    keys_combine[keys_lower.index("month")] = "Year-Month"
            mapper_tmp = partial(mr.mapper, bu=bu, ddict=ddict,
                                 func=partial(mr.last_txn_date, keys=keys),
                                 period=period, date_format=date_format)
            reducer_tmp = partial(mr.reducer, func=mr.last_txn,
                                  vectorize=True)
            func_combine = partial(mr.date_from_last_txn, unit=period_unit)
        elif agg_type.lower() == "avgspend":
            if keys is None:
                keys = "MemberID"
                keys_combine = keys
            else:
                if isinstance(keys, str):
                    keys = [keys]
                keys_combine = copy.deepcopy(keys)
                keys_lower = [k.lower() for k in keys]
                if "date" in keys_lower:
                    keys[keys_lower.index("date")] = "Date"
                    keys_combine[keys_lower.index("date")] = "Date"
                elif "week" in keys_lower:
                    keys[keys_lower.index("week")] = "Week"
                    keys_combine[keys_lower.index("week")] = "Year-Week"
                elif "month" in keys_lower:
                    keys[keys_lower.index("month")] = "Month"
                    keys_combine[keys_lower.index("month")] = "Year-Month"
            mapper_tmp = partial(mr.mapper, bu=bu, ddict=ddict,
                                 func=partial(mr.amt_of_spending, keys=keys),
                                 period=period, date_format=date_format)
            reducer_tmp = partial(mr.reducer, func=mr.avg_spending,
                                  vectorize=True)
            func_combine = None
        elif agg_type.lower() == "totspend":
            if keys is None:
                keys = "MemberID"
                keys_combine = keys
            else:
                if isinstance(keys, str):
                    keys = [keys]
                keys_combine = copy.deepcopy(keys)
                keys_lower = [k.lower() for k in keys]
                if "date" in keys_lower:
                    keys[keys_lower.index("date")] = "Date"
                    keys_combine[keys_lower.index("date")] = "Date"
                elif "week" in keys_lower:
                    keys[keys_lower.index("week")] = "Week"
                    keys_combine[keys_lower.index("week")] = "Year-Week"
                elif "month" in keys_lower:
                    keys[keys_lower.index("month")] = "Month"
                    keys_combine[keys_lower.index("month")] = "Year-Month"
            mapper_tmp = partial(mr.mapper, bu=bu, ddict=ddict,
                                 func=partial(mr.amt_of_spending, keys=keys),
                                 period=period, date_format=date_format)
            reducer_tmp = partial(mr.reducer, func=mr.tot_spending,
                                  vectorize=True)
            func_combine = None
        elif agg_type.lower() == "frequency":
            if keys is None:
                keys = "MemberID"
                keys_combine = keys
            else:
                if isinstance(keys, str):
                    keys = [keys]
                keys_combine = copy.deepcopy(keys)
                keys_lower = [k.lower() for k in keys]
                if "date" in keys_lower:
                    keys[keys_lower.index("date")] = "Date"
                    keys_combine[keys_lower.index("date")] = "Date"
                elif "week" in keys_lower:
                    keys[keys_lower.index("week")] = "Week"
                    keys_combine[keys_lower.index("week")] = "Year-Week"
                elif "month" in keys_lower:
                    keys[keys_lower.index("month")] = "Month"
                    keys_combine[keys_lower.index("month")] = "Year-Month"
            mapper_tmp = partial(mr.mapper, bu=bu, ddict=ddict,
                                 func=partial(mr.no_of_txn, keys=keys),
                                 period=period, date_format=date_format)
            reducer_tmp = partial(mr.reducer, func=mr.tot_txn,
                                  vectorize=True)
            func_combine = None
        elif agg_type.lower() == 'rfm':
            mapper_tmp = partial(mr.mapper, bu=bu, ddict=ddict,
                                 func=partial(mr.extract_rfm_data),
                                 period=period, date_format=date_format)
            reducer_tmp = partial(mr.reducer, func=mr.compute_rfm_stat,
                                  vectorize=True)
            keys_combine = 'MemberID'
            func_combine = None
        elif agg_type.lower() == 'mba':
            attributes = ['OrderID', 'MemberID', 'ProductID', 'ProductName',
                          'Category', 'NetPrice']
            if bu == 107:
                attributes.append('CardNumber')
            mapper_tmp = partial(mr.mapper, bu=bu, ddict=ddict,
                                 func=None, attributes=attributes, period=period,
                                 date_format=date_format)
            reducer_tmp = None
            keys_combine = None
            func_combine = None
        result = mr.map_reduce(mapper_tmp, reducer_tmp,
                               [os.path.join(data_path, f) for f in files] +
                               glob.glob(os.path.join(data_path, "*/*.csv.xz")),
                               keys_combine, func_combine)
        return result

    @gen.coroutine
    def get(self, *args):
        input_args = self.request.arguments
        input_args = list(input_args.keys())
        bu = self.get_argument("bu")
        output_type = "csv"
        if "out" in input_args:
            output_type = self.get_argument("out")
        agg_type = None
        if "type" in input_args:
            agg_type = self.get_argument("type")
        keys = None
        if "keys" in input_args:
            keys = self.get_argument("keys")
            if ',' in keys:
                keys = keys.split(',')
        attributes = None
        if "attributes" in input_args:
            attributes = self.get_argument("attributes")
            attributes = attributes.split(',')
        if "begin" in input_args:
            begin = self.get_argument("begin")
        else:
            begin = None
        if "end" in input_args:
            end = self.get_argument("end")
        else:
            end = None
        period = [begin, end]
        if all([item is None for item in period]):
            period = None
        period_unit = 'D'
        if "unit" in input_args:
            period_unit = self.get_argument("unit")
        cumqty_type = 2
        if "cumqty" in input_args:
            cumqty_type = int(self.get_argument("cumqty"))
        if bu != "hko":
            if bu == "holiday":
                res_out = yield self.get_holidays(period, agg_type)
                out_filename = "holiday_extract." + output_type
            else:
                bu = int(bu)
                res_out = yield self.split_apply_combine(bu, agg_type, keys,
                                                         attributes, period, period_unit,
                                                         cumqty_type)
                out_filename = "data_extract." + output_type
        else:
            if "aws" in input_args:
                aws = self.get_argument("aws")
            else:
                aws = "hko"
            res_out = yield self.get_weather_data(aws, attributes, period, agg_type)
            out_filename = "weather_extract." + output_type

        self.set_header('Content-Type', 'application/octet-stream')
        self.set_header('Content-Disposition',
                        'attachment; filename=%s' % out_filename)
        
        if output_type == "json":
            self.write(res_out.to_json(orient='records'))
        else:
            self.write(res_out.to_csv(index=False))
        self.finish()
        
#    @web.asynchronous
#    def post(self):
#        pass


app = web.Application([
    (r'/', IndexHandler),
    (r'/api', ApiHandler)
])

if __name__ == '__main__':
    parser = argparse.ArgumentParser(
            description='Data processor',
            formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument('--datapath', type=str, default='../Data/',
                        dest='data_path', help='Data path')
    parser.add_argument('--port', type=int, default=8888,
                        dest='port', help='port number')
    parser.add_argument('--logpath', type=str, default='./log/',
                        dest='log_path', help='Log path')
    parser.add_argument('--loglevel', type=str, default="info",
                        dest='loglevel', help='Logging level')
    args = parser.parse_args()
    
    DATA_PATH = args.data_path
    log_path = args.log_path
    if not os.path.exists(log_path):
        os.mkdir(log_path)
    log_filename = log_path + time.strftime("%Y%m%d%H%M%S") + ".log"
    
    numeric_level = getattr(logging, args.loglevel.upper(), None)
    if not isinstance(numeric_level, int):
        raise ValueError('Invalid log level: %s' % args.loglevel)
    logging.basicConfig(level=numeric_level)
    log_handler = logging.FileHandler(log_filename)
    access_log = logging.getLogger("tornado.access")
    app_log = logging.getLogger("tornado.application")
    gen_log = logging.getLogger("tornado.general")
    enable_pretty_logging()
    access_log.addHandler(log_handler)
    app_log.addHandler(log_handler)
    gen_log.addHandler(log_handler)
    
    app.listen(args.port)
    ioloop.IOLoop.instance().start()
