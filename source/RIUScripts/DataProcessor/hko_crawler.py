#!/usr/bin/env python3

import datetime
import json
import logging
import calendar
import warnings

import multiprocessing as mp
import numpy as np
import pandas as pd
import requests

from pandas.tseries.offsets import MonthEnd
from functools import reduce, partial
from bs4 import BeautifulSoup
from datetime import datetime as dt


class HKOClimatologicalInfoCrawler(object):
    """
    This is the HKO Climatological Information crawler for extracting
    weather or climate data.
    """
    def __init__(self):
        self.cis_url = "http://www.hko.gov.hk/cis/"
        self.aws_info_url = self.cis_url + "aws/awsInfo_uc.xml"
        self.aws_url = self.cis_url + "aws/aws.xml"
        self.daily_data_colnames = {"Day": "Day",
                                    "MSLP": "Mean Pressure",
                                    "MAX_TEMP": "Absolute Daily Max Temperature",
                                    "TEMP": "Mean Temperature",
                                    "MIN_TEMP": "Absolute Daily Min Temperature",
                                    "DEW_PT": "Mean Dew Point",
                                    "RH": "Mean Relative Humidity",
                                    "CLD": "Mean Amount of Cloud",
                                    "RF": "Total Rainfall",
                                    "SUNSHINE": "Total Bright Sunshine",
                                    "PREV_DIR": "Prevailing Wind Direction",
                                    "MEAN_WIND": "Mean Wind Speed"}
        self.monthly_data_colnames = {"Month": "Month",
                                      "MSLP": "Mean Pressure",
                                      "MAX_TEMP": "Absolute Daily Max Temperature",
                                      "MEAN_MAX_TEMP": "Mean Daily Max Temperature",
                                      "TEMP": "Mean Temperature",
                                      "MEAN_MIN_TEMP": "Mean Daily Min Temperature",
                                      "MIN_TEMP": "Absolute Daily Min Temperature",
                                      "DEW_PT": "Mean Dew Point",
                                      "RH": "Mean Relative Humidity",
                                      "CLD": "Mean Amount of Cloud",
                                      "RF": "Total Rainfall",
                                      "SUNSHINE": "Total Bright Sunshine",
                                      "PREV_DIR": "Prevailing Wind Direction",
                                      "MEAN_WIND": "Mean Wind Speed"}
        self.weekly_agg_type = {"MSLP": np.mean,
                                "MAX_TEMP": np.mean,
                                "TEMP": np.mean,
                                "MIN_TEMP": np.mean,
                                "DEW_PT": np.mean,
                                "RH": np.mean,
                                "CLD": np.mean,
                                "RF": np.sum,
                                "SUNSHINE": np.mean,
                                "PREV_DIR": np.mean,
                                "MEAN_WIND": np.mean}
        self.aws_available_dict = self.get_aws_desc()
        self.aws_period_dict = self.get_aws_period()
        self.aws_cis_available = self.extract_aws_cis()

    def get_aws_desc(self):
        """
        Get the available automatic weather station (aws) description.
        :return: A dictionary of all available aws.
        """
        logging.debug("Getting the available automatic weather stations description.")
        aws_info_page = requests.get(self.aws_info_url)
        aws_info_dict = json.loads(aws_info_page.content)
        aws_info_dict_out = dict()
        for aws_info in aws_info_dict["aws"]:
            aws_info_dict_out[aws_info["code"]] = {"eng": aws_info["eng"],
                                                   "chi": aws_info["chi"]}
        return aws_info_dict_out

    def get_aws_period(self):
        """
        Get the automatic weather station (aws) service period.
        :return: A dictionary of all available aws with service period.
        """
        logging.debug("Getting the service period of the available automatic weather stations.")
        aws_page = requests.get(self.aws_url)
        aws_dict = json.loads(aws_page.content)
        aws_dict_out = dict()
        for aws in aws_dict["aws"]:
            aws_dict_out[aws["code"]] = {key: aws[key]
                                         for key in aws.keys() if key != "code"}
        return aws_dict_out

    def extract_aws_cis(self):
        """
        Get the available measurement for the station: aws.
        :return: A dictionary of all available measurement for the selected aws.
        """
        logging.debug("Getting the available attributes and period .")
        cis_url = self.cis_url + "hko.xml"
        cis_page = requests.get(cis_url)
        cis_dict = json.loads(cis_page.content)
        cis_out_dict = dict()
        for item in cis_dict['hko']:
            cis_out_dict[item["code"]] = {key: item[key]
                                          for key in item.keys() if key != "code"}
        return cis_out_dict

    @staticmethod
    def __data_cleaning(df):
        df[df == "***"] = np.nan
        df.loc[df["RF"] == "Trace", "RF"] = '0'
        df.replace(regex=True, inplace=True, to_replace=r'\#', value='')
        cols = df.columns
        if "Date" in cols:
            cols = cols.drop('Date')
        if "Datetime" in cols:
            cols = cols.drop('Datetime')
        df[cols] = df[cols].apply(pd.to_numeric, errors='coerce')
        return df

    @staticmethod
    def __extract_day_data(data_list, attr_names, year, month):
        daily_data_df = pd.DataFrame.from_records(data_list,
                                                  columns=list(attr_names.keys()))
        daily_data_df.replace(regex=True, inplace=True, to_replace=r'\^', value='')
        daily_data_df["Year"] = year
        daily_data_df["Month"] = month

        monthly_data_df = daily_data_df[daily_data_df["Day"] == "Mean/Total"].drop(columns=["Day"])
        climatological_normal_df = daily_data_df[daily_data_df["Day"] == "Normal"].drop(columns=["Year", "Day"])

        daily_data_df = daily_data_df[(daily_data_df["Day"] != "Mean/Total") &
                                      (daily_data_df["Day"] != "Normal")]
        year_month = year + month
        daily_data_df["Datetime"] = pd.to_datetime(year_month + daily_data_df["Day"], format="%Y%m%d")
        daily_data_df["Date"] = daily_data_df["Datetime"].dt.date

        if daily_data_df.shape[0] > 0:
            latest_date = max(daily_data_df["Date"])
        else:
            end_day = str(calendar.monthrange(int(year), int(month))[1])
            latest_date = datetime.datetime.strptime(year_month + end_day, "%Y%m%d").date()
        monthly_data_df["Date"] = latest_date
        return [daily_data_df, monthly_data_df, climatological_normal_df]

    def __check_available_daily_attr(self, aws, month):
        attr_names = self.daily_data_colnames.copy()
        if aws.lower() == "hka":
            del attr_names["SUNSHINE"]
        elif aws.lower() == "kp":
            del attr_names["CLD"]
        elif aws.lower() == "hko" and month != '':
            del attr_names["SUNSHINE"]
            del attr_names["PREV_DIR"]
            del attr_names["MEAN_WIND"]
        else:
            if aws.lower() != "hko":
                del attr_names["SUNSHINE"]
                del attr_names["CLD"]
        return attr_names

    def __extract_daily_data(self, cis_data, aws: str, year: str, month: str):
        """
        Extract daily weather data from dictionary to DataFrame.
        :param cis_data: A list of dictionaries of data.
        :param aws: A string (3 characters) of the selected station.
        :param year: A string (4 characters) of the selected year, e.g.: "2018".
        :param month: A string (2 characters) of the selected station, '' => whole year, e.g.: "01'.
        :return: A list of DataFrames of daily data, monthly data, and climatological normal data
        """
        attr_names = self.__check_available_daily_attr(aws, month)
        daily_data_df = pd.DataFrame()
        if month != '':
            out_list = self.__extract_day_data(cis_data[0]['dayData'],
                                               attr_names, year, month)
            daily_data_df = out_list[0]
            monthly_data_df = out_list[1]
            climatological_normal_df = out_list[2]
        else:
            monthly_data_df = pd.DataFrame()
            climatological_normal_df = pd.DataFrame()
            for month_dict in cis_data:
                current_month = str(month_dict["month"])
                if len(current_month) == 1:
                    current_month = '0' + current_month
                current_day_data = month_dict.get("dayData")
                if len(current_day_data) != 0:
                    out_list_temp = self.__extract_day_data(current_day_data,
                                                            attr_names, year, current_month)
                    daily_data_df = daily_data_df.append(out_list_temp[0],
                                                         ignore_index=True)
                    monthly_data_df = monthly_data_df.append(out_list_temp[1],
                                                             ignore_index=True)
                    climatological_normal_df = climatological_normal_df.append(out_list_temp[2],
                                                                               ignore_index=True)
        daily_data_df = self.__data_cleaning(daily_data_df)
        monthly_data_df = self.__data_cleaning(monthly_data_df)
        climatological_normal_df = self.__data_cleaning(climatological_normal_df)
        return daily_data_df, monthly_data_df, climatological_normal_df

    @staticmethod
    def __extract_month_data(data_list, attr_names):
        monthly_data_df = pd.DataFrame()
        for data_dict in data_list:
            year = str(data_dict['year'])
            if 'yearData' in data_dict.keys():
                data_list = data_dict['yearData']
            elif 'monData' in data_dict.keys():
                data_list = data_dict['monData']
            else:
                raise Exception("No available monthly data for extraction.")
            monthly_data_row_df = pd.DataFrame.from_records(data_list, columns=list(attr_names.keys()))
            monthly_data_row_df['Year'] = year
            monthly_data_df = monthly_data_df.append(monthly_data_row_df, ignore_index=True)
        monthly_data_df["Date"] = pd.to_datetime(monthly_data_df["Year"] + monthly_data_df["Month"],
                                                 format="%Y%m") + MonthEnd(1)
        monthly_data_df["Date"] = monthly_data_df["Date"].dt.date
        return monthly_data_df

    def __check_available_monthly_attr(self, aws):
        attr_names = self.monthly_data_colnames.copy()
        if aws.lower() == "hka":
            del attr_names["SUNSHINE"]
        elif aws.lower() == "kp":
            del attr_names["CLD"]
        else:
            if aws.lower() != "hko":
                del attr_names["SUNSHINE"]
                del attr_names["CLD"]
        return attr_names

    def __extract_monthly_data(self, cis_data, aws: str):
        """
        Extract monthly weather data from dictionary to DataFrame.
        :param cis_data: A list of dictionaries of data.
        :param aws: A string (3 characters) of the selected station.
        :return: A DataFrame of monthly data
        """
        attr_names = self.__check_available_monthly_attr(aws)
        df_out = self.__extract_month_data(cis_data, attr_names)
        return self.__data_cleaning(df_out)

    def extract_aws_data(self, aws: str, year: str=None, month: str=None,
                         frequency: str='d'):
        """
        Extract the (period) weather or climate data for the station (aws) in
        the time period  (year, month).
        :param aws: A string (3 characters) of the selected station.
        :param year: A string (4 characters) of the selected year, e.g.: "2018".
        :param month: A string (2 characters) of the selected station, '' => whole year, e.g.: "01'. (default: None)
        :param frequency: A string (1 character) of the selected frequency, 'd' => daily, 'm'=> monthly. (default: 'd')
        :return: A pandas.DataFrame with attributes: date, type, value
        """
        if frequency == 'd':
            extract_type = "dailyExtract"
            if year is None:
                raise Exception("The parameter year is None")
            else:
                if month is None:
                    month = ''
                else:
                    if len(month) == 1:
                        month = '0' + month
                if int(year) > self.aws_cis_available[extract_type]['endYear'] or \
                        (month != '' and aws.lower() != 'hko' and
                         int(year) == self.aws_cis_available[extract_type]['endYear'] and
                         int(month) > self.aws_cis_available[extract_type]['endMonth'] + 1):
                    raise Exception("The selected time period is out of range.")
        elif frequency == 'm':
            extract_type = "monthlyExtract"
            # if int(year) > aws_cis_available[extract_type]['endYear']:
            #     Exception("The selected time period is out of range.")
        else:
            raise Exception("Wrong frequency type, either 'd' or 'm'.")
        if aws.upper() not in self.aws_available_dict.keys() and aws.lower() != "hko":
            raise Exception("The selected aws is not available.")
        elif aws.lower() != "hko":
            cis_data_url = self.cis_url + 'aws/' + extract_type + '/' + extract_type
            cis_data_url = cis_data_url + "_" + aws.upper()
        else:
            cis_data_url = self.cis_url + extract_type + '/' + extract_type
        if frequency == 'd':
            cis_data_url = cis_data_url + "_" + year + month + ".xml"
        else:
            cis_data_url = cis_data_url + ".xml"
        cis_data_page = requests.get(cis_data_url)
        if cis_data_page.status_code != 200:
            warnings.warn("The requested data: %s is not available." % cis_data_url)
            return None
        cis_data_dict = json.loads(cis_data_page.content)
        if frequency == 'd':
            return self.__extract_daily_data(cis_data_dict["stn"]["data"],
                                             aws, year, month)
        else:
            return self.__extract_monthly_data(cis_data_dict["stn"]["data"], aws)

    def __check_date(self, aws, begin_date, end_date):
        if begin_date is not None:
            begin_date = datetime.datetime.strptime(begin_date, "%Y%m%d").date()
        else:
            if aws.lower() == "hko":
                start_year = str(self.aws_cis_available['dailyExtract']['startYear'])
                start_month = str(self.aws_cis_available['dailyExtract']['startMonth'])
            else:
                start_year = str(self.aws_period_dict[aws.upper()]['startYear'])
                start_month = str(self.aws_period_dict[aws.upper()]['startMonth'])
            if len(start_month) == 1:
                start_month = '0' + start_month
                begin_date = datetime.datetime.strptime(start_year + start_month, "%Y%m").date()
        if end_date is not None:
            end_date = datetime.datetime.strptime(end_date, "%Y%m%d").date()
        else:
            if aws.lower() == "hko":
                end_year = self.aws_cis_available['dailyExtract']['endYear']
                # end_month = self.aws_cis_available['dailyExtract']['endMonth']
                previous_date = datetime.datetime.now().date() + datetime.timedelta(-1)
                end_month = str(previous_date.month)
                end_day = str(previous_date.day)
            else:
                end_year = self.aws_period_dict[aws.upper()]['endYear']
                end_month = self.aws_period_dict[aws.upper()]['endMonth']
                end_day = str(calendar.monthrange(end_year, end_month)[1])
            end_year = str(end_year)
            end_month = str(end_month)
            if len(end_month) == 1:
                end_month = '0' + end_month
            end_date = datetime.datetime.strptime(end_year + end_month + end_day, "%Y%m%d").date()
        return begin_date, end_date

    def __append_daily_data(self, aws, begin_date, end_date):
        df_out = pd.DataFrame()
        for year in range(begin_date.year, end_date.year + 1):
            df_out = df_out.append(self.extract_aws_data(aws=aws, year=str(year), frequency='d')[0], ignore_index=True)
        if end_date >= max(df_out["Date"]) and aws.lower() == "hko":
            current_month_df = self.extract_aws_data(aws=aws, year=str(end_date.year), month=str(end_date.month),
                                                     frequency='d')
            if current_month_df is not None:
                df_out = df_out.append(current_month_df[0], ignore_index=True)
        return df_out

    def get_daily_data(self, aws, begin_date=None, end_date=None):
        begin_date, end_date = self.__check_date(aws, begin_date, end_date)
        df_out = self.__append_daily_data(aws, begin_date, end_date)
        df_out = df_out[(df_out["Date"] >= begin_date) & (df_out["Date"] <= end_date)]
        df_out = df_out.drop(columns=["Datetime"])
        return df_out

    def get_weekly_data(self, aws, begin_date=None, end_date=None):
        begin_date, end_date = self.__check_date(aws, begin_date, end_date)
        daily_df = self.__append_daily_data(aws, begin_date, end_date)
        daily_df["Week"] = daily_df["Datetime"].dt.week
        daily_df["Year1"] = daily_df["Year"]
        daily_df["Year"] = np.where((daily_df["Month"] == 1) & (daily_df["Week"] >= 52),
                                    daily_df["Year"] - 1,
                                    daily_df["Year"])
        daily_df["Year"] = np.where((daily_df["Month"] == 12) & (daily_df["Week"] == 1),
                                    daily_df["Year"] + 1,
                                    daily_df["Year"])
        weekly_grouped = daily_df.groupby(["Year", "Week"])
        weekly_agg_type = {key: self.weekly_agg_type[key]
                           for key in list(daily_df.columns) if key in self.weekly_agg_type.keys()}
        weekly_agg_type["Date"] = max
        weekly_df = weekly_grouped.agg(weekly_agg_type)
        weekly_df = weekly_df[(weekly_df["Date"] >= begin_date) & (weekly_df["Date"] <= end_date)]
        weekly_df = weekly_df.reset_index()
        weekly_df["Year-Week"] = weekly_df["Year"].astype(str) + "-" + weekly_df["Week"].astype(str)
        return weekly_df

    def get_monthly_data(self, aws, begin_date=None, end_date=None):
        begin_date, end_date = self.__check_date(aws, begin_date, end_date)
        df_out = pd.DataFrame()
        df_out = df_out.append(self.extract_aws_data(aws=aws, frequency='m'), ignore_index=True)
        if end_date >= max(df_out["Date"]):
            current_month_df = self.extract_aws_data(aws=aws, year=str(end_date.year),
                                                     month=str(end_date.month), frequency='d')
            if current_month_df is not None:
                df_out = df_out.append(current_month_df[1], ignore_index=True)
        df_out = df_out[(df_out["Date"] >= begin_date) & (df_out["Date"] <= end_date)]
        df_out["Year-Month"] = df_out["Year"].astype(str) + "-" + pd.to_datetime(df_out["Date"]).dt.month.astype(str)
        return df_out

    def get_monthly_climatological_normal(self):
        year = str(datetime.datetime.now().year - 1)
        current_year_data = self.extract_aws_data('hko', year)
        return current_year_data[2]


class HKOTCWarningSignalsCrawler(object):
    """
    This is the HKO Tropical Cyclone Warning Signals Database
    crawler for extracting Tropical Cyclone Warning Signals Data.
    """
    def __init__(self):
        self.warndb_url = "http://www.hko.gov.hk/cgi-bin/hko/warndb_e1.pl"
        self.available_warnings_opt = {"TC": "1",
                                       "RAINSTORM": "3"}
        self.warnings_filters_query = {"TC": "sgnl",
                                       "RAINSTORM": "rcolor"}
        self.warnings_filters = {
            "TC":
                ["1.or.higher", "3.or.higher", "8.or.higher",
                 "9.or.higher", "1", "3", "8", "9", "10"],
            "RAINSTORM":
                ["All+colours", "Amber", "Red",
                 "Red+or+Black", "Black"]
        }

    def _check_warning_type(self, signal_type):
        if signal_type.upper() not in self.available_warnings_opt.keys():
            raise Exception("The selected warning signal type is not available.")

    def _check_filter(self, signal_type, filters):
        if filters is None:
            filters = self.warnings_filters[signal_type][0]
        else:
            if filters not in self.warnings_filters[signal_type]:
                raise Exception("The selected warning filter is not available.")
        return filters

    def extract_warning_data(self, signal_type, begin_ym, end_ym, filters=None):
        self._check_warning_type(signal_type)
        filters = self._check_filter(signal_type, filters)
        if signal_type.upper() == "TC":
            return self._extract_tc_warning(begin_ym, end_ym, filters)
        elif signal_type.upper() == "RAINSTORM":
            return self._extract_rainstorm_warning(begin_ym, end_ym, filters)
        else:
            Exception("The selected warning signal type is not available.")

    @staticmethod
    def _extract_tc_soup(tc_soup):
        def str2dt(date, time):
            str_format = "%d/%b/%Y %H:%M"
            datetime_str = " ".join([date, time])
            return dt.strptime(datetime_str, str_format)

        def _duration2min(duration):
            duration = duration.split(' ')
            return int(duration[0]) * 60 + int(duration[0])

        tc_table = tc_soup.find("table", {"cellpadding": "1",
                                          "border": "2", "background": ""})
        tc_tr_list = tc_table.findAll("tr")
        tc_list = []
        for tr in tc_tr_list:
            if "bgcolor" not in tr.attrs.keys():
                tc_type = tr.find("td", {"headers": "header1"}).find(
                    "span", {"style": "font-weight:bold;"}).text
                tc_name = tr.find("td", {"headers": "header2"}).find(
                    "span", {"style": "font-weight:bold;"}).text
                tc_signal_no = tr.find("td", {"headers": "header3"}).find(
                    "span", {"style": "font-weight:bold;"}).text.strip()
                tc_begin_time = tr.find("td", {
                    "headers": "header4 header7"}).find(
                    "span", {"style": "font-weight:bold;"}).text
                tc_begin_date = tr.find("td", {
                    "headers": "header4 header8"}).find(
                    "span", {"style": "font-weight:bold;"}).text
                tc_end_time = tr.find("td", {
                    "headers": "header5 header9"}).find(
                    "span", {"style": "font-weight:bold;"}).text
                tc_end_date = tr.find("td", {
                    "headers": "header5 header10"}).find(
                    "span", {"style": "font-weight:bold;"}).text
                tc_duration = tr.find("td", {
                    "headers": "header6"}).find(
                    "span", {"style": "font-weight:bold;"}).text
                tc_list.append({"Intensity": tc_type, "Name": tc_name,
                                "Signal": tc_signal_no,
                                "Begin": str2dt(tc_begin_date, tc_begin_time),
                                "End": str2dt(tc_end_date, tc_end_time),
                                "Duration": _duration2min(tc_duration)})
        return pd.DataFrame(tc_list)

    @staticmethod
    def _extract_rainstorm_soup(rain_soup):
        def str2dt(date, time):
            str_format = "%d/%b/%Y %H:%M"
            datetime_str = " ".join([date, time])
            return dt.strptime(datetime_str, str_format)

        def _duration2min(duration):
            duration = duration.split(' ')
            return int(duration[0]) * 60 + int(duration[0])

        rain_table = rain_soup.find("table", {"cellpadding": "2",
                                              "border": "2", "background": ""})
        rain_tr_list = rain_table.findAll("tr")
        rain_list = []
        for tr in rain_tr_list:
            if "bgcolor" not in tr.attrs.keys():
                rain_type = tr.find("td", {"headers": "header1"}).find(
                    "span", {"style": "font-weight:bold;"}).text
                rain_begin_time = tr.find("td", {
                    "headers": "header2 header5"}).find(
                    "span", {"style": "font-weight:bold;"}).text
                rain_begin_date = tr.find("td", {
                    "headers": "header2 header6"}).find(
                    "span", {"style": "font-weight:bold;"}).text
                rain_end_time = tr.find("td", {
                    "headers": "header3 header7"}).find(
                    "span", {"style": "font-weight:bold;"}).text
                rain_end_date = tr.find("td", {
                    "headers": "header3 header8"}).find(
                    "span", {"style": "font-weight:bold;"}).text
                rain_duration = tr.find("td", {
                    "headers": "header4"}).find(
                    "span", {"style": "font-weight:bold;"}).text
                rain_list.append({"Color": rain_type,
                                  "Begin": str2dt(rain_begin_date, rain_begin_time),
                                  "End": str2dt(rain_end_date, rain_end_time),
                                  "Duration": _duration2min(rain_duration)})
        return pd.DataFrame(rain_list)

    def _extract_tc_warning(self, begin_ym, end_ym, filters=None):
        warndb_url = self._url_concat("TC", begin_ym, end_ym, filters)
        logging.debug("Getting the TC warning signals.")
        warndb_page = requests.get(warndb_url)
        soup = BeautifulSoup(warndb_page.content)
        df = self._extract_tc_soup(soup)
        return df

    def _extract_rainstorm_warning(self, begin_ym, end_ym, filters=None):
        warndb_url = self._url_concat("RAINSTORM", begin_ym,
                                      end_ym, filters)
        logging.debug("Getting the rainstorm warning signals.")
        warndb_page = requests.get(warndb_url)
        soup = BeautifulSoup(warndb_page.content)
        df = self._extract_rainstorm_soup(soup)
        return df

    def _url_concat(self, signal_type, begin_ym, end_ym, filters=None):
        warndb_url = self.warndb_url + "?opt=" + \
                    self.available_warnings_opt[signal_type]
        filter_query = "=".join([self.warnings_filters_query[signal_type],
                                 filters])
        start_ym = "=".join(["start_ym", begin_ym])
        end_ym = "=".join(["end_ym", end_ym])
        warndb_url = "&".join([warndb_url, filter_query, start_ym, end_ym,
                               "submit=Submit+Query"])
        return warndb_url


if __name__ == '__main__':
    hko_crawler = HKOClimatologicalInfoCrawler()
    print(hko_crawler.get_weekly_data(aws="hko"))
    print("\n-------------\n")
    print(hko_crawler.get_weekly_data(aws="hka"))
    print("\n-------------\n")
    print(hko_crawler.get_weekly_data(aws="kp"))
    print("\n-------------\n")
    print(hko_crawler.get_weekly_data(aws="hko", begin_date="20140101",  end_date="20180706"))
    print("\n-------------\n")
    print(hko_crawler.get_weekly_data(aws="hka", begin_date="20140101",  end_date="20180706"))
    print("\n-------------\n")
    print(hko_crawler.get_weekly_data(aws="kp", begin_date="20140101",  end_date="20180706"))
    print("\n-------------\n")
    print(hko_crawler.get_daily_data(aws="hko"))
    print("\n-------------\n")
    print(hko_crawler.get_daily_data(aws="hka"))
    print("\n-------------\n")
    print(hko_crawler.get_daily_data(aws="kp"))
    print("\n-------------\n")
    print(hko_crawler.get_daily_data(aws="hko", begin_date="20140101",  end_date="20180706"))
    print("\n-------------\n")
    print(hko_crawler.get_daily_data(aws="hka", begin_date="20140101", end_date="20180706"))
    print("\n-------------\n")
    print(hko_crawler.get_daily_data(aws="kp", begin_date="20140101", end_date="20180706"))
    print("\n-------------\n")
    print(hko_crawler.get_monthly_data(aws="hko"))
    print("\n-------------\n")
    print(hko_crawler.get_monthly_data(aws="hka"))
    print("\n-------------\n")
    print(hko_crawler.get_monthly_data(aws="kp"))
    print("\n-------------\n")
    print(hko_crawler.get_monthly_data(aws="hko", begin_date="20140101", end_date="20180706"))
    print("\n-------------\n")
    print(hko_crawler.get_monthly_data(aws="hka", begin_date="20140101", end_date="20180706"))
    print("\n-------------\n")
    print(hko_crawler.get_monthly_data(aws="kp", begin_date="20140101", end_date="20180706"))
    print("\n-------------\n")
    print(hko_crawler.get_monthly_climatological_normal())
    warning_signal_crawler = HKOTCWarningSignalsCrawler()
    warning_signal_crawler.extract_warning_data("TC", "201712", "201812")
    warning_signal_crawler.extract_warning_data("RAINSTORM", "201712", "201812")
