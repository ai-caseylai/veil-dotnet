#!/usr/bin/env python3

import pandas as pd
import workalendar
from datetime import date, timedelta
from workalendar.asia import HongKong


class HolidayCrawler(object):
    """
    This is a Hong Kong holiday crawler.
    """
    def __init__(self):
        self.days = {0: 'Monday', 1: 'Tuesday', 2: 'Wednesday',
                     3: 'Thursday', 4: 'Friday', 5: 'Saturday', 6: 'Sunday'}
        self.cal = HongKong()

    def get_holiday(self, year: int, month: int=None):
        holiday_list = self.cal.holidays(year)
        holiday_df = pd.DataFrame.from_records(holiday_list,
                                               columns=['Date', 'Holiday'])
        if month is not None:
            month_select = (holiday_df['Date'].dt.month == month)
            holiday_df = holiday_df[month_select]
        return holiday_df

    @staticmethod
    def __all_days(year: int, day: int, month: int=None):
        """
        Get all dates of day in the year
        :param year:
        :param day: 0 is Monday, ... , 6 is Sunday
        :param month:
        :return:
        """
        if month is None:
            dt = date(year, 1, 1)
        else:
            dt = date(year, month, 1)
        dt += timedelta(days=day - dt.weekday())
        if dt.year < year:
            dt += timedelta(days=7)
        while dt.year == year:
            yield dt
            if month is not None:
                dt_check = dt + timedelta(days=7)
                if dt_check.month > month:
                    break
            dt += timedelta(days=7)
        return dt

    def get_weekend(self, year: int, month: int=None):
        self.__check_month(month)
        sat_dt = self.__all_days(year, 5, month)
        sat_dt_list = [[dt] for dt in sat_dt]
        sat_df = pd.DataFrame.from_records(sat_dt_list, columns=['Date'])
        sat_df['Holiday'] = self.days[5]

        sun_dt = self.__all_days(year, 6, month)
        sun_dt_list = [[dt] for dt in sun_dt]
        sun_df = pd.DataFrame.from_records(sun_dt_list, columns=['Date'])
        sun_df['Holiday'] = self.days[6]

        return pd.concat([sat_df, sun_df])

    @staticmethod
    def __check_month(month: int):
        if month is not None and (month < 1 or month > 12):
            raise Exception('The month is out of range.')


if __name__ == '__main__':
    holiday_crawler = HolidayCrawler()
    weekend_df = holiday_crawler.get_weekend(2018)
    holidays_df = holiday_crawler.get_holiday(2018)
    weekend_df = weekend_df[
        ~weekend_df.isin({'Date': list(holidays_df['Date'])})['Date']]
    df_out = pd.concat([holidays_df, weekend_df])
    df_out = df_out.sort_values(by='Date')
    print(df_out)
