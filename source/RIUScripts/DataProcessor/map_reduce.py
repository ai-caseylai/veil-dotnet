#!/usr/bin/env python3

import copy
import json
import multiprocessing as mp
import numpy as np
import pandas as pd
from utils.master import rename_data

def _get_bauhaus_clean_data():
    from utils.bauhaus import clean_data
    return clean_data

def _get_sevenfans_clean_data():
    from utils.sevenFans import clean_data
    return clean_data
from functools import reduce, partial
from itertools import chain


def read_ddict(path):
    ddict_json = json.load(open(path))
    ddict = {}
    for key in ddict_json.keys():
        if 'type' == key:
            ddict[key] = {}
            for inner_key in ddict_json[key].keys():
                ddict[key][inner_key] = eval(ddict_json[key][inner_key])
        else:
            ddict[key] = ddict_json[key]
    return ddict


def read_multiple_csv(path, ddict, bu):
    if bu == 107:
        data_cleaner = _get_sevenfans_clean_data()
    elif bu == 106:
        data_cleaner = _get_bauhaus_clean_data()
    else:
        return None
    if isinstance(path, list):
        return pd.concat([rename_data(data_cleaner(
                pd.read_csv(f, dtype=ddict['type'])), ddict) for f in path ])
    else:
        return rename_data(data_cleaner(
            pd.read_csv(path, dtype=ddict['type'])), ddict)


def item_qty(df, keys='MemberID', agg_type=1, 
             attribute={'Quantity': 'Quantity', 'Category': 'Category',
                        'ProductID': 'ProductID'}):
    if agg_type == 2:  # by sub-department
        df['Item'] = df[attribute['Category']]
    elif agg_type == 3:  # by product
        df['Item'] = df[attribute['ProductID']]
    else:  # by department
        df['Item'] = df[attribute['Category']].str[0:3]
    grp = df.groupby([keys, 'Item'], as_index=False)
    sum_qty_df = pd.DataFrame(grp[attribute['Quantity']].sum())
    sum_qty_df = sum_qty_df.rename(columns={keys: keys,
                                      'Item': 'Item',
                                      attribute['Quantity']: 'Quantity'})
    return sum_qty_df


def cum_qty(extract, attribute='Quantity', vectorize=False):
    if vectorize:
        col_sel = list(set([col for col in extract.columns if attribute in col]))
        return vec_cum_qty(extract[col_sel].to_numpy())
    else:
        row_dict = extract.to_dict()
        col_selected = [key for key in row_dict.keys() if attribute in key]
        return np.sum(extract[col_selected])


def vec_cum_qty(quantity_array):
    return np.nansum(quantity_array, axis=1)


def last_txn_date(df, keys='MemberID',
                  attribute={'OrderID': 'OrderID', 'Timestamp': 'Timestamp'}):
    # Get the possible attributes
    attri = [i for i in attribute.values()]
    # Get the groupby keys
    keys_grp = [attribute['OrderID'], attribute['Timestamp']]
    if isinstance(keys, list):
        for key in keys:
            attri.append(key)
            keys_grp.append(key)
    else:
        attri.append(keys)
        keys_grp.append(keys)
    # Get the subset of the dataframe
    df_temp = df[attri]
    # Compute the last txn date
    grp = df_temp.groupby(keys, as_index=False)
    last_txn_df = pd.DataFrame(grp[attribute['Timestamp']].max())
    last_txn_df = last_txn_df.rename(columns={keys: keys, 
                                        attribute['Timestamp']: 'Timestamp'})
    return last_txn_df


def last_txn(extract, attribute='Timestamp', vectorize=False):
    if vectorize:
        col_sel = list(set([col for col in extract.columns if attribute in col]))
        return vec_last_txn(extract[col_sel].to_numpy())
    else:
        row_dict = extract.to_dict()
        col_selected = [key for key in row_dict.keys() if attribute in key]
        return np.max(extract[col_selected].dropna())


def vec_last_txn(timestamp_array):
    return np.max(timestamp_array, axis=1)


def date_from_last_txn(df, unit='D'):
    '''
        df is a dataframe.
        unit is either 'D' (Date), 'W' (Week), 'M' (Month).
    '''
    df_temp = df.copy()
    df_temp['value'] = df_temp['value'].max()-df_temp['value']
    df_temp['value'] = df_temp['value']/np.timedelta64(1, unit)
    return df_temp


def amt_of_spending(df, keys='MemberID', 
                    attribute={'OrderID': 'OrderID',
                               'Timestamp': 'Timestamp',
                               'Price': 'NetPrice',
                               'Quantity': 'Quantity'}):
    # Get the possible attirbutes
    attri = [i for i in attribute.values()]
    # Get the groupby keys
    keys_grp = [attribute['OrderID'], attribute['Timestamp']]
    if isinstance(keys, list):
        for key in keys:
            attri.append(key)
            keys_grp.append(key)
    else:
        attri.append(keys)
        keys_grp.append(keys)
    # Get the subset of the dataframe
    df_temp = df[attri]
    # Compute the total spending by Price * Quantity for each record
    df_temp = df_temp.assign(Total=pd.Series(
        np.asarray(df_temp[attribute['Price']]) *
        np.asarray(df_temp[attribute['Quantity']]), index=df_temp.index))
    # Compute the total spending for each OrderID
    grp = df_temp.groupby(keys_grp, as_index=False)
    tot_spending_df = pd.DataFrame(grp['Total'].sum())
    # Compute the total spending for each customer
    grp_keys = tot_spending_df.groupby(keys, as_index=False)
    spending_member_df = pd.DataFrame(grp_keys['Total'].sum())
    spending_member_df = spending_member_df.reset_index(drop=True)
    # Compute the number of transactions for each customer
#    txn_member_df = pd.DataFrame(grp_keys[attribute['OrderID']].nunique())
#    txn_member_df[keys] = txn_member_df.index
    txn_member_df = no_of_txn(df, keys=keys,
                              attribute={'OrderID' : attribute['OrderID']})
    txn_member_df = txn_member_df.reset_index(drop=True)
    return pd.merge(spending_member_df, txn_member_df, on=keys)


def tot_spending(extract, attribute='Total', vectorize=False):
    if vectorize:
        col_sel = list(set([col for col in extract.columns if attribute in col]))
        return vec_tot_spending(extract[col_sel].to_numpy())
    else:
        row_dict = extract.to_dict()
        col_selected = [key for key in row_dict.keys() if attribute in key]
        return np.sum(extract[col_selected].dropna())


def vec_tot_spending(total_array):
    return np.nansum(total_array, axis=1)


def avg_spending(extract, attribute='Total', vectorize=False):
    tot_spending_tmp = tot_spending(extract, vectorize=vectorize)
    tot_txn_tmp = tot_txn(extract, vectorize=vectorize)
    if vectorize:
        return vec_avg_spending(tot_spending_tmp, tot_txn_tmp)
    else:
        return tot_spending_tmp/tot_txn_tmp


def vec_avg_spending(tot_spending_array, tot_txn_array):
    return np.divide(tot_spending_array, tot_txn_array)


def no_of_txn(df, keys='StoreID', attribute={'OrderID': 'OrderID'}):
    if isinstance(keys, list):
        selected_columns = [key for key in keys]
        selected_columns.append(attribute['OrderID'])
    else:
        selected_columns = [keys, attribute['OrderID']]
    selected_columns_lower = [item.lower() for item in selected_columns]

    agg_funcs = {attribute['OrderID']: pd.Series.nunique}
    if "year-week" in selected_columns_lower or \
            "year-month" in selected_columns_lower:
        if "Date" in list(df.columns):
            selected_columns.append("Date")
            agg_funcs["Date"] = max

    grp = df[selected_columns].groupby(keys)
    result = grp.agg(agg_funcs)
    if isinstance(keys, list):
        result = result.reset_index()
        result = result.rename(columns={attribute['OrderID']: 'OrderID'})
    else:
        result[keys] = result.index
        result = result.rename(columns={keys: keys,
                                        attribute['OrderID']: 'OrderID'})
    return result


def tot_txn(extract, attribute='OrderID', vectorize=False):
    if any([col for col in extract.columns if 'Date' in col]):
        df_out = pd.DataFrame(index=extract.index)
        if vectorize:
            date_col_sel = list(set([col for col in extract.columns if 'Date' in col]))
            df_out["Date"] = extract[date_col_sel].apply(pd.to_datetime).max(axis=1).dt.date
            col_sel = list(set([col for col in extract.columns if attribute in col]))
            df_out["value"] = vec_tot_txn(extract[col_sel].to_numpy())
        else:
            row_dict = extract.to_dict()
            date_col_selected = [key for key in row_dict.keys() if 'Date' in key]
            df_out["Date"] = np.max(extract[date_col_selected].dopna())
            col_selected = [key for key in row_dict.keys() if attribute in key]
            df_out["value"] = np.sum(extract[col_selected].dropna())
        return df_out
    else:
        if vectorize:
            col_sel = list(set([col for col in extract.columns if attribute in col]))
            return vec_tot_txn(extract[col_sel].to_numpy())
        else:
            row_dict = extract.to_dict()
            col_selected = [key for key in row_dict.keys() if attribute in key]
            return np.sum(extract[col_selected].dropna())


def vec_tot_txn(orderid_array):
    return np.nansum(orderid_array, axis=1).astype(int)


def extract_rfm_data(df):
    last_txn_df = last_txn_date(df)
    spending_amt_df = amt_of_spending(df)
    if 'Gender' in df:
        gender_df = df[['MemberID','Gender']].drop_duplicates()
        return pd.merge(pd.merge(last_txn_df, spending_amt_df, on='MemberID'),
                        gender_df, how='left', on='MemberID').drop_duplicates()
    else:
        return pd.merge(last_txn_df, spending_amt_df, on='MemberID')


def compute_rfm_stat(extract, vectorize=False):
    if vectorize:
        return vec_compute_rfm_stat(extract)
    else:
        recency_stat = last_txn(extract)
        freqency_stat = tot_txn(extract)
        mean_monetary_stat = avg_spending(extract)
        total_monetray_stat = tot_spending(extract)
        return pd.Series({'LastTxnDate': recency_stat, 
                          'NoOfTxn': int(freqency_stat), 
                          'MeanMoneyValue': mean_monetary_stat, 
                          'TotalSpending': total_monetray_stat})


def vec_compute_rfm_stat(df, key='MemberID'):
    recency_col_sel = list(set([col for col in df.columns if 'Timestamp' in col]))
    recency_array = vec_last_txn(df[recency_col_sel].to_numpy())
    
    frequency_col_sel = list(set([col for col in df.columns if 'OrderID' in col]))
    freqency_array = vec_tot_txn(df[frequency_col_sel].to_numpy())
    
    monetary_col_sel = list(set([col for col in df.columns if 'Total' in col]))
    total_monetray_array = vec_tot_spending(df[monetary_col_sel].to_numpy())
    mean_monetary_array = vec_avg_spending(total_monetray_array, freqency_array)
    
    df['LastTxnDate'] = recency_array
    df['NoOfTxn'] = freqency_array
    df['MeanMoneyValue'] = mean_monetary_array
    df['TotalSpending'] = total_monetray_array
    return df[['LastTxnDate', 'MeanMoneyValue', 'NoOfTxn', 'TotalSpending']]


def mapper(path, bu, ddict, func, attributes=None, period=None, date_format=None):
    df = read_multiple_csv(path, ddict, bu)
    datetime_col = 'Timestamp'
    df[datetime_col] = pd.to_datetime(df[datetime_col], format=date_format)
    if not(period is None or len(period) == 0):
        if period[0] is not None:
            begin = pd.to_datetime(period[0])
            selected_period = (df[datetime_col] >= begin)
            if period[1] is not None:
                end = pd.to_datetime(period[1])
                selected_period = selected_period & (df[datetime_col] <= end)
        else:
            if period[1] is not None:
                end = pd.to_datetime(period[1])
                selected_period = (df[datetime_col] <= end)
        if np.any(selected_period):
            df = df.loc[selected_period]
        else:
            return None
    if func is None:
        return df[attributes]
    else:
        func_copy = copy.deepcopy(func)
        func_keys = [func_copy.keywords[key]
                     for key in func_copy.keywords.keys() if key == "keys"]
        is_nested_list = [isinstance(i, list) for i in func_keys]
        if any(is_nested_list):
            func_keys.extend(func_keys[is_nested_list.index(True)])
            del func_keys[is_nested_list.index(True)]
            func_keys = [k.lower() for k in func_keys]
        else:
            func_keys = [k.lower() for k in func_keys]
        if "date" in func_keys:
            df["Date"] = df[datetime_col].dt.date
        elif "week" in func_keys:
            df["Year"] = df[datetime_col].dt.year
            df["Month"] = df[datetime_col].dt.month
            df["Date"] = df[datetime_col].dt.date
            df["Week"] = df[datetime_col].dt.week
            df["Year"] = np.where((df["Month"] == 1) & (df["Week"] >= 52),
                                  df["Year"] - 1, df["Year"])
            df["Year"] = np.where((df["Month"] == 12) & (df["Week"] == 1),
                                  df["Year"] + 1, df["Year"])
            df["Year-Week"] = df["Year"].astype(str) + "-" + df["Week"].astype(str)
            if isinstance(func_copy.keywords["keys"], list):
                func_copy.keywords["keys"][func_keys.index("week")] = "Year-Week"
            else:
                func_copy.keywords["keys"] = "Year-Week"
        elif "month" in func_keys:
            df["Year"] = df[datetime_col].dt.year
            df["Month"] = df[datetime_col].dt.month
            df["Date"] = df[datetime_col].dt.date
            df["Year-Month"] = df["Year"].astype(str) + "-" + df["Month"].astype(str)
            if isinstance(func_copy.keywords["keys"], list):
                func_copy.keywords["keys"][func_keys.index("month")] = "Year-Month"
            else:
                func_copy.keywords["keys"] = "Year-Month"
        return func_copy(df)


def reducer(df, func, keys, vectorize=False):
    if vectorize:
        output = func(df, vectorize=True)
        if isinstance(output, pd.core.frame.DataFrame):
            return output.reset_index()
        else:
            df['value'] = output
            return df['value'].reset_index()
    else:
        if isinstance(df, list):
            df_merged = reduce(lambda df1, df2: df1.merge(df2, "outer", keys), df)
            df_merged = df_merged.set_index(keys)
            return df_merged.apply(func, axis=1)
        else:
            if len(keys) == 1:
                cols = [df.index.name]
            else:
                cols = [name for name in df.index.names]
            cols_out = [cols[i] for i in range(0,len(cols)) if cols[i] in keys]
            result = pd.DataFrame()
            if len(keys) == 1:
                for index, row in df.iterrows():
                    data = {}
                    for key in cols_out:
                        data.update({key: index})
                    data.update({'value': func(row)})
                    res_tmp = pd.DataFrame(data, cols_out)
                    result = result.append(res_tmp)
            else:
                # TODO: further optimize the loop by vectorization
                output = df.apply(func, axis=1).reset_index()
                if output.shape[1] == 2:
                    result = output.rename(columns={0: 'value'})
                else:
                    result = output
            return result


def chunk(x):
    if isinstance(x, pd.core.frame.DataFrame):
        num_processes =  mp.cpu_count()
        chunk_size = int(x.shape[0]/num_processes)
        return [x.loc[x.index[i:i + chunk_size]] for i in range(0, x.shape[0], chunk_size)]
    elif isinstance(x, list):
        num_processes =  mp.cpu_count()
        chunk_size = int(len(x)/num_processes)
        return [x[i:i+chunk_size] for i in range(0, len(x), chunk_size)]


def map_reduce(mapper, reducer, files, keys, out_func=None, 
               multi_process=True):
    map_results = []
    if multi_process:
        if len(files) > mp.cpu_count():
            files_chunk = chunk(files)
            with mp.Pool(processes=min([mp.cpu_count(), len(files_chunk)])) as pool:
                map_results = pool.map(mapper, files_chunk)
        else:
            with mp.Pool(processes=min([mp.cpu_count(), len(files)])) as pool:
                map_results = pool.map(mapper, files)
        pool.close()
    else:
        if len(files) > mp.cpu_count():
            files_chunk = chunk(files)
        for file in files_chunk:
            map_results.append(mapper(path=file))
    map_results = [res for res in map_results if not(res is None)]
    if reducer is None:
        return pd.concat(map_results)
    else:
        if 'Gender' in map_results[0]:  # TODO: not to include this check in this part
            df_gender = pd.concat([res[[keys, 'Gender']] for res in map_results])
            df_gender['Gender'] = pd.to_numeric(df_gender['Gender'])
            df_gender['Gender'] = df_gender['Gender'].fillna(0).astype(int)
            df_gender.drop_duplicates(inplace=True)
            # The following line set the member with two genders as NA
            df_gender.loc[df_gender.duplicated(keys, False), 'Gender'] = 0
            df_gender.drop_duplicates(inplace=True)
            df_gender = df_gender.reset_index(drop=True)
            [res.drop(columns='Gender', inplace=True) for res in map_results]
        df_merged = reduce(lambda df1, df2: df1.merge(df2, "outer", keys), map_results)
        df_merged = df_merged.set_index(keys)
        chunks = chunk(df_merged)
        results = []
        if multi_process:
            with mp.Pool(processes=min([mp.cpu_count(), len(chunks)])) as pool:
                results = pool.map(partial(reducer, keys=keys), chunks)
            pool.close()
        else:        
            for chunked in chunks:
                results.append(reducer(chunked, keys=keys))
        if out_func is None:
            df_output = pd.concat(results)
        else:
            df_output = out_func(pd.concat(results))
            
        if 'df_gender' in locals():
            df_output = pd.merge(df_output, df_gender, on=keys)
            df_output.drop_duplicates(inplace=True)
        return df_output
