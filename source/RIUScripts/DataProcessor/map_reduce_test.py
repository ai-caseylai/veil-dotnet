import glob
import map_reduce as mr
import os
import pandas as pd
import numpy as np

from functools import partial
from os import listdir
from os.path import isfile, join


if __name__ == "__main__":
    bu = 106
    data_path = '../Data/Bauhaus/txn/'
    ddict = mr.read_ddict(os.path.join(data_path + 'ddict.json'))
    datetime_col = 'Timestamp'
    date_format = ddict['format'][datetime_col]
    files = [f for f in listdir(data_path) if isfile(join(data_path, f)) and
             '.csv.xz' in f and not (any([s in f for s in ['.tmp', '.bak', 'old-']]))]

    # # Test concat with attributes TxnNo, Timestamp, MemberID, Qty, NetPrice, ProductID
    # mapper_tmp = partial(mr.mapper, bu=bu,
    #                      ddict=ddict, func=None,
    #                      attributes=['OrderID', 'Timestamp', 'MemberID',
    #                                  'Quantity', 'NetPrice', 'ProductID'],
    #                      period=['20150101', '20171031'],
    #                      date_format=date_format)
    # reducer_tmp = None
    # keys_combine = None
    # func_combine = None

    # # Test cum qty of department by MemberID within period
    # mapper_tmp = partial(mr.mapper, bu=bu, ddict=ddict,
    #                      func=partial(mr.item_qty, keys='MemberID',
    #                                   agg_type=3),
    #                      period=['20150101', '20171031'],
    #                      date_format=date_format)
    # reducer_tmp = partial(mr.reducer, func=mr.cum_qty,
    #                       vectorize=True)
    # keys_combine = ['MemberID', 'Item']
    # func_combine = None

    # # Test number of transactions by MemberID within period
    # mapper_tmp = partial(mr.mapper, bu=bu, ddict=ddict,
    #                      func=partial(mr.no_of_txn, keys='MemberID'),
    #                      period=['20150101', '20171031'],
    #                      date_format=date_format)
    # reducer_tmp = partial(mr.reducer, func=np.sum)
    # keys_combine = 'MemberID'
    # func_combine = None

    # # Test number of transactions by Date within period
    # mapper_tmp = partial(mr.mapper, bu=bu, ddict=ddict,
    #                      func=partial(mr.no_of_txn, keys='Date'),
    #                      period=['20170603', '20170630'],
    #                      date_format=date_format)
    # reducer_tmp = partial(mr.reducer, func=np.sum)
    # keys_combine = 'Date'
    # func_combine = None

    # Test number of transactions by Week within period
    # mapper_tmp = partial(mr.mapper, bu=bu, ddict=ddict,
    #                      func=partial(mr.no_of_txn, keys='Week'),
    #                      period=['20170603', '20170830'],
    #                      date_format=date_format)
    # reducer_tmp = partial(mr.reducer, func=mr.tot_txn,
    #                       vectorize=True)
    # keys_combine = 'Year-Week'
    # func_combine = None

    # # Test number of transactions by Month within period
    # mapper_tmp = partial(mr.mapper, bu=bu, ddict=ddict,
    #                      func=partial(mr.no_of_txn, keys='Month'),
    #                      period=['20170603', '20180517'],
    #                      date_format=date_format)
    # reducer_tmp = partial(mr.reducer, func=mr.tot_txn,
    #                       vectorize=True)
    # keys_combine = 'Year-Month'
    # func_combine = None

    # # Test number of transactions by Date
    # mapper_tmp = partial(mr.mapper, bu=bu, ddict=ddict,
    #                      func=partial(mr.no_of_txn, keys='Date'),
    #                      date_format=date_format)
    # reducer_tmp = partial(mr.reducer, func=mr.tot_txn,
    #                       vectorize=True)
    # keys_combine = 'Date'
    # func_combine = None

    # # Test number of transactions by Month in list
    # mapper_tmp = partial(mr.mapper, bu=bu, ddict=ddict,
    #                      func=partial(mr.no_of_txn, keys=['Month']),
    #                      date_format=date_format)
    # reducer_tmp = partial(mr.reducer, func=mr.tot_txn,
    #                       vectorize=True)
    # keys_combine = ['Year-Month']
    # func_combine = None

    # # Test number of transactions by Month and StoreID
    mapper_tmp = partial(mr.mapper, bu=bu, ddict=ddict,
                         func=partial(mr.no_of_txn, keys=['Month', 'StoreID']),
                         date_format=date_format)
    reducer_tmp = partial(mr.reducer, func=mr.tot_txn,
                          vectorize=True)
    keys_combine = ['Year-Month', 'StoreID']
    func_combine = None

    result = mr.map_reduce(mapper_tmp, reducer_tmp,
                           [data_path + f for f in files] + glob.glob(
                               data_path + "*/*.csv.xz"), keys_combine,
                           func_combine, multi_process=False)
