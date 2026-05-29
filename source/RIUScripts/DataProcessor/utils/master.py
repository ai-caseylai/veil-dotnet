#!/usr/bin/env python3
# Utils file for all BU


def rename_data(df, ddict):
    return df.rename(columns=ddict['rename'])