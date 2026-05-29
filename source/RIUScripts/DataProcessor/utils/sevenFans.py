#!/usr/bin/env python3
# Utils file for 7fans


def clean_data(df):
    # Drop the 7-11 staff accounts (CardNumber begin with 9).
    df = df[(df['CardNumber'].str[0] != '9') &
              (df['CardNumber'].str.lower() != 'null') &
              (df['MemberID'] != 0) &
              (df['DepartCode'].str[0:3] != 'SER') &
              (df['NetPrice'] > 0)]
    
    # Remove the items contain keywords: $ SAVE, SAVE $, CPN,COUPON, BOTTLE RETURN, 
    # bottle refund 7, DIS CARD 8, NEWSPAPER OFF 9, #TAKE-OUT
    product_name_regexp = (r"(\$).*(SAVE)|(SAVE).*(\$)|"
                           r"(CPN)|(COUPON)|(BOTTLE).*(RETURN)|"
                           r"(bottle).*(refund)|(DIS CARD)|"
                           r"(NEWSPAPER).*(OFF)|(#TAKE-OUT)")
    df = df[~(df['ProdName1'].str.contains(product_name_regexp))]
    return df
