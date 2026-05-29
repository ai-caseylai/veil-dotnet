# Data processor

This part contians the scripts for Data processing.

## Prerequisites
* python (>= 3.6.4)
* argparse (>= 1.1)
* json (>= 2.0.9)
* numpy (>= 1.14.0)
* pandas (>= 0.22.0)
* tornado (>= 4.5.3)

## List of scripts
* [[map_reduce.py](./map_reduce.py)] - Script to carry out split-apply-combine for transaction data.
* [[map_reduce-driver.py](./map_reduce-driver.py)] - Script to host the data processing service using tornado.
* [[index1.html](./index1.html)] - Index page containing examples for the data processing service.
* [[utils/master.py](./utils/master.py)] - Script for data processing shared over all BU.
* [[utils/sevenFans.py](./utils/sevenFans.py)] - Script for data processing for 7Fans.
* [[utils/bauhaus.py](./utils/bauhaus.py)] - Script for data processing for Bauhaus.

## TODO List

1. Add comments.
2. Remove unused comments.
3. Improve the scripts.
4. etc.
