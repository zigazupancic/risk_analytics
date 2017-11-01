import requests
import requests_cache

import json

import pandas as pd

# Set path of file for caching API calls
requests_cache.install_cache('../bci')

def get_blockchain(max_height, min_height=0, 
                   base_url = "https://blockchain.info/",
                   timeout = 30, payload=None, verbose=False):
    """
    Query blockchain.info API and return list of block JSONs between (inclusive)
    min and max heights
    """
    block_list = []
    for height in range(min_height, max_height+1):
        if verbose and height % 500 == 0:
            print("Reading block {0}".format(height))
        resource = 'block-height/{0}?format=json'.format(height)
        response_content = requests.get(base_url + resource, payload, timeout=timeout).content
        
        block_list.append(json.loads(response_content)['blocks'])
    
    # Flatten the list
    block_list_flat = [item for sublist in block_list for item in sublist]
    
    return block_list_flat


def blocks2df(block_list, cols = None):
    """
    Parse the flat list of blocks to a dataframe, with one row per transaction
    """
    tx_in_list = []
    tx_out_list = []
    tx_in_dfs = []
    tx_out_dfs = []
    #for block in [block_list_flat[multi] for multi in mts]:
    for block in block_list:
        height = block['height']
        for i_tx in range(block['n_tx']):
            tx_hash = block['tx'][i_tx]['hash']
            for in_dict in block['tx'][i_tx]['inputs']:

                if 'prev_out' in in_dict.keys():
                    prev_out_dict = {}
                    for key, val in in_dict['prev_out'].items():
                        prev_out_dict['prev_' + key] = val
                    prev_out_dict = {**{'height': height, 'tx_hash': tx_hash}, **prev_out_dict}
                    tx_in_list.append(prev_out_dict)

                else:
                    tx_in_dict = {**{'height': height, 'tx_hash': tx_hash}, **in_dict}
                    tx_in_list.append(tx_in_dict)

            for out_dict in block['tx'][i_tx]['out']:
                tx_out_dicts = {**{'height': height, 'tx_hash': tx_hash}, **out_dict}
                tx_out_list.append(tx_out_dicts)
    tx_in_df = pd.DataFrame(tx_in_list)
    tx_out_df = pd.DataFrame(tx_out_list)
    
    block_df = pd.merge(tx_out_df, tx_in_df, on = ['tx_hash', 'height'], how = 'outer')
    if cols is None:
        return block_df
    else:
        return block_df[[c for c, _ in cols]].rename(columns = dict(cols))
        