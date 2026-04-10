import os
import json
import requests

import pandas as pd
import matplotlib.pyplot as plt

from pprint import pprint
#from scrapy import Selector
from tqdm.notebook import tqdm



#LOAD STOPWORDS JSON
file_path_stopwords = '../data/stopwords.json'
with open(file_path_stopwords, 'r') as file:
    stopwords = json.load(file)


def scrape_all_subreddits(list):
    all_subreddits_data = pd.DataFrame()
    for subreddit in list:
        subreddit_data = scrape_subreddit(subreddit)
        all_subreddits_data = pd.concat([all_subreddits_data, subreddit_data], ignore_index=True)
    return all_subreddits_data
    

subreddits = ['newyork', 'Maine', 'newhampshire','vermont', 'massachusetts', 'RhodeIsland', 'Connecticut', 'newjersey', 
              'Delaware', 'maryland','washingtondc','WestVirginia', 'Pennsylvania', 'Virginia', 'NorthCarolina', 
              'southcarolina', 'Georgia', 'florida', 'Alabama', 'Tennessee', 'mississippi', 'Kentucky', 'Ohio', 
              'Indiana' , 'Michigan', 'Louisiana', 'Arkansas', 'missouri', 'illinois', 'Iowa', 'wisconsin', 'minnesota',
              'texas', 'oklahoma', 'kansas', 'Nebraska','SouthDakota', 'northdakota', 'NewMexico','Colorado', 'wyoming',
              'Montana', 'arizona', 'Utah', 'Idaho', 'Nevada', 'California', 'oregon', 'Washington','alaska', 'Hawaii',
              'democrats', 'Republican', 'politics' ]


def scrape_subreddit(subreddit):
    '''
    returns json of a singular subreddit
    '''
    credentials_file_path = "../credentials.json"
    # Load API creds
    with open(credentials_file_path, "r") as f:
        credentials = json.load(f)
        # Reddit API Doc Code
    client_auth = requests.auth.HTTPBasicAuth(credentials["app_id"], credentials["app_secret"])
    post_data = {"grant_type": "password", "username": credentials["username"], "password": credentials["password"]}
    headers = {"User-Agent": f"LSE student data science project {credentials['username']}"}
        #Load access token
    ACCESS_TOKEN_ENDPOINT = "https://www.reddit.com/api/v1/access_token"
    response = requests.post(ACCESS_TOKEN_ENDPOINT, auth=client_auth, data=post_data, headers=headers)
    my_token = response.json()['access_token']
    headers = {"Authorization": f"bearer {my_token}", 
        "User-Agent": f"LSE student data science project {credentials['username']}"}
    after_id = None
    all_data = pd.DataFrame()
    for _ in range(5):
        params = {'limit': '100', 't': 'year'} #'t': hour,day,year
        if after_id is not None:
            params['after'] = after_id

        data = requests.get(f"https://oauth.reddit.com/r/{subreddit}/top",headers = headers, params=params).json()                                                                         
        df_page = pd.DataFrame.from_dict(data['data']['children'])
        df_page = pd.json_normalize(df_page['data'], max_level = 1)
        
        df_page = df_page[['subreddit','title', 'selftext','created_utc','id', 'upvote_ratio', 'ups','num_comments',]].copy()
        after_id = f"{data['data']['after']}"
        print(data['data']['after'])

        all_data = pd.concat([all_data, df_page], ignore_index=True)
        if data['data']['after'] == None:
            break
    
    df_clean = all_data.drop_duplicates(subset=['id'])
    
    return df_clean



def clean_text(data,subreddit):
    title_list = [x for x in data[data['subreddit']== subreddit]['title']]
    text_list = [x for x in data[data['subreddit']== subreddit]['selftext']]
    all_text = title_list + text_list
    filtered_text = list(filter(lambda x: x.strip(), all_text))
    clean_text = [" ".join([w for w in t.split() if not w in stopwords['stopwords']]) for t in filtered_text]
    return clean_text

def list_join(list):
    '''
    turns list in large string of all elements in list
    - used for comparing total content of subreddit to another
    '''
    list_join = ''
    for i in list:
        list_join += f'{i}'
    return list_join
        









