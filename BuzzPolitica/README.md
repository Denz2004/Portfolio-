# Overview of Buzz Politica Git Repo
- Website Qmds
- Code Analysis
- Data
- Docs
- Pictures

# INSTRUCTIONS FOR REPLICATION
1- obtain a reddit API to pull info from 500 posts from each state subreddit and configure API access - this can be seen in the nb01 reddit file  
2- Next, fetch the data and apply bertopic model t to each state in order to surmise topics from the posts (bert.ipynb)  
3- Graphs are produced in order to investigate the nature of these topics, as well as interstate topic similarity  
4- Finally, a interactive map (map.html) is produced using this data - the map consists of each state and its top post, associated upvote rating and number of comments, and top 3 topics
# running order
nb01 reddit + functions.py --> bert.ipynb

# Requirements

Requests
Pandas
matplotlib
tqdm
ipython-sql
mysql-connector-python
tensorflow
Flask
Bertopic
Dropbox
plotly
itables
