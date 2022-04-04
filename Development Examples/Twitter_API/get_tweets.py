import tweepy
import twitter_credentials
import time
import datetime
import pandas as pd
import pathlib


def authenticate():
    auth = tweepy.OAuthHandler(consumer_key=twitter_credentials.con_key,
                               consumer_secret=twitter_credentials.con_secret)
    auth.set_access_token(twitter_credentials.acc_token,
                          twitter_credentials.acc_secret)

    return auth


def get_tweets(t_api, the_date: str, last_id: int = -1):

    # The tweet search rate limit is 180 calls in 15 minutes.
    # 900 seconds/180 calls = 5 sec/call.
    wait_time = 5.0
    num = 1000
    test_date = datetime.date.fromisoformat(the_date)
    one_day = datetime.timedelta(days=1)
    next_day = test_date + one_day
    tweet_list = []

    while num > len(tweet_list):
        try:
            new_tweets = t_api.search(q='"Merry Christmas" OR "Happy Christmas" OR "Merry Xmas" OR "Happy Xmas" AND -filter:retweets AND -filter:replies',
                                      tweet_mode='extended', count=200, lang='en', max_id=str(last_id - 1), since=the_date, until=str(next_day))
        except tweepy.TweepError as e:
            print("Error", e)
            break
        else:
            if not new_tweets:
                print("Could not find any more tweets!")
                break
            else:
                last_id = new_tweets[-1].id
                tweet_list.extend(new_tweets)
        time.sleep(wait_time)

    return (tweet_list)


def count_christmas(tweet_text: str):
    """
    Accepts the text of a single tweet.
    Determines if specified strings exist
    Returns a 2-tuple of the results of
    happy vs merry.
    """

    merry = False
    happy = False
    tweet_text = tweet_text.upper()
    merry_list = ["MERRY CHRISTMAS", "MERRY XMAS",
                  "MERRY-CHRISTMAS", "MERRY-XMAS"]
    happy_list = ["HAPPY CHRISTMAS", "HAPPY XMAS",
                  "HAPPY-CHRISTMAS", "HAPPY-XMAS"]
    for greet in merry_list:
        if greet in tweet_text:
            merry = True
            break

    for greet in happy_list:
        if greet in tweet_text:
            happy = True
            break

    return (merry, happy)


def get_data(tweet_list):
    """
    Accepts a list of tweets.
    Extracts ID, Author, Date, Location,
    and whether tweet contains happy or merry

    Returns pandas dataframe

    DEPENDENCIES: pandas
    """
    t_id = []
    t_author = []
    t_date = []
    t_happy = []
    t_merry = []
    t_all = [t_id, t_author, t_date, t_merry, t_happy]
    labels = ['id', 'author', 'date', 'merry', 'happy']

    for tweet in tweet_list:
        this_id = '"' + str(tweet.id) + '"'
        t_id.append(this_id)
        t_author.append(tweet.user.screen_name)
        t_date.append(tweet.created_at)

        # Search also searches url text. Add all url text to tweet text
        urls_list = tweet._json["entities"]["urls"]
        all_urls = ""
        for each_dict in urls_list:
            all_urls = all_urls + " " + each_dict["expanded_url"]

        full_text = tweet.full_text + all_urls
        merry_happy = count_christmas(full_text)
        t_merry.append(merry_happy[0])
        t_happy.append(merry_happy[1])

    d_tweets = {}
    for num, item in enumerate(labels):
        d_tweets[item] = t_all[num]

    return pd.DataFrame(d_tweets)


def save_tweets(filename: str, tweet_df):
    """
    Accepts a filename and a pandas dataframe
    Appends the output to the specified CSV file.

    Returns boolean indicating success
    DEPENDENCIES: pathlib
    """
    b_saved = True
    fp = pathlib.Path(filename)
    use_header = not(fp.exists())

    try:
        tweet_df.to_csv(filename, index=False, header=use_header, mode="a")
    except IOError as e:
        b_saved = False
        print(f"I/O error({0}): {1}".format(e.errno, e.strerror))
    else:
        b_saved = True

    return b_saved


def get_state(filename: str):
    """
    Accepts a filename
    Reads the date and id strings from the text file.

    Returns boolean indicating success
    """

    try:
        with open(filename, "r") as state_file:
            date = state_file.readline().strip()
            max_id = state_file.readline().strip()
            return(date, max_id)
    except IOError as e:
        print(f"I/O error({0}): {1}".format(e.errno, e.strerror))

    return("", "")


def save_state(filename: str, datestring: str, idstring: str):
    """
    Accepts a filename, date string, and id string.
    Writes the date string and id string to the file as text.

    Returns boolean indicating success
    """
    b_saved = True
    try:
        with open(filename, "w") as state_file:
            datestring += "\n"
            idstring += "\n"
            state_file.write(datestring)
            state_file.write(idstring)
    except IOError as e:
        print(f"I/O error({0}): {1}".format(e.errno, e.strerror))
        b_saved = False

    return b_saved


state_file = "statefile.txt"
tweet_csv_file = "tweet_summary.csv"
start_date = ""
start_id = "-1"
day = 1
gathering = True

p = pathlib.Path(state_file)
if p.exists():
    start_date, start_id = get_state(state_file)
    day = int(start_date.split("-")[2])

twitter_auth = authenticate()
twitter_api = tweepy.API(twitter_auth)

limit = 20
for today in range(day, 8):
    print(f"Day #{today}:")
    the_date = "2019-12-0" + str(today).strip()
    gathering = True
    stop = 0

    while gathering:
        next_tweets = get_tweets(twitter_api, the_date, int(start_id))
        stop += 1
        if(len(next_tweets) > 0) and (stop < limit):
            start_id = str(next_tweets[-1].id)
            print(
                f"Next {len(next_tweets)} date: {next_tweets[-1].created_at} id: {start_id}")
            tweet_df = get_data(next_tweets)
            saved = save_tweets(tweet_csv_file, tweet_df)
            if saved:
                state_saved = save_state(
                    state_file, the_date, str(next_tweets[-1].id))
        else:  # len = 0 or we hit our emergency stop limit.
            gathering = False
            start_id = -1
