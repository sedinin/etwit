-ifndef(etwit_data).
-define(etwit_data, true).

-define(TWIT_SERVER, etwit_server).

%% information about twitter user (only needed)
-record(twitter_user, {user, user_id, user_name}).

%% oauth configuration
-record(oauth_data, {consumer_key, consumer_secret, token, secret}).

-endif. % -ifndef(etwit_data)
