-module(etwit_api).

-include_lib("alog/include/alog.hrl").
-include("etwit.hrl").

-export([follow_key/3, get_followers/1]).

-define(TWIT_SEARCH_URL, "http://search.twitter.com/search.json").
-define(TWIT_FOLLOW_URL, "http://api.twitter.com/1/friendships/create.json").
-define(TWIT_FOLLOWERS_URL, "http://api.twitter.com/1/followers/ids.json").


-define(MAX_RESULTS_PER_QUERY, 1500).
-define(MAX_RESULTS_PER_PAGE, 100).

%% unpack oauth_data record
-define(CONSUMER(OD), {OD#oauth_data.consumer_key, OD#oauth_data.consumer_secret, hmac_sha1}).
-define(TOKEN(OD), OD#oauth_data.token).
-define(SECRET(OD), OD#oauth_data.secret).


%%
%% Search for Count twits by Key,
%%   make list of unique users and follow
%%   them all.
%% Returns: 'ok' on success, 'fail' on error
follow_key(Key, Count, OauthData) ->
    {ok, Users} = search(Key, Count),
    UniqueUsers = uniquify_users(Users),
    ?INFO("Going to follow ~p unique users by key: ~p", [length(UniqueUsers), Key]),
    case follow_users(UniqueUsers, OauthData) of
        ok ->
            %% all users followed, nice
            ?INFO("Total ~p users followed by key: ~p", [length(UniqueUsers), Key]),
            ok;
        {fail, SuccCount, FailCount} ->
            ?WARNING("Could not follow all users. Only ~p success (~p fails)", [SuccCount, FailCount]),
            fail
    end.

%%
%% Search for a Query using twitter API
%%
%% returns: {ok, Results} | {error, Reson}
%% where Result is a list of parsed JSON entries
%%
search(Query, Count) when Count < ?MAX_RESULTS_PER_QUERY ->
    search(Query, Count, Count, []).


search(_Query, 0, _Count, Results) ->
    {ok, Results};

search(Query, CountLeft, Count, Results) ->
    {ok, FullUrl, NCountLeft} = compose_url(Query, CountLeft, Count),
    case find_twits(FullUrl) of
        {ok, AddRes} ->
            search(Query, NCountLeft, Count, lists:append(AddRes, Results));
        {error, Reason} ->
            {error, Reason}
    end.

%%
%% compose URL according to needed results count
%% using twitter paging
%%
compose_url(Query, CountLeft, Count) ->
    Page = (Count - CountLeft) / ?MAX_RESULTS_PER_PAGE,
    Rpp = if
              CountLeft > ?MAX_RESULTS_PER_PAGE ->
                  ?MAX_RESULTS_PER_PAGE;
              CountLeft =< ?MAX_RESULTS_PER_PAGE ->
                  CountLeft
          end,
    Url = compose_get_params(?TWIT_SEARCH_URL, [{"q", Query},
                                                {"rpp", integer_to_list(Rpp)},
                                                {"page", integer_to_list(round(Page)+1)}]),
    {ok, Url, CountLeft - Rpp}.


%%
%% Compose GET request URL from base Url and proplist in form: [{ParamName, ParamValue}]
%%
compose_get_params(Url, Params) when is_list(Params) ->
    GetParams=string:join([P ++ "=" ++ V || {P, V} <- Params], "&"),
    lists:flatten([Url, "?", GetParams]).


%%
%% Actually get URL, parse results and return list of 'twitter_user' records.
%%
find_twits(Url) ->
    ?INFO("Retrieving users by url: ~p", [Url]),
    case httpc:request(get, {Url, []}, [], [{full_result, false}]) of
        {ok, {200, Body}} ->
            %% status 200, proceed
            parse_twits_json(Body);
        {ok, {Code, _Body}} ->
            %% not a 200 status, log and return error
            ?ERROR("Receiving non-OK (~p) from upstream by request: ~p", [Code, Url]),
            {error, bad_http_code};
        {error, Reason} ->
            %% client error (like connection refused)
            ?ERROR("Receiving error from HTTP client: ~p", [Reason]),
            {error, http_client}
    end.

parse_twits_json(Str) ->
    {ok, {obj, JsonResult}, _} = rfc4627:decode(Str),
    unpack_twits_json_result(JsonResult).


%%
%% Repack from ubgly structure returned by rfc4627 parser to list of nice
%% 'twiter_us' resords
%%
unpack_twits_json_result(JsonRes) ->
    case proplists:get_value("results", JsonRes) of
        undefined ->
            ?ERROR("JSON structure changed. Could not extract users list."),
            { error, json_format};
        Results ->
            {ok, [proplist_to_twitter_user(Props) || {obj, Props} <- Results]}
    end.


%% extracts fields from proplist to record twit
proplist_to_twitter_user(P) ->
    #twitter_user{ user = proplists:get_value("from_user", P),
        user_id = proplists:get_value("from_user_id", P),
        user_name = proplists:get_value("from_user_name", P) }.


%% uniquify users list. Sort, traverse, add only one from all doubles entities
uniquify_users(List) ->
    SortedList = lists:sort(fun(E1, E2) -> E1#twitter_user.user_id =< E2#twitter_user.user_id end, List),
    uniquify_users(SortedList, [], undefined).

%% start condition
uniquify_users([H | T], Acc, undefined) ->
    uniquify_users(T, [H|Acc], H);

%% stop condition
uniquify_users([], Acc, _) ->
    Acc;

uniquify_users([H | T], Acc, Prev) ->
    if
        H#twitter_user.user_id == Prev#twitter_user.user_id ->
            uniquify_users(T, Acc, Prev);
        true ->
            uniquify_users(T, [H | Acc], H)
    end.


%% follows all users in the list
%% to report.
follow_users(List, OauthData) ->
    [ok = follow_user(U, OauthData) || U <- List],
    ok.

follow_user(User, OD) ->
    ?INFO("followng twit: ~p", [User]),
    case oauth:post(?TWIT_FOLLOW_URL, [{"user_id", integer_to_list(User#twitter_user.user_id)}, {"follow", "true"}],
                    ?CONSUMER(OD), ?TOKEN(OD), ?SECRET(OD)) of
        {ok,{{_HTTP, 200, _Explain}, _Headers, _Err}} ->
            ok;
        {ok,{{_HTTP, HTTPCode, _Explain}, _Headers, Err}} ->
            ?ERROR("Twitter API answer with code: ~p, err: ~p", [HTTPCode, Err]),
            fail
    end.


%%
%% Gets the list of users following UserName, recursive
%%
get_followers(UserName) ->
    {NextCursor, Ids} = request_one_followers_page(UserName, "-1"),
    get_followers(UserName, NextCursor, Ids).

%% stop condtion
get_followers(_UserName, 0, Acc) ->
    {ok, Acc};

%% iterate
get_followers(UserName, Cursor, Acc) ->
    {NextCursor, AddIds} = request_one_followers_page(UserName, Cursor),
    get_followers(UserName, NextCursor, lists:append(Acc, AddIds)).

request_one_followers_page(UserName, Cursor) ->
    Url = compose_get_params(?TWIT_FOLLOWERS_URL, [{"screen_name", UserName}, {"cursor", Cursor}]),
    case httpc:request(get, {Url, []}, [], [{full_result, false}]) of
        {ok, {200, Body}} ->
            parse_followers_json(Body);
        Other ->
            ?ERROR("Could not retrieve followers, cursor: ~p, result: ~p", [Cursor, Other]),
            fail
    end.

%%
%% parse json returned by twitter API call GET followers/ids
%% returns {Nextcursor, ListIds}
%%
parse_followers_json(Str) ->
    {ok, {obj, JsonResult}, _} = rfc4627:decode(Str),
    NextCursor = proplists:get_value("next_cursor_str", JsonResult),
    Ids = proplists:get_value("ids", JsonResult),
    {NextCursor, Ids}.



