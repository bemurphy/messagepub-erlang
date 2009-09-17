%% TODO - parse xml reply bodies out into records, or, perhaps leave up to caller?  Then
%% they can parse as they see fit
%% TODO - consider replacing building the position incrementally via append, and allow it
%% to be explicity set

-module(messagepub).
-author("luc.castera@gmail.com").
-behaviour(gen_server).

-include("messagepub.hrl").

-define(UA, "erlang-messagepub").
-define(BASE_URL, "http://messagepub.com/").

-export([send/3, create/1, view/0, get_notification/1, cancel/1, replies/0]).
-export([start_link/1, stop/0]).
-export([new_notification/2, new_notification/3, make_recipient/2]).
-export([twitter_recipient/1, gchat_recipient/1, aim_recipient/1, email_recipient/1, sms_recipient/1, phone_recipient/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% PUBLIC API

%% Build helpers, record builders, etc
new_notification(Body, Recipients) ->
    new_notification(Body, 10, Recipients).
new_notification(Body, Escalation, Recipients) ->
    #notification{body=Body, escalation=Escalation, recipients=Recipients}.

%% Helpers for recipient formatting.  A caller can just call make_recipient and provide channel
%% if they like.  Not totally sold on using records for recipients, vs. plain old tuples.
make_recipient(Channel, Address) ->
    #recipient{channel=Channel, address=Address}.

%% TODO: do verifications on address format?
email_recipient(Email) ->
    make_recipient("email", Email).

twitter_recipient(Tweeter) ->
    make_recipient("twitter", Tweeter).

gchat_recipient(GJid) ->
    make_recipient("gchat", GJid).

aim_recipient(ScreenName) ->
    make_recipient("aim", ScreenName).

sms_recipient(MobileNumber) ->
    make_recipient("sms", MobileNumber).

phone_recipient(PhoneNumber) ->
    make_recipient("phone", PhoneNumber).

%% Adding a send to match old api usage, for backwards compat
send(Channel, Address, Message) ->
    create(new_notification(Message,  [make_recipient(Channel, Address)])).

%% Create a new notification!
create(Notification) ->
    gen_server:call(?MODULE, {create, Notification}, 10000).

%% Grab xml for all notifications
view() ->
    gen_server:call(?MODULE, view, 10000).

%% Grab xml for a specific notification.  Will take numeric or string.
%% I wonder if this is too defensive?
get_notification(NotificationId) when is_number(NotificationId) ->
    get_notification(integer_to_list(NotificationId));
get_notification(NotificationId) ->
    gen_server:call(?MODULE, {get, NotificationId}, 10000).

%% Cancel pending channel recipients for a notification.
cancel(NotificationId) when is_number(NotificationId) ->
    cancel(integer_to_list(NotificationId));
cancel(NotificationId) ->
    gen_server:call(?MODULE, {cancel, NotificationId}, 10000).

%% Grab xml for all replies
replies() ->
    gen_server:call(?MODULE, replies, 10000).

%% Public process control
start_link(ApiKey) ->
    io:format("Entering messagepub start_link~n"),
    gen_server:start_link({local, ?MODULE}, ?MODULE, ApiKey, []).

stop() ->
    gen_server:cast(?MODULE, stop).

%% Private API
init(ApiKey) ->
    inets:start(),
    {ok, ApiKey}.

%% Format a recipient into the xml block expected by messagepub.  Use recursion
%% to build a list of incrementing positions
format_recipients(Recipients) ->
    format_recipients(Recipients, [], 1).
format_recipients([], XmlRecipients, _Position) ->
    "<recipients>" ++ XmlRecipients ++ "</recipients>";
format_recipients([Recipient|Rest], XmlRecipients, Position) ->
    XmlRecipient = "<recipient><position>" ++ integer_to_list(Position) ++ "</position>" ++
        "<channel>" ++ Recipient#recipient.channel ++ "</channel>" ++
        "<address>" ++ Recipient#recipient.address ++ "</address></recipient>",
    format_recipients(Rest, XmlRecipients ++ XmlRecipient, Position + 1).

%% remove newlines from a list
strip_newlines(Text) ->
    re:replace(Text, "\n", "", [global, {return, list}]).

%% Prefer is used to force extraction of info from Body rather than Msg
extract_response(Prefer, {Code, Msg, Body}, OkCode) when Prefer == body; Prefer == msg ->
    MoreInfo = case {Prefer, Code} of
                   {msg, OkCode} -> Msg;
                   {_, _}  -> strip_newlines(Body)
               end,
    Status = case Code of
                 OkCode -> ok;
                 _ ->   error
             end,
    {Status, Code, MoreInfo}.

basic_auth_string(ApiKey) ->
    "Basic " ++ base64:encode_to_string(ApiKey ++ ":").

%% Generate the request tuple for the web service
request(Url, ApiKey, Params) ->
    {RequestUrl, RequestHeaders} = {Url, [{"User-Agent", ?UA}, {"Content-Type", "text/xml"}, {"Authorization", basic_auth_string(ApiKey)}]},
    case length(Params) of
        0 ->
            {RequestUrl, RequestHeaders};
        _ ->
            {RequestUrl, RequestHeaders, "text/xml", Params}
    end.

%% Talk to the web service!
http_request_and_reply(Method, Url, {OkCode, Prefer}, ApiKey) ->
    http_request_and_reply(Method, Url, {OkCode, Prefer}, ApiKey, []).
http_request_and_reply(Method, Url, {OkCode, Prefer}, ApiKey, Params) ->
    Reply = case http:request(Method, request(Url, ApiKey, Params), [], []) of
        {error, Reason} ->
                    {error, Reason};
        {ok, {{_HttpVer, Code, Msg}, _Headers, Body}} ->
                    extract_response(Prefer, {Code, Msg, Body}, OkCode)
    end,
    {reply, Reply, ApiKey}.

%% OTP message callbacks...
%% creating a notification
handle_call({create, Notification}, _From, ApiKey) ->
    Url = ?BASE_URL ++ "notifications.xml",
    XmlRecipients = format_recipients(Notification#notification.recipients),
    BodyXML = "<notification><body>" ++ Notification#notification.body ++ "</body>" ++
        "<escalation>" ++ integer_to_list(Notification#notification.escalation) ++"</escalation>" ++
        XmlRecipients ++ "</notification>",
    http_request_and_reply(post, Url, {201, msg}, ApiKey, BodyXML);

%% Viewing notifications
handle_call(view, _From, ApiKey) ->
    Url = ?BASE_URL ++ "notifications.xml",
    http_request_and_reply(get, Url, {200, body}, ApiKey);

%% Get a specific notification
handle_call({get, NotificationId}, _From, ApiKey) ->
    Url = ?BASE_URL ++ "notifications/" ++ NotificationId ++ ".xml",
    http_request_and_reply(get, Url, {200, body}, ApiKey);

%% Cancel a notification
handle_call({cancel, NotificationId}, _From, ApiKey) ->
    Url = ?BASE_URL ++ "notifications/" ++ NotificationId ++ ".xml",
    http_request_and_reply(delete, Url, {200, msg}, ApiKey);

%% Fetch all replies
handle_call(replies, _From, ApiKey) ->
    Url = ?BASE_URL ++ "replies.xml",
    http_request_and_reply(get, Url, {200, body}, ApiKey);

%% Catch unknown messages
handle_call(Request, _From, State) ->
  io:format("Call received: ~p~n", [Request]),
  {reply, ignored, State}.

%% Boilerplate OTP stuff...
handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  io:format("Info message received: ~p~n", [_Info]),
  {noreply, State}.

terminate(_Reason, _State) ->
  io:format("Server is stopping...~n"),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
