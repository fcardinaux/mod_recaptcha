%% @author François Cardinaux, CH 1207 Geneva (http://bit.ly/qTaona)
%%     and the owner of recaptcha-erlang (http://code.google.com/p/recaptcha-erlang/)
%% @copyright 2011 François Cardinaux
%% @date 2011-09-18
%% @doc Recaptcha module

%% @todo Implement the use of the Ajax API (http://code.google.com/apis/recaptcha/docs/display.html)

-module(mod_recaptcha).
-author("François Cardinaux <fcardinaux@gmail.com>, Driebit <tech@driebit.nl>").

-mod_title("Recaptcha").
-mod_description("Recaptcha module.").

-export([observe_signup_check/3]).

-export([check_recaptcha/1]).

-include_lib("zotonic.hrl").

%% @doc Recaptcha check on a signup
%% This function is called by mod_signup:check_signup/3, where the instruction
%% z_notifier:foldl(signup_check, ...) calls **all** signup_check observers,
%% including this one.
observe_signup_check(signup_check, {ok, Props, SignupProps}, Context) ->
    case m_recaptcha:is_enabled(Context) of
        false ->
            {ok, Props, SignupProps};

        _ ->
            case check_recaptcha(Context) of
                ok ->
                    {ok, Props, SignupProps};
                {error, _} = Error ->
                    Error
            end
    end.

%% @doc This function calls the reCAPTCHA API and verifies that the response
%% corresponds to the challenge
%% @spec check_recaptcha(record()) -> ok | {error, Reason}
check_recaptcha(Context) ->
    case z_context:get_q("g-recaptcha-response", Context) of
        undefined ->
            check_recaptcha1(Context);
        Response ->
            check_recaptcha2(Response, Context)
    end.

check_recaptcha1(Context) ->
    RemoteIP    = get_remote_ip(Context),
    Challenge   = z_context:get_q("recaptcha_challenge_field", Context),
    Response    = z_context:get_q("recaptcha_response_field", Context),

    % Explanation:
    %   * http://erlangexamples.com/2009/02/24/how-to-make-http-post/
    %   * http://code.google.com/p/recaptcha-erlang/
    inets:start(),
    Data = string:join(
                    [
                        string:join(["privatekey",   mochiweb_util:quote_plus(m_recaptcha:private_key(Context))], "="),
                        string:join(["remoteip",     mochiweb_util:quote_plus(RemoteIP)], "="),
                        string:join(["challenge",    mochiweb_util:quote_plus(Challenge)], "="),
                        string:join(["response",     mochiweb_util:quote_plus(Response)], "=")
                    ], "&"),

    HttpResponse =
		httpc:request(
			post,
			{
				get_recaptcha_verify_url(),
				[],
				"application/x-www-form-urlencoded",
				Data
			},
			[], []),

    case HttpResponse of
        {ok, Result} ->
            [FirstLine|NextLines] = case Result of
                {_Status, _Headers, Body} -> string:tokens(Body, "\r\n");
                {_Status, Body}           -> string:tokens(Body, "\r\n")
            end,
            case FirstLine of
                "true" ->
                    ok;
        _ ->
        {error, lists:flatten(NextLines)}
            end;
        {error, _} = Error ->
            Error
    end.

http_post(Url, Data) ->
    EncodedData = z_convert:to_binary(mochiweb_util:urlencode(Data)),
    case httpc:request(post, {Url, [], "application/x-www-form-urlencoded", EncodedData}, [], []) of
        {ok, {
            {_HTTP, StatusCode, _OK},
            _Headers,
            Body
        }} when StatusCode >= 200 andalso StatusCode < 400 ->
            mochijson2:decode(Body);
        Error ->
            Error
    end.

check_recaptcha2(Response, Context) ->
    case http_post(
        "https://www.google.com/recaptcha/api/siteverify",
        [
            {secret, m_recaptcha:get_key(secret, Context)},
            {response, z_url:url_encode(Response)},
            {remoteip, get_remote_ip(Context)}
        ]
    ) of
        {error, Error} ->
            Error;
        {struct, Json} ->
            case proplists:get_value(<<"success">>, Json) of
                true ->
                    ok;
                false ->
                    {error, "invalid response code"}
            end
    end.

%% Support function

get_recaptcha_verify_url() ->
    "http://www.google.com/recaptcha/api/verify".

get_remote_ip(Context) ->
    % This instruction is wrapped in a try-catch statement because it relies on
    % wrq:peer/1, which may theoretically fail, if the function guard isn't
    % respected.
    try m_req:get(peer, Context) of
        IP ->
            IP
    catch
        Error:Reason ->
            io:format("Unable to find remote IP: ~p, ~p~n", [Error, Reason]),
            "127.0.0.1"
    end.
