%% @author François Cardinaux, CH 1207 Geneva (http://bit.ly/qTaona)
%%     and the owner of recaptcha-erlang (http://code.google.com/p/recaptcha-erlang/)
%% @copyright 2011 François Cardinaux
%% @date 2011-09-18
%% @doc Recaptcha module

%% @todo Implement the use of the Ajax API (http://code.google.com/apis/recaptcha/docs/display.html)

-module(mod_recaptcha).
-author("François Cardinaux <fcardinaux@gmail.com>").

-mod_title("Recaptcha").
-mod_description("Recaptcha module.").

-export([observe_signup_check/3]).

-include_lib("zotonic.hrl").

%% @doc Recaptcha check on a signup
%% This function is called by mod_signup:check_signup/3, where the instruction
%% z_notifier:foldl(signup_check, ...) calls **all** signup_check observers, 
%% including this one. 
observe_signup_check(signup_check, {ok, Props, SignupProps}, Context) ->
    Challenge   = z_context:get_q("recaptcha_challenge_field", Context),
    Response    = z_context:get_q("recaptcha_response_field", Context),
    
    case check_recaptcha(Challenge, Response, Context) of
        ok -> 
            {ok, Props, SignupProps};
        {error, _} = Error -> 
            Error
    end.
    
%% Support functions

%% @doc This function calls the reCAPTCHA API and verifies that the response
%% corresponds to the challenge
check_recaptcha(Challenge, Response, Context) ->

    RemoteIP = "127.0.0.1", %% @todo Replace this with the remote IP address
    
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
                        
get_recaptcha_verify_url() ->
    "http://www.google.com/recaptcha/api/verify".

