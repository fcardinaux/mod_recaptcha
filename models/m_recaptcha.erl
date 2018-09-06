%% @doc Recaptcha model
%% @end
%%
%% @author François Cardinaux, CH 1207 Geneva (http://bit.ly/qTaona)
%% @copyright 2011 François Cardinaux

-module(m_recaptcha).
-author("François Cardinaux <fcardinaux@gmail.com>").

-behaviour(gen_model).

%% interface functions
-export([
    % Callbacks for gen_model behaviour
    m_find_value/3,
    m_to_list/2,
    m_value/2,
    get_key/2
]).

-export([private_key/1, public_key/1, is_enabled/1]).

-include_lib("zotonic.hrl").

%% @doc Fetch the value for the key from a model source
-spec m_find_value(atom(), #m{}, #context{}) -> term().
m_find_value(public_key, #m{value=undefined}, Context) ->
    public_key(Context);
m_find_value(site_key, #m{value=undefined}, Context) ->
    get_key(site_key, Context);
m_find_value(is_enabled, #m{value=undefined}, Context) ->
    is_enabled(Context);

m_find_value(_Key, #m{value=undefined}, _Context) ->
    undefined.

%% @doc Transform a m_config value to a list, used for template loops
%% @spec m_to_list(Source, Context)
m_to_list(_, _Context) ->
    [].

%% @doc Transform a model value so that it can be formatted or piped through filters
%% @spec m_value(Source, Context) -> term()
m_value(#m{value=undefined}, _Context) ->
    undefined.

%% @doc Get the private key
%% @spec private_key(record()) -> string()
private_key(Context) ->
    get_key(private_key, Context).

%% @doc Get the public key
%% @spec public_key(record()) -> string()
public_key(Context) ->
    get_key(public_key, Context).

is_enabled(Context) ->
    z_notifier:foldl(recaptcha_enabled, true, Context).

%% Support function

get_key(Type, Context) ->
    Val = m_config:get_value(mod_recaptcha, Type, Context),

    SVal  = if
                is_binary(Val) ->
                    binary_to_list(Val);
                true ->
                    Val
            end,

    string:strip(SVal).



