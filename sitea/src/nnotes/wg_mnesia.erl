%%% -----------------------------------------------
%%% @author Lloyd R. Prentice
%%% @copyright 2014 Lloyd R. Prentice 
%%% @version 0.01
%%% @doc Mnesia primitives 
%%% @end
%%% -----------------------------------------------


-module(wg_mnesia).


-export([one_time/0, 
         start/0,
         info/0, 
         stop/0]).

%% -------------------------------------------------
%% Initialize mnesia 
%% -------------------------------------------------

%% @doc Initialize mnesia

-spec schema() -> ok | {error, Reason}
    when Reason :: term().

schema() ->
	case mnesia:create_schema([node()]) of
		ok -> ok;
		{error, {_, {already_exists, _}}} -> ok;
		Other -> exit(Other)
	end,
    mnesia:start().

%% ---------------------------------------------
%% Execute this one time to initialize db
%% ---------------------------------------------

%% @doc Execute this one time to initialize db

one_time() ->
   schema().
%   init_table().
   
%% -------------------------------------------------
%% Start mnesia
%% -------------------------------------------------

%% @doc Start mnesia

-spec start() -> ok.

start() ->
   mnesia:start().

%% -------------------------------------------------
%% Mnesia info 
%% -------------------------------------------------

%% @doc display Mneia info

-spec info() -> ok.

info() ->
  mnesia:info().

%% -------------------------------------------------
%% Stop mnesia table
%% -------------------------------------------------

%% @doc Stop mnesia

-spec stop() -> stopped.

stop() ->
   mnesia:stop().

