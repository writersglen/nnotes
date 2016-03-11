
%%% -----------------------------------------------
%%% @author Lloyd R. Prentice
%%% @copyright 2016 Lloyd R. Prentice 
%%% @version 0.01
%%% @doc Mnesia table definitions 
%%% @end
%%% -----------------------------------------------

-module(wg_apps_i).


-define(NOTE_TABLE, note).

%% ---------------------------------------------
%% Create note table
%% ---------------------------------------------

%% @doc Create note table

init_table() ->
    mnesia:create_table(?NOTE_TABLE,
        [ {disc_copies, [node()] },
             {attributes,
                record_info(fields, note)} ]).


