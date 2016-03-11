%%% -----------------------------------------------
%%% @author Lloyd R. Prentice
%%% @copyright 2014 Lloyd R. Prentice 
%%% @version 0.01
%%% @doc ni_lib 
%%% @end
%%% -----------------------------------------------


-module(nn_lib).

%% Expected API exports

-export([
          create_id/0
       ]). 

create_id() ->
    crypto:rand_uniform(1, 9999999).

