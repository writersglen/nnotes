%%% ni_notes
%%% -----------------------------------------------
%%% @author Lloyd R. Prentice
%%% @copyright 2014 Lloyd R. Prentice 
%%% @version 0.01
%%% @doc  Note functions 
%%% @end
%%% -----------------------------------------------

-module(nn_notes).

-export([init_db/0, 
         start/0,
         seed/0, 
         info/0, 
         stop/0]).


%% Expected API exports
-export([
        get_all/0,
        save_note/7,
        get_note/1,
        put_note/1,
        update_note/7,
        delete_note/1,
        search/1,
        id/1,
        date/1,
        type/1,
        event/1,
        source/1,
        topic/1,
        question/1,
        note/1
]).

%% ---------------------------------------------
%% @doc mnesia specific 
%% ---------------------------------------------

init_db() ->
   nn_mnesia:one_time().

start() ->
   nn_mnesia:start().

seed() ->
   nn_mnesia:seed().

info() ->
   nn_mnesia:info().

stop() ->
   nn_mnesia:stop().

get_all() ->
   nn_mnesia:get_all(). 

%% ---------------------------------------------
%% @doc Save a note (new or existing)
%% ---------------------------------------------

save_note(new, Type, Event, Source, Topic, Question, Note) ->
   nn_mnesia:save_note(new, Type, Event, 
                       Source, Topic, Question, Note);

save_note(ID, Type, Event, Source, Topic, Question, Note) ->
   nn_mnesia:save_note(ID, Type, Event, 
                       Source, Topic, Question, Note).

%% ---------------------------------------------
%% @doc Retrieve note 
%% ---------------------------------------------

get_note(new) ->
   new;

get_note(ID) ->
   nn_mnesia:get_note(ID).

put_note(Webnote) ->
   nn_mnesia:put_note(Webnote).

update_note(ID, Type, Event, Source, Topic, Question, Note) ->
   nn_mnesia:update_note(ID, Type, Event, 
                         Source, Topic, Question, Note).

%% ---------------------------------------------
%% @doc Delete note 
%% ---------------------------------------------

delete_note(ID) ->
   nn_mnesia:delete_note(ID).

%% ---------------------------------------------
%% @doc Search by keyword 
%% ---------------------------------------------

search(SearchList) ->
   nn_mnesia:search(SearchList).

%% ---------------------------------------------
%% @doc Return field values 
%% ---------------------------------------------

id(Note) ->
   nn_mnesia:id(Note).

date(Note) ->
   nn_mnesia:date(Note).

type(Note) ->
   nn_mnesia:type(Note).

event(Note) ->
   nn_mnesia:event(Note).

source(Note) ->
   nn_mnesia:source(Note).

topic(Note) ->
   nn_mnesia:topic(Note).

question(Note) ->
   nn_mnesia:question(Note).

note(Note) ->
   nn_mnesia:note(Note).


