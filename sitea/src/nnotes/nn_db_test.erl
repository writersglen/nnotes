
-module(nn_db_test).

-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").

%% @doc We may want to clear the table for testing
%%      purposes. Here's how:

clear_table() ->
   mnesia:clear_table(note).


seed_db() ->
   Notes = nn_notes:seed(),
   [ nn_notes:put_note(Note) || Note <- Notes].

put_note_test() ->
   case seed_db() of
      [ok, ok] -> io:format("Put note test: passed\n");
      _        -> io:format("Put note test: failed\n")
   end.

get_all_test() ->
   List = nn_notes:get_all(),
   case length(List) >= 0 of
      true  -> io:format("Get all test: passed\n");
      false -> io:format("Get all test: failed\n")
   end.

search_test() ->
   Flag = length(nn_notes:search(["SEED"])) >= 2,
   case Flag of
      true -> io:format("Search test: passed\n");
      _    -> io:format("Search test: failed\n")
   end.

date_test() ->
   Note = hd(nn_notes:search(["SEED"])),
   Date = element(3, Note), 
   io:format("Date: ~p~n", [Date]),
   case is_tuple(Date) of
      true  -> io:format("Date test: passed\n");
      false -> io:format("Date test: failed\n")
   end.


