
-module(nn_search).

-compile(export_all).

%% ---------------------------------------------
%% @doc Search by keyword
%% ---------------------------------------------

dedup(List) ->
    Set = sets:from_list(List),
    sets:to_list(Set).

shared(List1, List2) ->
    S1 = sets:from_list(List1),
    S2 = sets:from_list(List2),
    S3 = sets:intersection(S1, S2),
    sets:to_list(S3).



