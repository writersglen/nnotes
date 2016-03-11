

-module(nn_search).

-compile(export_all).

%% ---------------------------------------------
%% @doc Search by keyword
%% ---------------------------------------------



filter(SearchString, DataList) ->
    SearchTokens = to_tokens(SearchString),
    DataTokens = data_to_tokens(DataList),
    List = shared(SearchTokens, DataTokens),
    length(List) > 0.


to_tokens(String) ->
    Tokens1 = string:tokens(String, ".,;:? "),
    Tokens3 = [string:strip(Token) || Token <- Tokens1],
    [string:to_lower(Token) || Token <- Tokens3].

data_to_tokens(DataList) ->
    List = [Item || Item <- DataList, is_list(Item)],
    Tokens1 = [string:tokens(String, " ") || String <- List], 
    Tokens2 = lists:append(Tokens1),
    Tokens3 = [string:to_lower(Token) || Token <- Tokens2],
    dedup(Tokens3).

dedup(List) ->
    Set = sets:from_list(List),
    sets:to_list(Set).

shared(List1, List2) ->
    S1 = sets:from_list(List1),
    S2 = sets:from_list(List2),
    S3 = sets:intersection(S1, S2),
    sets:to_list(S3).



