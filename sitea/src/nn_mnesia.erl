%%% -----------------------------------------------
%%% @author Lloyd R. Prentice
%%% @copyright 2014 Lloyd R. Prentice 
%%% @version 0.01
%%% @doc Mnesia primitives 
%%% @end
%%% -----------------------------------------------


-module(nn_mnesia).


-export([one_time/0, 
         init_table/0, 
         start/0,
         seed/0, 
         info/0, 
         stop/0]).

%% Expected API exports
-export([
	get_all/0,
	save_note/6,
	get_note/1,
        put_note/1,
        update_note/6,
	delete/1,
        search/1,
        search_by_date/1,
        search_within_dates/2,
	id/1,
        date/1,
        type/1,
        event/1,
        source/1,
	topic/1,
        question/1,
	note/1
]).

-include_lib("stdlib/include/qlc.hrl").

-define(TABLE, note).

-record(note, {
   id = nn_lib:create_id() :: integer(), 
   date = date()           :: tuple(),
   type                    :: list() | undefined,
   event                   :: list() | undefined,
   source                  :: list() | undefined,
   topic                   :: list() | undefined,
   question                :: list() | undefined,
   note                    :: list() | undefined
   }).

-opaque link() :: #note{}.

-export_type([link/0]).

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
%% Create note table
%% ---------------------------------------------

%% @doc Create note table

init_table() ->
    mnesia:create_table(?TABLE,
        [ {disc_copies, [node()] },
             {attributes,      
                record_info(fields, note)} ]).

%% ---------------------------------------------
%% Execute this one time to initialize db
%% ---------------------------------------------

%% @doc Execute this one time to initialize db

one_time() ->
   schema(),
   init_table().
   
%% -------------------------------------------------
%% Start mnesia
%% -------------------------------------------------

%% @doc Start mnesia

-spec start() -> ok.

start() ->
   mnesia:start().

%% ---------------------------------------------
%% Seed db to simplify testing
%% ---------------------------------------------

%% @doc Seed db to simplify testing

put_record(Record) ->
    Insert =
       fun() ->
           mnesia:write(Record)
       end,
       {atomic, Results} = mnesia:transaction(Insert),
       Results.

%% @doc For testing
       
seed() ->
  [#note{type="Idea",
         topic="SEED Our awesome note program",
         note="A test record"},
   #note{type="Web",
         topic="Testing",
         source="http://nitrogenproject.com/",
         note="SEED Another test record."}
   ].


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

%% ---------------------------------------------
%% Return all links 
%% ---------------------------------------------

%% @doc Return all links 

get_all() ->
    Query =  
    fun() ->
        qlc:eval( qlc:q(
            [ Note || Note <- mnesia:table(?TABLE) ] 
        )) 
    end,
    {atomic, Results} = mnesia:transaction(Query),
    Results.


%% ---------------------------------------------
%% Create web_link table
%% ---------------------------------------------


-spec save_note(Type     :: list(),
                Event    :: list(),
                Source   :: list(),
                Topic    :: list(),
                Question :: list(),
                Note     :: list()
               ) -> tuple().
%% @doc Create web_link table

save_note(Type, Event, Source, Topic, Question, Note) ->
     Record = create_link_record(Type, Event, 
                                 Source, Topic, Question, Note),
	 Results = put_record(Record),
	 {Results, Record}.
	
create_link_record(Type, Event, Source, Topic, Question, Note) ->
   #note{ type=Type,
          event=Event,
          source=Source,
          topic=Topic,
          question=Question,
          note=Note }.

%% ---------------------------------------------
%% Retrieve link 
%% ---------------------------------------------

%% @doc Retrieve link 

get_note(ID) ->
    Query = 
        fun() ->
            mnesia:read({?TABLE, ID})
        end,
    {atomic, Results} = mnesia:transaction(Query),
    Results.

%% ---------------------------------------------
%% store note 
%% ---------------------------------------------

%% @doc store link 

put_note(Note) ->
   put_record(Note). 

%% ---------------------------------------------
%% update note 
%% ---------------------------------------------

%% @doc update note 

update_note(Type, Event, Source, Topic, Question, Note) ->
   save_note(Type, Event, Source, Topic, Question, Note).

%% ---------------------------------------------
%% Delete note 
%% ---------------------------------------------

%% @doc Delete note 

delete(Id) ->
    Query = 
        fun() ->
            mnesia:delete({?TABLE, Id} )
        end,
    {atomic, Results} = mnesia:transaction( Query),
    Results.

%% ---------------------------------------------
%% Search 
%% ---------------------------------------------

%% @doc Search 

filter(SearchList, Note) ->
    #note{type=Type,
          event=Event,
          source=Source,
          topic=Topic,
          question=Question,
          note=Note} = Note,
    List1 = [Type, Event, Source, Topic, Question, Note],
    List2 = [S || S <- List1, is_list(S)],
    Tokens1 = [string:tokens(S, " ") || S <- List2],
    Tokens2 = lists:append(Tokens1),
    Tokens3 = nn_search:dedup(Tokens2),
    Tokens4 = nn_search:shared(SearchList, Tokens3),
    length(Tokens4) > 0.

search(SearchList) ->
    Query =  
    fun() ->
        qlc:eval( qlc:q(
            [ Note || Note <- mnesia:table(?TABLE), filter(SearchList, Note)] 
        )) 
    end,
    {atomic, Results} = mnesia:transaction(Query),
    Results.

%% ---------------------------------------------
%% Search by date
%% ---------------------------------------------

search_by_date(Date) ->
    Query =  
    fun() ->
        qlc:eval( qlc:q(
            [ Note || Note <- mnesia:table(?TABLE), dates_equal(Note, Date)] 
        )) 
    end,
    {atomic, Results} = mnesia:transaction(Query),
    Results.

dates_equal(Note, Date) ->
   NoteDate = Note#note.date,
   NoteDate == Date.

%% ---------------------------------------------
%% Search within dates 
%% ---------------------------------------------

search_within_dates(DateStart, DateEnd) ->
    Query =  
    fun() ->
        qlc:eval( qlc:q(
            [ Note || Note <- mnesia:table(?TABLE), 
               dated_within_dates(Note, DateStart, DateEnd)] 
        )) 
    end,
    {atomic, Results} = mnesia:transaction(Query),
    Results.

dated_within_dates(Note, DateStart, DateEnd) ->
   NoteDate = Note#note.date,
   (NoteDate >= DateStart) and (NoteDate =< DateEnd).




   
%% ---------------------------------------------
%% Retrieve field values 
%% ---------------------------------------------

%% @doc Retrieve id 

id(Note) ->
    Note#note.id.

%% @doc Retrieve date 

date(Note) ->
    Note#note.date.

%% @doc Retrieve type 

type(Note) ->
    Note#note.type.

%% @doc Retrieve event 

event(Note) ->
    Note#note.event.

%% @doc Retrieve source 

source(Note) ->
    Note#note.source.

%% @doc Retrieve topic 

topic(Note) ->
    Note#note.topic.

%% @doc Retrieve question

question(Note) ->
    Note#note.question. 

%% @doc Retrieve note 

note(Note) ->
    Note#note.note.




% field(WebLink, id) -> id(WebLink);
% field(WebLink, topic) -> topic(WebLink);
% field(WebLink, descriptor) -> descriptor(WebLink);
% field(WebLink, url) -> url(WebLink).

% id(#link{id=ID}) -> ID.
% topic(#link{topic=Topic}) -> Topic.
% descriptor(#link{descriptor=Desc}) -> Desc.
% url(#link{url=Url}) -> Url.

