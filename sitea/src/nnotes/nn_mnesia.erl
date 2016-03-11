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
         seed_db/0, 
         info/0, 
         stop/0]).

%% Expected API exports
-export([
	get_all/0,
        get_notes_by_type/1,
        get_notes_by_date_and_type/2,
	save_note/8,
	get_note/1,
        put_note/1,
        update_note/8,
	delete_note/1,
        search/2,
        search_within_dates/2,
	id/1,
        date/1,
        type/1,
        event/1,
        source/1,
	topic/1,
        question/1,
        note_data/1,
        note_filter/2,
	note/1
]).

-include_lib("stdlib/include/qlc.hrl").

-define(TABLE, note).

-record(note, {
   id         :: list(), 
   date       :: tuple(),
   type       :: atom() | undefined,
   event      :: list() | undefined,
   source     :: list() | undefined,
   topic      :: list() | undefined,
   question   :: list() | undefined,
   note       :: list() 
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
  [#note{id   = wg_utils:create_id(),
         date = date(),
         type=idea,
         topic="SEED Our awesome note program",
         note="A test record"},
   #note{id   = wg_utils:create_id(),
         date = date(),
         type=web,
         topic="Testing",
         source="http://nitrogenproject.com/",
         note="SEED Another test record."}
   ].

seed_db() ->
   [put_record(Note) || Note <- seed()].

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
%% Retrieve notes by type  
%% ---------------------------------------------

get_notes_by_type(Type) ->
    Query =  
    fun() ->
        qlc:eval( qlc:q(
            [ Note || Note <- mnesia:table(?TABLE), type(Note) == Type ] 
        )) 
    end,
    {atomic, Results} = mnesia:transaction(Query),
    Results.


%% ---------------------------------------------
%% Save note 
%% ---------------------------------------------


-spec save_note(ID       :: list(),
                Date     :: list(),
                Type     :: atom(),
                Event    :: list() | atom(),
                Source   :: list() | atom(),
                Topic    :: list() | atom(),
                Question :: list() | atom(),
                Note     :: list() | atom()
               ) -> tuple().
%% @doc Create web_link table

save_note(ID, Date, Type, Event, Source, Topic, Question, Note) ->
     io:format("Entering mnesia save\n"),
     Record = create_note_record(ID, Date, Type, Event, 
                                 Source, Topic, Question, Note),
	 Results = put_record(Record),
	 {Results, Record}.
	
create_note_record(ID, Date, Type, Event, Source, Topic, Question, Note) ->
   io:format("Entering mnesia create_note_record\n"),
   #note{ id=ID,
          date=Date,
          type=Type,
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
    case length(Results) < 1 of
       true  -> [];
       false -> hd(Results)
    end.

   


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

update_note(ID, Date, Type, Event, Source, Topic, Question, Note) ->
   save_note(ID, Date, Type, Event, Source, Topic, Question, Note).

%% ---------------------------------------------
%% Delete note 
%% ---------------------------------------------

%% @doc Delete note 

delete_note(Id) ->
    Query = 
        fun() ->
            mnesia:delete({?TABLE, Id} )
        end,
    {atomic, Results} = mnesia:transaction( Query),
    Results.

%% ---------------------------------------------
%% Search 
%% ---------------------------------------------

note_data(Note) ->
   #note{ type=Type,
          event=Event,
          source=Source,
          topic=Topic,
          question=Question,
          note=Note1} = Note,
   [Type, Event, Source, Topic, Question, Note1].

%% @doc Search 

note_filter(SearchString, Note) ->
    List = note_data(Note),
    nn_search:filter(SearchString, List). 

search(Type, SearchList) ->
    Query =  
    fun() ->
        qlc:eval( qlc:q(
            [ Note || Note <- mnesia:table(?TABLE), 
                 note_filter(SearchList, Note), type(Note) == Type] 
        )) 
    end,
    {atomic, Results} = mnesia:transaction(Query),
    Results.

%% ---------------------------------------------
%% Search by date
%% ---------------------------------------------

get_notes_by_date_and_type(Date, Type) ->
    Query =  
    fun() ->
        qlc:eval( qlc:q(
            [ Note || Note <- mnesia:table(?TABLE), 
                date_and_type(Note, Date, Type)] 
        )) 
    end,
    {atomic, Results} = mnesia:transaction(Query),
    Results.

date_and_type(Note, Date, Type) ->
   NoteDate = Note#note.date,
   NoteType = Note#note.type,
   (NoteDate == Date) and (NoteType == Type).

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

