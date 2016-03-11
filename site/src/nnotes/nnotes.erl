%% -*- mode: nitrogen -*-

-module (nnotes).

-compile(export_all).

-include_lib("nitrogen_core/include/wf.hrl").

main() -> #template { file="./site/templates/nnote.html" }.

title() -> "Welcome to Nnote".

%% ***************************************************
%% Grid functions
%% ***************************************************


contain() ->
   lsidebar(),
   content().

lsidebar() ->
   [ #h1 {text="nnote"},
     #br {},
     #p {},
     select_new(),
     select_operation(), 
     #br {},
     #p {},
     #p {id=note_type_menu}
   ].

content() -> 
    [
       #p {id=content}
    ].

%% ***************************************************
%% Left sidebar functions 
%% ***************************************************

select_new() ->
   #panel {id=select_new, body=
      [ #h3 {text="New"},
       #radio {name=pick_op, text="add note",
             value="add_new_note",
             postback={select_task, new_note} },
       #br {},
       #br {}
      ]}.
     

select_operation() -> 
   #panel {id=select_op, body=
      [ #h3 {text="Search"},
       #radio {name=pick_op, text="by type", 
             value="show",
             postback={select_task, query_by_type} },
       #br {},
       #radio {name=pick_op, text="by_topic", 
             value="search",
             postback={select_task, query_by_topic} },
       #br {},
       #radio {name=pick_op, text="by date", 
             value="search_by_date",
             postback={select_task, query_by_date} },
       #br {}
     ]}.

select_note_type(Task) -> 
   NoteTypes = note_types(),
   Radios=[note_type_radio(Type, Task) || Type <- NoteTypes],
   #panel {id=new_note, body= [
      #h3 {text="Note type"}, 
      wf:join(Radios, #br{})
   ]}.

note_types() ->
   [conference, idea, interview,
    lab, lecture, research, web].


note_type_radio(Type, Task) ->
    #radio{
        name=pick_type,
        text=Type,
        value=Type,
        postback={select_note_type, Type, Task}
    }.

%% ***************************************************
%% Event functions 
%% ***************************************************

event({select_task, new_note}) ->
   wf:update(content, content()),
   wf:update(note_type_menu, select_note_type(new_note));
      

event({select_task, Task}) ->
   wf:update(content, content()),
   wf:update(note_type_menu, select_note_type(Task));

event({select_note_type, Type, new_note}) ->
   wf:update(content, content()),
   wf:redirect("/nn_add_edit?" ++ 
      wf:to_qs([{id, new}, {type, Type}]));

event({select_note_type, Type, query_by_type}) ->
   wf:update(content, show_notes_header(Type)),
   wf:update(list_results, query_by_type(Type));

event({select_note_type, Type, query_by_topic}) ->
   wf:update(content, query_by_topic(Type));

event({select_note_type, Type, query_by_date}) ->
   wf:update(content, query_by_date(Type));

event({search_by_type, Type}) ->
   wf:update(list_results, search_by_type(Type));

event({search_by_topic, Type}) ->
   wf:update(list_results, search_by_topic(Type));

event({search_by_date, Type}) ->
   wf:update(list_results, search_by_date(Type));

event({show_note, Task, Note}) ->
   wf:update(list_results, show_note(Task, Note));

event({edit_note, ID}) ->
  wf:redirect("/nn_add_edit?" ++ 
      wf:to_qs([{id, ID}]));

event({delete_note, ID}) ->
   nn_notes_db:delete_note(ID),
   wf:update(search_header, content()).


%% ***************************************************
%% Query functions 
%% ***************************************************

query_by_type(Type) ->
    show_notes_header(Type),
    search_by_type(Type).

query_by_topic(Type) ->
  [ #h3 {text=["Search for ", Type, " notes by keyword"]},
    #textbox{id=search_words, class=standard, placeholder = "kw1, 'kw2' kwN"},
    #button {id=retrieve, text="Search", postback={search_by_topic, Type}},
    #hr {},
    #panel {id=list_results}
  ].

query_by_date(Type) ->
  % Text = "dd-mm-yyyy",
  Date = date(),
  Date1 = wg_dates:convert_to_datepicker(Date),
  [ #h3 {text=["Search ", Type, " notes by date"]},
    wg_dates:datepicker(Date1),   % Text),  
    #button {id=retrieve, text="Search", postback={search_by_date, Type} },
    #hr {},
    #panel {id=list_results}
  ].

%% ***************************************************
%% Search functions 
%% ***************************************************

search_by_type(Type) ->
    Notes = nn_notes_db:get_notes_by_type(Type),
    show_results(type, Notes).


search_by_topic(Type) ->
    SearchList = wf:q(search_words),
    LenList = length(SearchList),
    case LenList < 1 of
       true  -> Notes = nn_notes_db:get_notes_by_type(Type);
       false -> Notes = nn_notes_db:search(Type, SearchList)
    end ,
    show_results(topic, Notes). 

search_by_date(Type) ->
    Date = wf:q(date),
    Date1 = wg_dates:convert_from_datepicker(Date),
    Notes = nn_notes_db:get_notes_by_date_and_type(Date1, Type),
    show_results(date, Notes).

%% ***************************************************
%% Content headers 
%% ***************************************************

show_notes_header(Type) ->
    [#panel {id=content, body = 
        [ #h3 {text= ["Search for ", Type, " notes"]},
         #hr {},
         #panel {id=list_results}
        ]}
    ].

%% ***************************************************
%% Result list functions 
%% ***************************************************

show_results(Task, List) ->
    case length(List) < 1 of
       true  -> [#p {class=show_topic, text="Sorry. Nothing found."}]; 
       false -> [#panel {id = list_results, 
                         body = show_topics(Task, List)
                       }
                ]
    end.

show_topics(Task, Notes) ->
   [show_topic(Task, Note) || Note <- Notes]. 

show_topic(Task, Note) ->
    Topic = nn_notes_db:topic(Note),
    [#link {class=show_topic, 
            text=Topic,
            postback = {show_note, Task, Note}
           },
     #br {}
    ].

%% ***************************************************
%% Display functions 
%% ***************************************************

show_note(Task, Note) ->
    ID       = nn_notes_db:id(Note),
    Type     = nn_notes_db:type(Note),
    Date     = nn_notes_db:date(Note),
    Date1    = wg_dates:format_date(Date),
    Topic    = nn_notes_db:topic(Note),
    Question = nn_notes_db:question(Note),
    Source   = nn_notes_db:source(Note),
    Event    = nn_notes_db:event(Note),
    Note1     = nn_notes_db:note(Note),
    [ #panel {id=note, body=[
        #p { },
        #h3 {body = Date1},
        #panel {class=note, body = [ 
            #p {body = ["<b>Note: </b> ", Note1]}
        ]},
        #hr {},
        #p {body = ["<b>Topic: </b> ", Topic]},
        #p {body = ["<b>Question:</b> ", Question], 
            show_if = Question /= undefined},
        #p {body = ["<b>Source:</b> ", Source],
            show_if = Source /= undefined},
        #p {body = ["<b>Event:</b> ", Event], 
            show_if = Event /= undefined},
        #hr {},
        choose(Task, Type, ID)
    ]}]. 

 
choose(Task, Type, ID) ->
   [ #link {text = "edit | ", postback={edit_note, ID}},
     continue(Task, Type),
%    #link {text= "continue search | ", postback = {search_by_topic, Type}},
    #link {text = "delete", postback={delete_note, ID}}
   ].
 
continue(type, Type)  ->
    #link {text= "continue search | ", postback = {search_by_type, Type}};
continue(topic, Type) ->
    #link {text= "continue search | ", postback = {search_by_topic, Type}};
continue(date, Type)  ->
    #link {text= "continue search | ", postback = {search_by_date, Type}}.

