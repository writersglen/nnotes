-module(nn_add_edit).

-compile(export_all).

-include_lib("nitrogen_core/include/wf.hrl").


main() -> #template{file="./site/templates/nnote.html"}.

title() -> "Add/Edit Note".

%% Will return the ID from the trailing part of the URL.
%% For example, a request to http://localhost:8000/edit/xyz
%% will return the string "xyz". If the request is just for
%% http://localhost:8000/edit (with the trailing part being
%% blank), wf:path_info() will return "", and so we'll return
%% the atom 'new' to be for a new element.
get_noteid() ->
        ID   = wf:q(id),
	case ID of
		"new" -> new;
		ID    -> ID
	end.

content() ->
	NoteID = get_noteid(),
        Note = nn_notes_db:get_note(NoteID),
	form(Note).

%% *******************************************************
%% State 2: Enter new note 
%% ******************************************************


form(new) ->
   ID   = wg_utils:create_id(),
   Type = wf:q(type),
   Date = date(),
   wf:disable(id), 
   [#h2 {text=["Create ", Type, " note"]},
    #br {},
    #p {},
    form(ID, Date, Type, "", "", "", "","")
   ];

form(Note) ->
   ID       = nn_notes_db:id(Note),
   Date     = nn_notes_db:date(Note),
   Type     = nn_notes_db:type(Note),
   Event    = nn_notes_db:event(Note),
   Source   = nn_notes_db:source(Note),
   Topic    = nn_notes_db:topic(Note),
   Question = nn_notes_db:question(Note),
   Note1    = nn_notes_db:note(Note),
   [#h2 {text=["Edit ", Type, " note"]},
    #br {},
    #p {},
    form(ID, Date, Type, Event, Source, Topic, Question, Note1)
   ].

form(ID, Date, Type, Event, Source, Topic, Question, Note) ->
    wf:defer(save_link, type, #validate{validators=[
          #is_required{text="Type required"}]}),
    wf:defer(save_link, topic, #validate{validators=[
          #is_required{text="Topic required"}]}),
    wf:defer(save_link, note, #validate{validators=[
         #is_required{text="Note required"}]}),
    wf:disable(id), 
    [ 
      enter_id(ID),
      enter_date(Date),
      enter_type(Type),
      clear(),
      #label{text="Event"},
      #textbox{id=event, size=40, text=Event},
      #label{text="Source"},
      #textbox{id=source, size=40, text=Source},
      #label{text="Topic"},
      #textbox{id=topic, size=40,text=Topic},
      #label{text="Question"},
      #textbox{id=question, size=40,text=Question},
      #label{text="Note"},
      #textarea{id=note, 
                text=Note,
                rows=4,
                columns=57},
      #br{},
      #br {},
      #button { id=save_note,
                text="Submit",
                postback={save, ID}
              },
        #button{text=" Cancel", postback=cancel},
        #br {},
        #br {},
        #br {},
        #p {}  
     ].

enter_id(ID) ->
    [#panel {class=enter_id, body=
        [ #label{text="ID"},
          #textbox{id=id, text=ID, size=7}
        ]
    }].
   

enter_date(Date) ->
   io:format("Date: ~p~n", [Date]),
    Date1 = wg_dates:convert_to_datepicker(Date),
    [#panel {class=enter_date, body=
        [ #label{text="Date"},
          wg_dates:datepicker(Date1)
        ]
    }].
   
enter_type(Type) ->
    List = nnotes:note_types(),
    [#panel {class=enter_type, body =
        [ #label{text="Type"},
          #dropdown {id=type, value=Type, options=
            [#option {text=Item, value=Item} || Item <- List]
        }]
    }].
    
clear() ->
    [ #panel {class=clear, body= [ ]
             },
      #br {}
    ].
   

event({save, NoteID}) ->
      save(NoteID);

event(cancel) ->
    wf:redirect("/nnotes").


save(NoteID) ->
     [Date, Type, Event, Source, Topic, Question, Note] = 
             wf:mqs([date,
                    type, 
                    event, 
                    source, 
                    topic, 
                    question, 
                    note]
                   ),
     Date1 = wg_dates:convert_from_datepicker(hd(Date)),
     Type1 = list_to_atom(hd(Type)),
     nn_notes_db:save_note(NoteID, 
                           Date1,
                           Type1, 
                           hd(Event), 
                           hd(Source), 
                           hd(Topic), 
                           hd(Question), 
                           hd(Note) 
                          ),
     wf:wire(#alert {text = "SAVED"}),
     wf:redirect("/nnotes").




