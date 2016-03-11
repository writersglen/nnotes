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
		ID    ->  wf:to_integer(ID)
	end.

content() ->
	NoteID = get_noteid(),
        Note = nn_notes:get_note(NoteID),
        io:format("Note: ~p~n", [Note]),
	form(Note).

%% *******************************************************
%% State 2: Enter new note 
%% ******************************************************


form(new) ->
   Type = wf:q(type),
   [#h2 {text=["Create ", Type, " note"]},
    #br {},
    #p {},
    form(new, "", "", "", "", "","")
   ];

form(Note) ->
   ID   = nn_notes:id(Note),
   Date = nn_notes:date(Note),
   Type = nn_notes:type(Note),
   Event = nn_notes:event(Note),
   Source = nn_notes:source(Note),
   Topic = nn_notes:topic(Note),
   Question = nn_notes:question(Note),
   Note = nn_notes:note(Note),
   [#h2 {text=["Edit ", Type, " note"]},
    #br {},
    #p {},
    form(ID, Date, Type, Event, Source, Topic, Question, Note)
   ].

form(ID, Date, Type, Event, Source, Topic, Question, Note) ->
    wf:defer(save_link, type, #validate{validators=[
          #is_required{text="Type required"}]}),
    wf:defer(save_link, topic, #validate{validators=[
          #is_required{text="Topic required"}]}),
    wf:defer(save_link, note, #validate{validators=[
         #is_required{text="Note required"}]}),
    [ 

      #label{text="ID"},
      #textbox{id=id, text=ID},
      #label{text="Date"},
      #textbox{id=type, text=Date},
      #label{text="Type"},
      #textbox{id=type, text=Type},
      #label{text="Event"},
      #textbox{id=event, text=Event},
      #label{text="Source"},
      #textbox{id=source, text=Source},
      #label{text="Topic"},
      #textbox{id=topic, text=Topic},
      #label{text="Question"},
      #textbox{id=question, text=Question},
      #label{text="Note"},
      #textbox{id=note, text=Note},
      #br{},
      #br {},
      #button { id=save_link,
                text="Submit",
                postback={save, ID}
              },
        #button{text=" Cancel", postback=cancel} 
     ].

event({save, NoteID}) ->
      save(NoteID);

event(cancel) ->
    wf:redirect("/").

save(NoteID) ->
     [Topic, Desc, Url] = wf:mq([topic, descriptor, url]),
      ni_links:save_link(NoteID, Topic, Desc, Url),
      wf:wire(#alert {text = "SAVED"}),
      wf:redirect("/").




