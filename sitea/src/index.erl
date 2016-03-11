%% -*- mode: nitrogen -*-
-module (index).

-compile(export_all).

-include_lib("nitrogen_core/include/wf.hrl").

main() -> #template { file="./site/templates/nnote.html" }.

title() -> "Welcome".

contain() ->
   top(),
   lsidebar(),
   content().

top() ->
   [
     #h1 {text = "Welcome"}
    ].

lsidebar() ->
   [ #h2 {text="SELECT"},
     #br {},
     #p {},
     select_app(), 
     #br {},
     #p {}
   ].

content() -> 
    [
       #p {id=content}
    ].

event({select, nnote}) ->
   wf:redirect("/nnotes").

select_app() -> 
   #panel {id=select_app, body=[
       #radio {name=pick_app, text="nnote",
             value="nnote",
             postback={select, nnote}},
       #br {}
     ]}.



