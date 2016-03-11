%%  coding: latin-1
%%------------------------------------------------------------
%%
%% Implementation stub file
%% 
%% Target: CosNotifyFilter_InvalidValue
%% Source: /home/vagrant/build-dir_15-12-18_12-20-50/otp-support/lib/cosNotification/src/CosNotifyFilter.idl
%% IC vsn: 4.4
%% 
%% This file is automatically generated. DO NOT EDIT IT.
%%
%%------------------------------------------------------------

-module('CosNotifyFilter_InvalidValue').
-ic_compiled("4_4").


-include("CosNotifyFilter.hrl").

-export([tc/0,id/0,name/0]).



%% returns type code
tc() -> {tk_except,"IDL:omg.org/CosNotifyFilter/InvalidValue:1.0",
            "InvalidValue",
            [{"constr",
              {tk_struct,"IDL:omg.org/CosNotifyFilter/ConstraintExp:1.0",
                  "ConstraintExp",
                  [{"event_types",
                    {tk_sequence,
                        {tk_struct,
                            "IDL:omg.org/CosNotification/EventType:1.0",
                            "EventType",
                            [{"domain_name",{tk_string,0}},
                             {"type_name",{tk_string,0}}]},
                        0}},
                   {"constraint_expr",{tk_string,0}}]}},
             {"value",tk_any}]}.

%% returns id
id() -> "IDL:omg.org/CosNotifyFilter/InvalidValue:1.0".

%% returns name
name() -> "CosNotifyFilter_InvalidValue".



