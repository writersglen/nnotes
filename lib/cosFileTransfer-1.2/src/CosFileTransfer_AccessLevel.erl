%%  coding: latin-1
%%------------------------------------------------------------
%%
%% Implementation stub file
%% 
%% Target: CosFileTransfer_AccessLevel
%% Source: /home/vagrant/build-dir_15-12-18_12-20-50/otp-support/lib/cosFileTransfer/src/CosFileTransfer.idl
%% IC vsn: 4.4
%% 
%% This file is automatically generated. DO NOT EDIT IT.
%%
%%------------------------------------------------------------

-module('CosFileTransfer_AccessLevel').
-ic_compiled("4_4").


-include("CosFileTransfer.hrl").

-export([tc/0,id/0,name/0]).



%% returns type code
tc() -> {tk_struct,"IDL:omg.org/CosFileTransfer/AccessLevel:1.0",
                   "AccessLevel",
                   [{"read",tk_boolean},
                    {"insert",tk_boolean},
                    {"replace",tk_boolean},
                    {"extend",tk_boolean},
                    {"erase",tk_boolean},
                    {"read_attr",tk_boolean},
                    {"change_attr",tk_boolean},
                    {"delete",tk_boolean}]}.

%% returns id
id() -> "IDL:omg.org/CosFileTransfer/AccessLevel:1.0".

%% returns name
name() -> "CosFileTransfer_AccessLevel".



