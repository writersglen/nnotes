%%  coding: latin-1
%%------------------------------------------------------------
%%
%% Implementation stub file
%% 
%% Target: CosFileTransfer_SupportedProtocolAddresses
%% Source: /home/vagrant/build-dir_15-12-18_12-20-50/otp-support/lib/cosFileTransfer/src/CosFileTransfer.idl
%% IC vsn: 4.4
%% 
%% This file is automatically generated. DO NOT EDIT IT.
%%
%%------------------------------------------------------------

-module('CosFileTransfer_SupportedProtocolAddresses').
-ic_compiled("4_4").


-include("CosFileTransfer.hrl").

-export([tc/0,id/0,name/0]).



%% returns type code
tc() -> {tk_sequence,{tk_struct,"IDL:omg.org/CosFileTransfer/ProtocolSupport:1.0",
                                "ProtocolSupport",
                                [{"protocol_name",{tk_string,0}},
                                 {"addresses",{tk_sequence,{tk_string,0},0}}]},
                     0}.

%% returns id
id() -> "IDL:omg.org/CosFileTransfer/SupportedProtocolAddresses:1.0".

%% returns name
name() -> "CosFileTransfer_SupportedProtocolAddresses".



