


-module(wg_utils).

-compile(export_all).

-include_lib("nitrogen_core/include/wf.hrl").


create_id() ->
   Rand = crypto:rand_uniform(100, 999),
   Time = calendar:time_to_seconds(time()),
   Date = calendar:date_to_gregorian_days(date()),
   Time1 = (Time * 1000) + Rand,
   Date1 = (Date * 100000000) + Time1,
   integer_to_list(Date1, 16).
 
