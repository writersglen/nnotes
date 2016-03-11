


-module(wg_dates).

-compile(export_all).

-include_lib("nitrogen_core/include/wf.hrl").
 
 
datepicker(Date) ->
   #datepicker_textbox {
      id=date,
      text=Date,
     size=12,
     % placeholder="mm/dd/yy",
      options=[
         {dateFormat, "mm/dd/yy"},
         {showButtonPanel, true}
      ]
    }.

format_date(Date) ->
   {Y, M, D} = Date,
   Month = format_month(M),
   Year = integer_to_list(Y),
   Day = integer_to_list(D),
   Month ++ Day ++ ", " ++ Year.



format_month(N) ->
   Months = {"January ",
             "February ",
             "March ",
             "April ",
             "May ",
             "June ",
             "July ",
             "August ",
             "September ",
             "October ",
             "Novemer ",
             "December"
            },
    element(N, Months).

convert_to_datepicker([]) ->
   "";

convert_to_datepicker(Date) ->
   Month = integer_to_list(element(2, Date)),
   Day   = integer_to_list(element(3, Date)),
   Year  = integer_to_list(element(1, Date)),
   Month ++ "/" ++ Day ++ "/" ++ Year.

convert_from_datepicker(Date) ->
   Tokens = string:tokens(Date, "/- "),
   Integers = [list_to_integer(Token) || Token <- Tokens],
   [Day | Tail] = Integers,
   [Month | Tail1] = Tail,
   [Year | _] = Tail1,
   {Year, Day, Month}.






