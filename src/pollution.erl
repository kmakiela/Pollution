%%%-------------------------------------------------------------------
%%% @author kmakiela
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Apr 2018 19:52
%%%-------------------------------------------------------------------
-module(pollution).
-author("kmakiela").

%% API
-export([createMonitor/0,addStation/3, addValue/5, removeValue/4,getOneValue/4, getStationMean/3, getDailyMean/3,getMaximumGradientStations/2]).

-record(monitor,{locations=#{},stations=#{}}).
-record(measurement,{type, amount, dateAndTime}).
-record(station,{name="",coordinates, measures=#{}}).

createMonitor() -> #monitor{}.

addStation(Name,{_,_}=Loc,Monitor) ->
  case maps:is_key(Name,Monitor#monitor.stations) of
    true -> Monitor;
    false ->
      case maps:is_key(Loc,Monitor#monitor.locations) of
        true -> Monitor;
        false -> #monitor{
          locations = maps:put(Loc,Name,Monitor#monitor.locations),
          stations = maps:put(Name,#station{name=Name,coordinates = Loc},Monitor#monitor.stations)
        }
      end
  end.

addValue(Name,Date,Type,Amount,Monitor) ->
  case maps:find(Name,Monitor#monitor.stations) of
    error -> Monitor;
    {ok,Station} ->
      case maps:is_key({Date,Type},Station#station.measures) of
        true ->Monitor;
        false ->
          Monitor#monitor{
            stations=maps:put(Name,Station#station{
              measures = maps:put({Date,Type},#measurement{
                type = Type,dateAndTime = Date,amount = Amount
              },Station#station.measures)
            },Monitor#monitor.stations)}
      end
  end;

addValue({_,_}=Loc,Date,Type,Amount,Monitor) ->
  case maps:find(Loc,Monitor#monitor.locations) of
    error -> Monitor;
    {ok,Name} ->
      case maps:find(Name,Monitor#monitor.stations) of
        error -> Monitor;
        {ok,Station} ->
          case maps:is_key({Date,Type},Station#station.measures) of
            true ->Monitor;
            false ->
              Monitor#monitor{
                stations=maps:put(Name,Station#station{
                  measures = maps:put({Date,Type},#measurement{
                    type = Type,dateAndTime = Date,amount = Amount
                  },Station#station.measures)
                },Monitor#monitor.stations)}
          end
      end
  end.

removeValue(Name,Date,Type,Monitor) ->
  case maps:find(Name,Monitor#monitor.stations) of
    error -> Monitor;
    {ok,Station} ->
      Monitor#monitor{
        stations = maps:update(Name,Station#station{
          measures = maps:remove({Date,Type},Station#station.measures)
        },Monitor#monitor.stations)}
  end;
removeValue({_,_}=Loc,Date,Type,Monitor) ->
  case maps:find(Loc,Monitor#monitor.locations) of
    error -> Monitor;
    {ok,Name} ->
      case maps:find(Name,Monitor#monitor.stations) of
        error -> Monitor;
        {ok,Station} ->
          Monitor#monitor{
            stations = maps:update(Name,Station#station{
              measures = maps:remove({Date,Type},Station#station.measures)
            },Monitor#monitor.stations)}
      end
  end.

getOneValue(Name,Type,Date,Monitor) ->
  case maps:find(Name,Monitor#monitor.stations) of
    error -> io:format("No station with that name~n");
    {ok,Station} ->
      case maps:find({Date,Type},Station#station.measures) of
        error -> io:format("No measurement with that parameters~n");
        {ok,Val} -> Val#measurement.amount
      end
  end.

getStationMean(Name,Type,Monitor) ->
  case maps:find(Name, Monitor#monitor.stations) of
    error -> io:format("No station with that name~n");
    {ok,Station} ->
      {Sum,Elements} = maps:fold(fun(_,V,{Sum,Elements}) -> {Sum+V#measurement.amount,Elements+1} end,
        {0,0},maps:filter(fun(_,Mes)-> Mes#measurement.type==Type end, Station#station.measures)),
        case Elements of
          0 -> io:format("No measurements matching~n");
          _ -> Sum/Elements
        end
  end.

getDailyMean(Date,Type,Monitor) ->
  {Sum,Elements} = maps:fold(fun(_,{SStation,EStation},{S,E}) -> {S+SStation,E+EStation} end,{0,0},
    maps:map(fun(_,Station) ->
             maps:fold(fun(_,Mes,{SumS,ElS}) -> {SumS+Mes#measurement.amount,ElS+1} end, {0,0},
               maps:filter(fun(_,V) -> (V#measurement.type==Type) and (dateAndTimetoDate(V#measurement.dateAndTime)==Date)
                           end, Station#station.measures))
             end, Monitor#monitor.stations)),
  case Elements of
    0 -> io:format("No measurements matching~n");
    _ -> Sum/Elements
  end.

dateAndTimetoDate(DateTime) ->
  {Date,{_,_,_}} = DateTime,
  Date.

getMaximumGradientStations(Type,Monitor) ->
  {Final1,Final2,FinalGradient} = maps:fold(fun(Name,{Min,Max},{St1,St2,Gradient}) ->
    {ok,St} = maps:find(Name,Monitor#monitor.stations),
    case calculateGradient(St,{Min,Max},St1,getMinandMax(St1,Type)) > Gradient of
      true -> {St,St1,calculateGradient(St,{Min,Max},St1,getMinandMax(St1,Type))};
      false -> case calculateGradient(St,{Min,Max},St2,getMinandMax(St2,Type)) > Gradient of
                 true -> {St,St2,calculateGradient(St,{Min,Max},St2,getMinandMax(St2,Type))};
                 false -> {St1,St2,Gradient}
               end
    end
    end,{#station{coordinates = {0,0},measures = #{}},#station{coordinates = {0,0},measures = #{}},0},maps:map(fun(_,Station) ->
    getMinandMax(Station,Type)end,Monitor#monitor.stations)),
  {Final1,Final2,FinalGradient}.

 calculateGradient(St1,{Min1,Max1},St2,{Min2,Max2}) ->
   {X1,Y1} = St1#station.coordinates,
   {X2,Y2} = St2#station.coordinates,
   D = math:sqrt(math:pow(X2-X1,2)+math:pow(Y2-Y1,2)),
   case abs(Max1-Min2)/D > abs(Max2-Min1)/D  of
     true -> (Max1-Min2)/D;
     false -> (Max2-Min1)/D
   end.

getMinandMax(St,Type) ->
  {FMin,FMax} = maps:fold(fun(_,Mes,{Min,Max}) ->
    case Mes#measurement.type==Type of
      false -> {Min,Max};
      true -> if
                Mes#measurement.amount<Min -> {Mes#measurement.amount,Max};
                Mes#measurement.amount>Max -> {Min,Mes#measurement.amount};
                true -> {Min,Max}
              end
    end
    end,
    {0,0},St#station.measures),
  {FMin,FMax}.