%%%-------------------------------------------------------------------
%%% @author kmakiela
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. May 2018 14:49
%%%-------------------------------------------------------------------
-module(pollution_server).
-author("kmakiela").

%% API
-import(pollution,[createMonitor/0,addStation/3, addValue/5, removeValue/4,getOneValue/4, getStationMean/3, getDailyMean/3,getMaximumGradientStations/2]).
-export([start/0,stop/0,create_monitor/0,addStation/2,addValue/4,removeValue/3,getOneValue/3,getStationMean/2,getDailyMean/2,getMaximumGradientStations/1]).

start() ->
  register(server,spawn(?MODULE,create_monitor,[])).

stop() ->
  call(stop,[]).

create_monitor() ->
  P = pollution:createMonitor(),
  loop(P).

loop(P) ->
  receive
    {request,Pid,add_station,[Name,Loc]} ->
      NewP = addStation(Name,Loc,P),
      Pid ! {reply,ok},
      loop(NewP);
    {request,Pid,add_value,[Name,Date,Type,Amount]} ->
      NewP = addValue(Name,Date,Type,Amount,P),
      Pid ! {reply,ok},
      loop(NewP);
    {request,Pid,remove_value,[Name,Date,Type]} ->
      NewP = removeValue(Name,Date,Type,P),
      Pid ! {reply,ok},
      loop(NewP);
    {request,Pid,get_one_value,[Name,Type,Date]} ->
      Val = getOneValue(Name,Type,Date,P),
      Pid ! {reply,Val},
      loop(P);
    {request,Pid,get_station_mean,[Name,Type]} ->
      Val = getStationMean(Name,Type,P),
      Pid ! {reply,Val},
      loop(P);
    {request,Pid,get_daily_mean,[Date,Type]} ->
      Val = getDailyMean(Date,Type,P),
      Pid ! {reply,Val},
      loop(P);
    {request,Pid,get_gradient,[Type]} ->
      Val = getMaximumGradientStations(Type,P),
      Pid ! {reply,Val},
      loop(P);
    {request,Pid,stop,[]} ->
      Pid ! {reply,ok};
    {request,Pid,_,_} ->
      Pid ! {reply,error,unrecognizedRequest},
      loop(P)
  end.

%% functions w/o monitor

addStation(Name,{_,_}=Loc) ->
  call(add_station,[Name,Loc]).

addValue(Name, Date,Type,Amount) ->
  call(add_value,[Name,Date,Type,Amount]).

removeValue(Name,Date,Type) ->
  call(remove_value,[Name,Date,Type]).

getOneValue(Name,Type,Date) ->
  call(get_one_value,[Name,Type,Date]).

getStationMean(Name,Type) ->
  call(get_station_mean,[Name,Type]).

getDailyMean(Date,Type) ->
  call(get_daily_mean,[Date,Type]).

getMaximumGradientStations(Type) ->
  call(get_gradient,[Type]).

call(Msg,Args) ->
  server ! {request,self(),Msg, Args},
  receive
    M -> M
  after
    100 -> {error}
  end.



