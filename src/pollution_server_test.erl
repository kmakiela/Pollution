%%%-------------------------------------------------------------------
%%% @author kmakiela
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. May 2018 16:27
%%%-------------------------------------------------------------------
-module(pollution_server_test).
-author("kmakiela").

-include_lib("eunit/include/eunit.hrl").
%% API

start_test() ->
  ?assertEqual(pollution_server:start(), true),
  pollution_server:stop().

stop_test() ->
  pollution_server:start(),
  ?assertEqual(pollution_server:stop(),{reply,ok}).

getOneValue_test() ->
  Time = calendar:local_time(),
  pollution_server:start(),
  pollution_server:addStation("A",{45,54}),
  pollution_server:addStation("B",{11,22}),
  pollution_server:addValue("A",Time,"Uran",15),
  pollution_server:addValue("B",Time,"Uran",25),
  ?assertEqual({reply,15} ,pollution_server:getOneValue("A","Uran",Time)),
  ?assertEqual({reply,25} ,pollution_server:getOneValue("B","Uran",Time)),
  pollution_server:stop().

getStationMean_test() ->
  Time = calendar:local_time(),
  Time2 = calendar:universal_time(),
  pollution_server:start(),
  pollution_server:addStation("A",{45,54}),
  pollution_server:addStation("B",{12,21}),
  pollution_server:addValue("A",Time,"Uran",15),
  pollution_server:addValue("B",Time,"PM",20),
  pollution_server:addValue("A",Time2,"Uran",25),
  pollution_server:addValue("B",Time2,"PM",1),
  ?assertEqual({reply,20.0} ,pollution_server:getStationMean("A","Uran")),
  ?assertEqual({reply,10.5} ,pollution_server:getStationMean("B","PM")),
  pollution_server:stop().

getDailyMean_test() ->
  Time = calendar:local_time(),
  {Date,{_,_,_}} = Time,
  Time2 = calendar:universal_time(),
  {Y,M,D} = Date,
  Date2 = {Y,M,D/2},
  pollution_server:start(),
  pollution_server:addStation("A",{45,54}),
  pollution_server:addStation("B",{12,21}),
  pollution_server:addValue("A",Time,"Urine",15),
  pollution_server:addValue("B",Time,"Urine",20),
  pollution_server:addValue("A",Time2,"Urine",2),
  pollution_server:addValue("B",Time2,"Urine",1),
  ?assertEqual({reply,9.5} ,pollution_server:getDailyMean(Date,"Urine")),
  ?assertEqual({reply,io:format("No measurements matching~n")} ,pollution_server:getDailyMean(Date2,"Urine")),
  pollution_server:stop().
