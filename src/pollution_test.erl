%%%-------------------------------------------------------------------
%%% @author kmakiela
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Apr 2018 18:05
%%%-------------------------------------------------------------------
-module(pollution_test).
-author("kmakiela").

-include_lib("eunit/include/eunit.hrl").

-record(monitor,{locations=#{},stations=#{}}).
-record(measurement,{type, amount, dateAndTime}).
-record(station,{name="",coordinates, measures=#{}}).

createMonitor_test() ->
  {monitor,#{},#{}} = pollution:createMonitor().

addStation_test() ->
  P = pollution:createMonitor(),
  St1 = #station{name = "A",coordinates = {0,0}},
  St2 = #station{name = "B",coordinates = {0,1}},

  P1 = pollution:addStation("A",{0,0},P),
  ?assertEqual(maps:get("A",P1#monitor.stations),St1),
  P2 = pollution:addStation("B",{0,1},P1),
  ?assertEqual(P2,#monitor{locations = #{{0,0}=>"A",{0,1}=>"B"}, stations = #{"A"=>St1,"B"=>St2}}),

  P3 = pollution:addStation("A",{0,2},P2),
  ?assertEqual(P3,#monitor{locations = #{{0,0}=>"A",{0,1}=>"B"}, stations = #{"A"=>St1,"B"=>St2}}),
  P4 = pollution:addStation("C",{0,1},P3),
  ?assertEqual(P3,#monitor{locations = #{{0,0}=>"A",{0,1}=>"B"}, stations = #{"A"=>St1,"B"=>St2}}).

addValue_test() ->
  P = pollution:createMonitor(),
  Time = calendar:local_time(),
  P1 = pollution:addStation("A",{0,0},P),

  P2 = pollution:addValue("A",Time,"Zika",10,P1),
  ?assertEqual(maps:get("A",P2#monitor.stations),#station{name = "A",coordinates = {0,0},
    measures = #{{Time,"Zika"}=>#measurement{type = "Zika",amount = 10,dateAndTime = Time}}}),
  P3 = pollution:addValue("A",Time,"Zika",20,P2),
  ?assertEqual(maps:get("A",P3#monitor.stations),#station{name = "A",coordinates = {0,0},
    measures = #{{Time,"Zika"}=>#measurement{type = "Zika",amount = 10,dateAndTime = Time}}}).

removeValue_test() ->
  P = pollution:createMonitor(),
  Time = calendar:local_time(),
  P1 = pollution:addStation("A",{0,0},P),
  P2 = pollution:addValue("A",Time,"Zika",10,P1),
  ?assertEqual(maps:get("A",P2#monitor.stations),#station{name = "A",coordinates = {0,0},
    measures = #{{Time,"Zika"}=>#measurement{type = "Zika",amount = 10,dateAndTime = Time}}}),

  P3 = pollution:removeValue("A",Time,"Zika",P2),
  ?assertEqual(P3#monitor.stations,#{"A"=>#station{name = "A",coordinates = {0,0},measures = #{}}}).

getOneValue_test() ->
  P = pollution:createMonitor(),
  Time = calendar:local_time(),
  P1 = pollution:addStation("A",{0,0},P),
  P2 = pollution:addValue("A",Time,"Zika",10,P1),

  ?assertEqual(pollution:getOneValue("A","Zika",Time,P2), 10),
  ?assertEqual(pollution:getOneValue("A","Hakuna",Time,P2),
    io:format("No measurement with that parameters~n")).

getStationMean_test() ->
  P = pollution:createMonitor(),
  {{Year,_,_},{_,_,_}} = calendar:local_time(),
  P1 = pollution:addStation("A",{0,0},P),
  P2 = pollution:addValue("A",Year,"Zika",10,P1),
  P3 = pollution:addValue("A",Year+1,"Zika",50,P2),
  ?assertEqual(pollution:getStationMean("A","Zika",P3),30.0).

getDailyMean_test() ->
  P = pollution:createMonitor(),
  {Date,{_,_,_}} = calendar:local_time(),
  Time = calendar:local_time(),
  P1 = pollution:addStation("A",{0,0},P),
  P2 = pollution:addStation("B",{1,0},P1),
  P3 = pollution:addValue("A",Time,"Zika",10,P2),
  P4 = pollution:addValue("B",Time,"Zika",50,P3),

  ?assertEqual(pollution:getDailyMean(Date,"Zika",P4),30.0).

