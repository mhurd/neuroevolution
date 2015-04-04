%%%-------------------------------------------------------------------
%%% @author mhurd
%%% @copyright (C) 2015
%%% @doc
%%%
%%% @end
%%% Created : 04. Apr 2015 11:14
%%%-------------------------------------------------------------------
-module(simple_neuron).
-author("mhurd").

%% API
-export([create/0, sense/1, loop/1]).

create() ->
  Weights = [random:uniform()-0.5, random:uniform() - 0.5, random:uniform() - 0.5],
  register(neuron, spawn(?MODULE, loop, [Weights])).

loop(Weights) ->
  receive
    {From, Input} ->
      io:format("****Processing****~n Input:~p~n Using Weights:~p~n", [Input, Weights]),
      Dot_Product = dot(Input, Weights, 0),
      Output = [math:tanh(Dot_Product)],
      From ! {result, Output},
      loop(Weights);
      Msg -> io:format("****Unknown message****~n Msg:~w~n", [Msg])
  end.

% The dot product function works on the assumption that the Bias (if present) is incorporated in the Weights vector as
% the last element. After calculating the dot product the Input vector will be empty and the Bias will be left in the
% Weights (if there is one).

dot([I|Input], [W|Weights], Acc) ->
  dot(Input, Weights, I*W+Acc);
dot([], [Bias], Acc) ->
  Acc+Bias;
dot([],[], Acc) ->
  Acc.

sense(Signal) ->
  case is_list(Signal) and (length(Signal) == 2) of
    true ->
      neuron ! {self(), Signal},
      receive
        {result, Output} ->
          io:format(" Output: ~p~n", [Output])
      end;
    false ->
      io:format("The Signal must be a list of length 2")
  end.

