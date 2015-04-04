%%%-------------------------------------------------------------------
%%% @author mhurd
%%% @copyright (C) 2015
%%% @doc
%%%
%%% @end
%%% Created : 04. Apr 2015 15:38
%%%-------------------------------------------------------------------
-author("mhurd").

-record(sensor, {id, cortex_id, name, vector_length, fanout_ids}).
-record(actuator, {id, cortex_id, name, vector_length, fanin_ids}).
-record(neuron, {id, cortex_id, activation_function, input_idps, output_ids}).
-record(cortex, {id, sensor_ids, actuator_ids, neuron_ids}).