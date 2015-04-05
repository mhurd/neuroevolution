%%%-------------------------------------------------------------------
%%% @author mhurd
%%% @copyright (C) 2015
%%% @doc
%%%
%%% @end
%%% Created : 04. Apr 2015 15:38
%%%-------------------------------------------------------------------
-author("mhurd").

-record(sensor, {
  id, % unique id of the form {sensor, UniqueVal}
  cortex_id, % the id of the Cortex element
  name, % name of the function to execute to get the sensory data
  vector_length, % the length of the resulting sensory data vector
  fanout_ids}). % list of Neuron ids the sensor data will fan out to

-record(actuator, {
  id, % unique id of the form {actuator, UniqueVal}
  cortex_id, % the id of the Cortex element
  name, % name of the function to execute to get the sensory data
  vector_length, % the length of the resulting sensory data vector
  fanin_ids}). % list of Neuron ids connected to the Actuator

-record(neuron, {
  id, % unique id of the form {neuron, {LayerIndex, UniqueVal}}
  cortex_id, % the id of the Cortex element
  activation_function, % the name of the function to apply to the dot product of the input/weight (+bias) vectors
  inputs, % Inputs, tuples of form [{Id1, Weights1} ... {IdN, WeightsN}, {bias, Val}]
  output_ids}). % list of Ids to fanout the output to

-record(cortex, {
  id, % unique id of the form {cortex, UniqueVal}
  sensor_ids, % list of Sensors that produce the sensory inputs to the input layer neurons
  actuator_ids, % list of Actuators that the neural output layer is connected to
  neuron_ids}). % list of all the Neurons in the network