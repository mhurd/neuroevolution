%%%-------------------------------------------------------------------
%%% @author mhurd
%%% @copyright (C) 2015
%%% @doc
%%%
%%% @end
%%% Created : 04. Apr 2015 15:42
%%%-------------------------------------------------------------------
-module(constructor).
-author("mhurd").
-include("records.hrl").
%% API
-export([construct_Genotype/3, construct_Genotype/4]).

construct_Genotype(SensorName, ActuatorName, HiddenLayerDensities) ->
  construct_Genotype(ffnn, SensorName, ActuatorName, HiddenLayerDensities).

construct_Genotype(FileName, SensorName, ActuatorName, HiddenLayerDensities) ->
  S = create_Sensor(SensorName),
  A = create_Actuator(ActuatorName),
  % we need to know the Actuator input vector length as this will dictate the
  % number of Neurons in the output layer
  Output_VL = A#actuator.vector_length,
  % add that output layer of Neurons to the HiddenLayerDensities
  LayerDensities = lists:append(HiddenLayerDensities, [Output_VL]),
  Cx_Id = {cortex, generate_id()},
  Neurons = create_NeuroLayers(Cx_Id, S, A, LayerDensities), % create all the neural layers
  % get all the input and output layers Neurons
  [Input_Layer|_] = Neurons,
  [Output_Layer|_] = lists:reverse(Neurons),
  FirstLayer_NIds = [N#neuron.id || N <- Input_Layer],
  LastLayer_NIds = [N#neuron.id || N <- Output_Layer],
  N_Ids = [N#neuron.id || N <- lists:flatten(Neurons)],
  % wire the Sensor to the first layer Neurons
  Sensor = S#sensor{cortex_id = Cx_Id, fanout_ids = FirstLayer_NIds},
  % wire the last layer Neurons to the Actuator
  Actuator = A#actuator{cortex_id = Cx_Id, fanin_ids = LastLayer_NIds},
  % create the Cortex and flatten all the data intoa list to output to file as the Genotype
  Cortex = create_Cortex(Cx_Id, [S#sensor.id], [A#actuator.id], N_Ids),
  Genotype = lists:flatten([Cortex, Sensor, Actuator|Neurons]),
  {ok, File} = file:open(FileName, write),
  lists:foreach(fun(X) -> io:format(File, "~p~n", [X]) end, Genotype),
  file:close(File).

create_Sensor(SensorName) ->
  case SensorName of
    rng -> #sensor{id = {sensor, generate_id()}, name = rng, vector_length = 2}; % 'senses' 2 random numbers
    _ -> exit("System does not yet support a sensor named: ~p", [SensorName])
  end.

create_Actuator(ActuatorName) ->
  case ActuatorName of
    pts -> #actuator{id = {actuator, generate_id()}, name = pts, vector_length = 1};
    _ -> exit("System does not yet support an actuator named: ~p", [ActuatorName])
  end.

% create the NN, we support a single Sensor input the specified LayerDensities and the
% single Actuator (and associated Neuron/s).
create_NeuroLayers(Cx_Id, Sensor, Actuator, LayerDensities) ->
  LayerIndex = 1,
  Sensor_Ids = [{Sensor#sensor.id, Sensor#sensor.vector_length}], % the single Sensor input
  Total_Layers = length(LayerDensities),
  [Total_FL_Neurons|Next_LDs] = LayerDensities,
  % generate an id for each neuron in the first layer
  NIds = [{neuron, {LayerIndex, Id}} || Id <- generate_ids(Total_FL_Neurons, [])],
  create_NeuroLayers(Cx_Id, Actuator#actuator.id, LayerIndex, Total_Layers, Sensor_Ids, NIds, Next_LDs, []).

create_NeuroLayers(Cx_Id, Actuator_Id, LayerIndex, Total_Layers, Inputs, NIds, [Next_LD|LDs], Acc) ->
  % generate the next layer's neuron ids (these are outputs to this layer)
  Output_NIds = [{neuron, {LayerIndex + 1, Id}} || Id <- generate_ids(Next_LD, [])],
  % create this layer (supplying the output Neuron Ids)
  Layer_Neurons = create_NeuroLayer(Cx_Id, Inputs, NIds, Output_NIds, []),
  Next_Inputs = [{NId, 1} || NId <- NIds], % get all this layers Neuron's Ids, they are Inputs for the next layer
  % recurse creating the next layer and so forth
  create_NeuroLayers(Cx_Id, Actuator_Id, LayerIndex + 1, Total_Layers, Next_Inputs, Output_NIds, LDs, [Layer_Neurons|Acc]);
create_NeuroLayers(Cx_Id, Actuator_Id, Total_Layers, Total_Layers, Inputs, N_Ids, [], Acc) ->
  Output_Ids = [Actuator_Id],
  Layer_Neurons = create_NeuroLayer(Cx_Id, Inputs, N_Ids, Output_Ids, []),
  % final layer, add to the Accumilator and then reverse to get the layers in the correct order
  lists:reverse([Layer_Neurons|Acc]).

create_NeuroLayer(Cx_Id, Inputs, [Id| NIds], Output_Ids, Acc) ->
  % create a Neuron with the specified Inputs fanning out to the specified Outputs
  Neuron = create_Neuron(Inputs, Id, Cx_Id, Output_Ids),
  % recurse through the remaining Neurons in the layer
  create_NeuroLayer(Cx_Id, Inputs, NIds, Output_Ids, [Neuron|Acc]);
create_NeuroLayer(_Cx_Id, _Input_IdPs, [], _Output_Ids, Acc) ->
  Acc.

create_Neuron(Inputs, Id, Cx_Id, Output_Ids) ->
  Proper_Inputs = create_NeuralInput(Inputs, []),
  #neuron{id = Id, cortex_id = Cx_Id, activation_function = tanh, inputs = Proper_Inputs, output_ids = Output_Ids}.

create_NeuralInput([{Input_Id, Input_VL}|Inputs], Acc) ->
  % create the neural weights for each of the Inputs
  Weights = create_NeuralWeights(Input_VL, []),
  create_NeuralInput(Inputs, [{Input_Id, Weights}|Acc]);
create_NeuralInput([], Acc) ->
  lists:reverse([{bias, random:uniform()-0.5}|Acc]).

create_NeuralWeights(0, Acc) ->
  Acc;
create_NeuralWeights(Index, Acc) ->
  % random weight from -0.5 to 0.5
  W = random:uniform()-0.5,
  create_NeuralWeights(Index-1, [W|Acc]).

generate_ids(0, Acc) ->
  Acc;
generate_ids(Index, Acc) ->
  Id = generate_id(),
  generate_ids(Index-1, [Id|Acc]).

generate_id() ->
  {MegaSeconds, Seconds, MicroSeconds} = now(), %note: now() monotonically increments using global lock so is unique
  1/(MegaSeconds*1000000 + Seconds + MicroSeconds/1000000).

create_Cortex(Cx_Id, S_Ids, A_Ids, N_Ids) ->
  #cortex{id = Cx_Id, sensor_ids = S_Ids, actuator_ids = A_Ids, neuron_ids = N_Ids}.







