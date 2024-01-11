%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2024, c50
%%% @doc
%%%
%%% @end
%%% Created :  5 Jan 2024 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(worker_test).

%% API
-export([start/0]).


-define(NumWorkers,5).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    ok=create_which_tests(),
    
    io:format("Test Suit succeded OK !!! ~p~n",[?MODULE]),
    ok.
    
%%%===================================================================
%%% Internal functions
%%%===================================================================
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
create_which_tests()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),

    %% Normal 
    ok=kubelet:create_workers(?NumWorkers),
  %  {ok,
  %   [#{applications=>[kubelet,etcd,mnesia,rd,log,stdlib,kernel],
%	events=>[#{date_time=>{_,_},id=>glurk_id,state=>started_workers}],
%	node=>'kubelet_a@c50',
%	node_dir=>"/home/joq62/erlang/joq_kube/kubelet",
%	nodename=>"glurk"}
 %    ]
    {ok,WorkerNodesInfo}=kubelet:which_workers(),
    {error,["Already created ",WorkerNodesInfo]}=kubelet:create_workers(?NumWorkers),



    ok.
