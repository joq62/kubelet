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
    ok=common_tests(),
    ok=create_which_tests(),
    ok=check_connections(),

    ok=load_start_infra_test(),
    ok=stop_restart_node_test(),
    
    io:format("Test Suit succeded OK !!! ~p~n",[?MODULE]),
    ok.
    
%%%===================================================================
%%% Internal functions
%%%===================================================================
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% ----------------------
 load_start_infra_test()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    ApplicationId="resource_discovery",
    %%% 

    {ok,WorkerNodeInfoList}=kubelet:which_workers(),
    {ok,Candidate}=lib_workers:get_candidate(WorkerNodeInfoList,ApplicationId),
        
    E1=lists:append([maps:to_list(M)||M<-maps:get(events,Candidate)]),
    [{date_time,_},{id,'1_a@c50'},{state,started_worker}]=lists:sort(E1),

    [{applications,[]},
     {events,_},
     {node,'1_a@c50'},
     {node_dir,"kubelet_a.kubelet_dir/1_a.worker_dir"},
     {nodename,"1_a"}
    ]=lists:sort(maps:to_list(Candidate)),
    
    
    %% ---test deployment
    deploy(6,ApplicationId),
    [io:format("ping N ~p~n",[[{N,rpc:call(N,rd,ping,[],5000)}||N<-lists:sort(nodes())]])],
    
    ok.


deploy(0,_)->
    {ok,NewWorkerNodeInfoList}=kubelet:which_workers();
 %   io:format("NewWorkerNodeInfoList ~p~n",[{NewWorkerNodeInfoList,?MODULE,?FUNCTION_NAME,?LINE}]);

deploy(N,ApplicationId) ->
    io:format("N, kubelet:deploy_application(ApplicationId)  ~p~n",[{N,kubelet:deploy_application(ApplicationId),?MODULE,?FUNCTION_NAME,?LINE}]),
    deploy(N-1,ApplicationId).

%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
stop_restart_node_test()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    
    [NodeToKill,'2_a@c50','3_a@c50','4_a@c50','5_a@c50']=lists:sort(nodes()),
    rpc:call(NodeToKill,init,stop,[],5000),
    timer:sleep(3000),
    [NodeToKill,'2_a@c50','3_a@c50','4_a@c50','5_a@c50']=lists:sort(nodes()),
    
 
    ok.
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
check_connections()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    
    ['1_a@c50','2_a@c50','3_a@c50','4_a@c50','5_a@c50']=lists:sort(nodes()),
    ['2_a@c50','3_a@c50','4_a@c50','5_a@c50','kubelet_a@c50']=lists:sort(rpc:call('1_a@c50',erlang,nodes,[],5000)),
    ok.

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
common_tests()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    
    WorkerNodeName="1_a",
    ApplicationId="apollo_11",

    "a"=lib_kubelet_cmn:cookie_str(),    
    "kubelet_a"=lib_kubelet_cmn:kubelet_nodename(),    
    "kubelet_a.kubelet_dir"=lib_kubelet_cmn:kubelet_dir(),    
    '1_a_worker@c50'=lib_kubelet_cmn:worker_node(WorkerNodeName),    
    WorkerDir=lib_kubelet_cmn:worker_dir(WorkerNodeName), 
    "kubelet_a.kubelet_dir/1_a.worker_dir"=WorkerDir,
    "kubelet_a.kubelet_dir/1_a.worker_dir/apollo_11.application_dir"=lib_kubelet_cmn:application_dir(WorkerDir,ApplicationId),

    ok.


%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
create_which_tests()->
      io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
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
