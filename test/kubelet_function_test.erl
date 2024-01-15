%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2024, c50
%%% @doc
%%%
%%% @end
%%% Created :  5 Jan 2024 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(kubelet_function_test).

%% API
-export([start/0]).

-define(Adder,"adder").
-define(NumWorkers,5).
-define(TestDeployment,"test_c50").


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
    ok=create_cluster(),
    ok=stop_restart_node_test_1(),
    ok=add_applications(),
    ok=deploy_appl(),
    io:format("Test Suit succeded OK !!! ~p~n",[?MODULE]),
    ok.
    
%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
deploy_appl()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    {ok,HostName}=net:gethostname(),
    {ok,DeploymentList}=etcd_deployment:get_deployment_list(?TestDeployment),
    [
     {ok,"adder",'3_a@c50'},
     {ok,"adder",'4_a@c50'},
     {ok,"adder",'5_a@c50'},
     {ok,"divi",'2_a@c50'},
     {ok,"divi",'3_a@c50'}
    ]=lists:sort([kubelet:deploy_application(ApplicationId)||{ApplicationId,HostName}<-DeploymentList]),
    
    ApplicationList=[{N,rpc:call(N,application,which_applications,[],5000)}||N<-lists:sort(nodes())],
    io:format("ApplicationList ~p~n",[{ApplicationList,?MODULE,?FUNCTION_NAME,?LINE}]),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
add_applications()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    
    {ok,"adder",_Node}=kubelet:deploy_application(?Adder),
    
    {ok,AdderApp}=etcd_application:get_app(?Adder),
   
    NodeApplicationList=[{N,rpc:call(N,application,which_applications,[],5000)}||N<-lists:sort(nodes())],
    AdderNodeApplicationList=[{N,AppList}||{N,AppList}<-NodeApplicationList,
					   lists:keymember(AdderApp,1,AppList)],    
    io:format("NodeApplicationList ~p~n",[{NodeApplicationList,?MODULE,?FUNCTION_NAME,?LINE}]),
    io:format("AdderNodeApplicationList ~p~n",[{AdderNodeApplicationList,?MODULE,?FUNCTION_NAME,?LINE}]),

    %% kill application node
    [{NodeToKill,_}|_]=AdderNodeApplicationList,
    pong=rpc:call(NodeToKill,AdderApp,ping,[],5000),
    
    rpc:call(NodeToKill,init,stop,[],5000),
    timer:sleep(500),
    {badrpc,_}=rpc:call(NodeToKill,AdderApp,ping,[],5000),
    timer:sleep(5000),    

    [
     {adder,"An OTP application","0.1.0"},
     {kernel,"ERTS  CXC 138 10","8.2"},
     {log,"An OTP application","0.1.0"},
     {rd,"An OTP application","0.1.0"},
     {stdlib,"ERTS  CXC 138 10","3.17"}
    ]=lists:sort(rpc:call(NodeToKill,application,which_applications,[],5000)),
    
    pong=rpc:call(NodeToKill,AdderApp,ping,[],5000),


    ok.


%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
create_cluster()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    
    %% Test to deploy and get info before created workers
    {error,["No workers are created yet "]}=kubelet:which_workers(),
    {error,["No workers are created yet , you need to call create_workers(NumWorkers) and then it's possible to deploy ",?Adder]}=kubelet:deploy_application(?Adder),
    
    %% Create workers
    ok=kubelet:create_workers(?NumWorkers),
    ['1_a@c50','2_a@c50','3_a@c50','4_a@c50','5_a@c50']=lists:sort(nodes()),
    ['2_a@c50','3_a@c50','4_a@c50','5_a@c50','kubelet_a@c50']=lists:sort(rpc:call('1_a@c50',erlang,nodes,[],5000)),
    
    [
     [{kernel,"ERTS  CXC 138 10","8.2"},{log,"An OTP application","0.1.0"},{rd,"An OTP application","0.1.0"},{stdlib,"ERTS  CXC 138 10","3.17"}],
     [{kernel,"ERTS  CXC 138 10","8.2"},{log,"An OTP application","0.1.0"},{rd,"An OTP application","0.1.0"},{stdlib,"ERTS  CXC 138 10","3.17"}],
     [{kernel,"ERTS  CXC 138 10","8.2"},{log,"An OTP application","0.1.0"},{rd,"An OTP application","0.1.0"},{stdlib,"ERTS  CXC 138 10","3.17"}],
     [{kernel,"ERTS  CXC 138 10","8.2"},{log,"An OTP application","0.1.0"},{rd,"An OTP application","0.1.0"},{stdlib,"ERTS  CXC 138 10","3.17"}],
     [{kernel,"ERTS  CXC 138 10","8.2"},{log,"An OTP application","0.1.0"},{rd,"An OTP application","0.1.0"},{stdlib,"ERTS  CXC 138 10","3.17"}]     
    ]=[lists:sort(rpc:call(N,application,which_applications,[],5000))||N<-lists:sort(nodes())],
    
    


    ok.
%% --------------------------------------------------------------------
%% Function: available_hosts() 
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
stop_restart_node_test_1()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    [NodeToKill,'2_a@c50','3_a@c50','4_a@c50','5_a@c50']=lists:sort(nodes()),
    rpc:call(NodeToKill,init,stop,[],5000),
    timer:sleep(5000),
    [
     {kernel,"ERTS  CXC 138 10","8.2"},
     {log,"An OTP application","0.1.0"},
     {rd,"An OTP application","0.1.0"},
     {stdlib,"ERTS  CXC 138 10","3.17"}
    ]=lists:sort(rpc:call(NodeToKill,application,which_applications,[],5000)),
    ok.
