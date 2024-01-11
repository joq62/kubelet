%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2024, c50
%%% @doc
%%%
%%% @end
%%% Created : 11 Jan 2024 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(lib_workers).

%% API
-export([create_workers/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
create_workers(NumWorkers)->
    WorkerNodeNames=worker_node_names(NumWorkers),
    WorkerNodeInfoList=new_workers(WorkerNodeNames),
    {ok,WorkerNodeInfoList}.

%%% Internal functions
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
new_workers(WorkerNodeNames)->
    new_workers(WorkerNodeNames,[]).
new_workers([],WorkerNodeInfoList)-> 
    WorkerNodeInfoList;
new_workers([WorkerNodeName|T],Acc) ->
    {ok,WorkerNode,WorkerDir,WorkerNodeName}=new_worker(WorkerNodeName),
    WorkerNodeEvents=[#{id=>WorkerNode,date_time=>{date(),time()},state=>started_worker}],
    WorkerNodeInfo=#{node=>WorkerNode, nodename=>WorkerNodeName, node_dir=>WorkerDir, applications=>[], events=>WorkerNodeEvents},
    new_workers(T,[WorkerNodeInfo|Acc]).

new_worker(WorkerNodeName)->
    {ok,HostName}=net:gethostname(),
    CookieStr=lib_kubelet_cmn:cookie_str(),    
    WorkerDir=lib_kubelet_cmn:worker_dir(WorkerNodeName),
    file:del_dir_r(WorkerDir),
    ok=file:make_dir(WorkerDir),

    {ok,WorkerNode}=slave:start(HostName,WorkerNodeName,"-setcookie "++CookieStr),
    pong=net_adm:ping(WorkerNode),
    timer:sleep(500),
    {ok,WorkerNode,WorkerDir,WorkerNodeName}.

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
worker_node_names(NumWorkers)->
    worker_node_names(NumWorkers,[]).
worker_node_names(0,NodeNames)->
    NodeNames;
worker_node_names(N,Acc) ->
    NodeName=worker_node_name(N),
    worker_node_names(N-1,[NodeName|Acc]).

worker_node_name(N)->
    CookieStr=lib_kubelet_cmn:cookie_str(),    
    NStr=integer_to_list(N),
    NStr++"_"++CookieStr.
