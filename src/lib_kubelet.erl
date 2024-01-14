%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2024, c50
%%% @doc
%%%
%%% @end
%%% Created : 14 Jan 2024 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(lib_kubelet).

-include("kubelet.hrl").

%% API
-export([
	 restart/2
	]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
restart(WorkerNode,WorkerNodeList)->
    [WorkerNodeInfo]=[R||R<-WorkerNodeList,
			 WorkerNode=:=maps:get(node,R)],
    {ok,UpdatedWorkerNodeInfo1}=restart_node(WorkerNodeInfo),
    {ok,UpdatedWorkerNodeInfo2}=restart_infra_applications(UpdatedWorkerNodeInfo1),
    {ok,UpdatedWorkerNodeInfo3}=restart_service_applications(UpdatedWorkerNodeInfo2),

    WorkerNode=maps:get(node,WorkerNodeInfo),
    WorkerNodeName=maps:get(nodename,WorkerNodeInfo),
    pang=net_adm:ping(WorkerNode),   
    {ok,WorkerNode,_WorkerDir,WorkerNodeName}=lib_workers:new_worker(WorkerNodeName),
    pong=net_adm:ping(WorkerNode),
    NewWorkerEvent=#{id=>WorkerNode,date_time=>{date(),time()},state=>restarted_worker},
    
    WorkerEvents=[NewWorkerEvent|maps:get(events,WorkerNodeInfo)],
    UpdatedWorkerNodeInfo=maps:put(events,WorkerEvents,WorkerNodeInfo),
    
    %% Deploy infra applications
    

    %% Daploy service applications
    
    {ok,UpdatedWorkerNodeInfo}.
%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
restart_node(WorkerNodeInfo)->
    WorkerNode=maps:get(node,WorkerNodeInfo),
    WorkerNodeName=maps:get(nodename,WorkerNodeInfo),
    pang=net_adm:ping(WorkerNode),   
    {ok,WorkerNode,_WorkerDir,WorkerNodeName}=lib_workers:new_worker(WorkerNodeName),
    pong=net_adm:ping(WorkerNode),
    NewWorkerEvent=#{id=>WorkerNode,date_time=>{date(),time()},state=>restarted_worker},
    WorkerEvents=[NewWorkerEvent|maps:get(events,WorkerNodeInfo)],
    UpdatedWorkerNodeInfo=maps:put(events,WorkerEvents,WorkerNodeInfo),
    {ok,UpdatedWorkerNodeInfo}.
    
%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
restart_infra_applications(WorkerNodeInfo)->
    WorkerNode=maps:get(node,WorkerNodeInfo),
    UpdatedWorkerNodeInfo=a,
  %  fortsätt här med att starta fler applicationer
    {ok,UpdatedWorkerNodeInfo}.

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
restart_service_applications(WorkerNodeInfo)->
    WorkerNode=maps:get(node,WorkerNodeInfo),
    UpdatedWorkerNodeInfo=a,
  %  fortsätt här med att starta fler applicationer
    {ok,UpdatedWorkerNodeInfo}.
