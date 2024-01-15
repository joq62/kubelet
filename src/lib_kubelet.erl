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
	 restart/2,
	 start_infra_applications/1
	]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
start_infra_applications(WorkerNodeInfo)->
    {ok,UpdatedWorkerNodeInfo}=restart_infra_applications(WorkerNodeInfo),
    {ok,UpdatedWorkerNodeInfo}.

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
%    io:format("restart_services ~p~n",[{UpdatedWorkerNodeInfo3,?MODULE,?LINE}]),
    
    {ok,UpdatedWorkerNodeInfo3}.

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
    UpdatedWorkerNodeInfo=deploy(?InfraApplications,WorkerNodeInfo),
    {ok,UpdatedWorkerNodeInfo}.

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
restart_service_applications(WorkerNodeInfo)->
    ApplicationInfo=maps:get(applications,WorkerNodeInfo),
    AllApplicationIds=[maps:get(application,Map)||Map<-ApplicationInfo],
    ServiceIds=[ApplicationId||ApplicationId<-AllApplicationIds,
			       false=:=lists:member(ApplicationId,?InfraApplications)],
    UpdatedWorkerNodeInfo=deploy(ServiceIds,WorkerNodeInfo),
    {ok,UpdatedWorkerNodeInfo}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

deploy([],WorkerNodeInfo)->
    WorkerNodeInfo;
deploy([ApplicationId|T],WorkerNodeInfo)->
    {ok,NewWorkerNodeInfo}=lib_application:deploy(ApplicationId,WorkerNodeInfo),
    deploy(T,NewWorkerNodeInfo).    
