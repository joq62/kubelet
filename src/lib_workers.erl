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
-export([
	 
	 get_candidate/1,
	 get_candidate/2,
	 create_workers/1,
	 new_worker/1,
	 update_worker_info/2
	]).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
update_worker_info(WorkeInfo,WorkerList)->
    WorkerNode=maps:get(node,WorkeInfo),
    L1=[M||M<-WorkerList,
	   WorkerNode=/=maps:get(node,M)],
    [WorkeInfo|L1].

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
get_candidate(WorkerNodeInfoList,ApplicationId)->
    Result=case get_candidates(WorkerNodeInfoList,ApplicationId,[]) of
	       []->
		   {error,["No candidates are available, use another Host",?MODULE,?LINE]};
	       [Candidate|_]->
		   {ok,Candidate}
	   end,
    Result.

get_candidates([],_,Acc)->
    L1=lists:keysort(1,Acc),
    [M||{_,M}<-L1];
get_candidates([WorkerNodeInfo|T],ApplicationId,Acc)->
    ApplicationMaps=maps:get(applications,WorkerNodeInfo),
    Member=[Map||Map<-ApplicationMaps,
		 lists:member(ApplicationId,maps:values(Map))],
    case Member of
	[]->
	    NewAcc=[{ApplicationMaps,WorkerNodeInfo}|Acc];
	_ ->
	    NewAcc=Acc
    end,
    get_candidates(T,ApplicationId,NewAcc).
	
    
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
get_candidate(WorkerNodeInfoList)->
    L1=[{maps:get(applications,M),M}||M<-WorkerNodeInfoList],
    L2=lists:keysort(1,L1),
    [Candidate|_]=[M||{_,M}<-L2],
    {ok,Candidate}.
    
    


%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
create_workers(NumWorkers)->
    WorkerNodeNames=worker_node_names(NumWorkers),
    WorkerNodeInfoList=new_workers(WorkerNodeNames),
    {ok,WorkerNodeInfoList}.

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
new_worker(WorkerNodeName)->
    {ok,HostName}=net:gethostname(),
    CookieStr=lib_kubelet_cmn:cookie_str(),    
    WorkerDir=lib_kubelet_cmn:worker_dir(WorkerNodeName),
    file:del_dir_r(WorkerDir),	
    ok=file:make_dir(WorkerDir),
    {ok,WorkerNode}=slave:start(HostName,WorkerNodeName,"-setcookie "++CookieStr),
    pong=net_adm:ping(WorkerNode),
    timer:sleep(500),
    true=erlang:monitor_node(WorkerNode,true),
    {ok,WorkerNode,WorkerDir,WorkerNodeName}.


%%%===================================================================
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
    {ok,UpdatedWorkerNodeInfo}=lib_kubelet:start_infra_applications(WorkerNodeInfo),
    new_workers(T,[UpdatedWorkerNodeInfo|Acc]).


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
