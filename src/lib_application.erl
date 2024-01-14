%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2024, c50
%%% @doc
%%%
%%% @end
%%% Created : 11 Jan 2024 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(lib_application).

%% API
-export([
	 deploy/2
	]).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
%% WorkerNode = #{node=>Node, nodename=>NodeName, node_dir=>NodeDir, applications=>[Application], events=>[WorkerNodeEvents]}
%% Application = #{application=>ApplName,app=>App,git_path=>GitPath,event=>ApplicationStatus} 
deploy(ApplicationId,WorkerNodeInfo)->

    %% 1. check if applications is already started and there is a application dir
    %% 2. git clone the application 
    %% 3. on the worker node: add path to the application ebin and priv dir 
    %% 4. load and start the application , at each stage check 

     %% 1. check if applications is already started and there is a application dir 
    
    WorkerNodeName=maps:get(nodename,WorkerNodeInfo),
    WorkerDir=maps:get(node_dir,WorkerNodeInfo),
    ApplicationDir =lib_kubelet_cmn:application_dir(WorkerDir,ApplicationId),
  %  io:format("ApplicationDir ~p~n",[{ApplicationDir,?MODULE,?FUNCTION_NAME,?LINE}]),    

    false=filelib:is_dir(ApplicationDir),
    
    {ok,App}=etcd_application:get_app(ApplicationId),
    WorkerNode=maps:get(node,WorkerNodeInfo),
    {badrpc,_}=rpc:call(WorkerNode,App,ping,[],5000),

    %% 2. git clone the application 
    
    ok=file:make_dir(ApplicationDir),
    {ok,GitPath}=etcd_application:get_git_path(ApplicationId),
    GitResult=os:cmd("git clone "++GitPath++" "++ApplicationDir),
    io:format("GitResult ~p~n",[{GitResult,?MODULE,?FUNCTION_NAME,?LINE}]),    
    Ebin=filename:join(ApplicationDir,"ebin"),
    true=filelib:is_dir(Ebin),
    PrivDir=filename:join(ApplicationDir,"priv"),
    AddPatha=case filelib:is_dir(PrivDir) of
		 false->
		     [Ebin];
		 true->
		     [Ebin,PrivDir]
	     end,
    %% 3. on the worker node: add path to the application ebin and priv dir  
    
    ok=rpc:call(WorkerNode,code,add_pathsa,[AddPatha],5000),
     
    
    %% 4. load and start the application , at each stage check 
    
    ok=rpc:call(WorkerNode,application,load,[App],5000),
    ok=rpc:call(WorkerNode,application,start,[App],5000),
    
    ApplicationEvent=#{id=>ApplicationId,date_time=>{date(),time()},status=>started},
    ApplicationInfo2=#{application=>ApplicationId,app=>App,git_path=>GitPath,event=>ApplicationEvent}, 
    ApplicationInfo=maps:get(applications,WorkerNodeInfo),

    UpdatedWorkerNodeInfo=maps:put(applications,[ApplicationInfo2|ApplicationInfo],WorkerNodeInfo),    
    {ok,UpdatedWorkerNodeInfo}.
 



%%%===================================================================
%%% Internal functions
%%%===================================================================
