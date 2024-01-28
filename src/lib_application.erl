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
	 deploy/2,
	 remove/3
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

    io:format("ApplicationId ~p~n",[{ApplicationId,?MODULE,?FUNCTION_NAME,?LINE}]),

    %% 1. check if applications is already started and there is a application dir
    %% 2. git clone the application 
    %% 3. on the worker node: add path to the application ebin and priv dir 
    %% 4. load and start the application , at each stage check 

     %% 1. check if applications is already started and there is a application dir 
    
    WorkerNodeName=maps:get(nodename,WorkerNodeInfo),
    WorkerDir=maps:get(node_dir,WorkerNodeInfo),
    ApplicationDir =lib_kubelet_cmn:application_dir(WorkerDir,ApplicationId),
    false=filelib:is_dir(ApplicationDir),
    
  %  {ok,App}=etcd_application:get_app(ApplicationId),
    {ok,App}=rd:call(etcd,etcd_application,get_app,[ApplicationId],5000),

    WorkerNode=maps:get(node,WorkerNodeInfo),
    {badrpc,_}=rpc:call(WorkerNode,App,ping,[],5000),

    %% 2. git clone the application 
    
    ok=file:make_dir(ApplicationDir),
 %   {ok,GitPath}=etcd_application:get_git_path(ApplicationId),
    {ok,GitPath}=rd:call(etcd,etcd_application,get_git_path,[ApplicationId],5000),

    GitResult=os:cmd("git clone "++GitPath++" "++ApplicationDir),
%    io:format("GitResult ~p~n",[{GitResult,?MODULE,?FUNCTION_NAME,?LINE}]),    
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
    ApplicationsMap=maps:get(applications,WorkerNodeInfo),
%    io:format("ApplicationsMap~p~n",[{ApplicationsMap,?MODULE,?LINE}]),
    UsortedApplicationsMap=case ApplicationsMap of
			       []->
				   [ApplicationInfo2];
			       _->
				   M1=[M||M<-ApplicationsMap,
					    App=/=maps:get(app,M)],
				   [ApplicationInfo2|M1]
			   end,
    UpdatedWorkerNodeInfo=maps:put(applications,UsortedApplicationsMap,WorkerNodeInfo),    
    {ok,UpdatedWorkerNodeInfo}.


%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
%% WorkerNode = #{node=>Node, nodename=>NodeName, node_dir=>NodeDir, applications=>[Application], events=>[WorkerNodeEvents]}
%% Application = #{application=>ApplName,app=>App,git_path=>GitPath,event=>ApplicationStatus} 
remove(ApplicationId,WorkerNode,WorkerNodeInfo)->

    io:format("ApplicationId,WorkerNode,WorkerNodeInfo ~p~n",[{ApplicationId,WorkerNode,WorkerNodeInfo,?MODULE,?FUNCTION_NAME,?LINE}]),

    %% 1. Stop and unload Application and delete code path
    %% 2. Delete Application Dir and stop and unload
    %% 3. Update NodeInfo
  
    %% 1. Stop and unload Application and delete code path
    {ok,App}=rd:call(etcd,etcd_application,get_app,[ApplicationId],5000),
    ok=rpc:call(WorkerNode,application,stop,[App],5000),
    ok=rpc:call(WorkerNode,application,unload,[App],5000),

    WorkerNodeName=maps:get(nodename,WorkerNodeInfo),
    WorkerDir=maps:get(node_dir,WorkerNodeInfo),
    ApplicationDir =lib_kubelet_cmn:application_dir(WorkerDir,ApplicationId),
    Ebin=filename:join(ApplicationDir,"ebin"),
    PrivDir=filename:join(ApplicationDir,"priv"),
    DelPatha=case filelib:is_dir(PrivDir) of
		 false->
		     [Ebin];
		 true->
		     [Ebin,PrivDir]
	     end,
    [true]=[rpc:call(WorkerNode,code,del_path,[Path],5000)||Path<-DelPatha],
    
    %% 2. Delete Application Dir and stop and unload

    file:del_dir_r(ApplicationDir),
    false=filelib:is_dir(ApplicationDir),
    
    %% 3. Update NodeInfo
    ApplicationEvent=#{id=>ApplicationId,date_time=>{date(),time()},status=>removed},
    ApplicationInfo2=#{application=>ApplicationId,app=>not_applicable,git_path=>not_applicable,event=>ApplicationEvent}, 
    ApplicationsMap=maps:get(applications,WorkerNodeInfo),
%    io:format("ApplicationsMap~p~n",[{ApplicationsMap,?MODULE,?LINE}]),
    UsortedApplicationsMap=case ApplicationsMap of
			       []->
				   [ApplicationInfo2];
			       _->
				   M1=[M||M<-ApplicationsMap,
					    App=/=maps:get(app,M)],
				   [ApplicationInfo2|M1]
			   end,
    UpdatedWorkerNodeInfo=maps:put(applications,UsortedApplicationsMap,WorkerNodeInfo),    
    {ok,UpdatedWorkerNodeInfo}.

 



%%%===================================================================
%%% Internal functions
%%%===================================================================
