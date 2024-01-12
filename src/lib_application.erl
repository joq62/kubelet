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
deploy(ApplicationId,Candidate)->
    
    {ok,App}=etcd_application:get_app(ApplicationId),
    {ok,GitPath}=etcd_application:get_git_path(ApplicationId),
    ApplicationEvent=#{id=>ApplicationId,date_time=>{date(),time()},status=>started},
    ApplicationInfo2=#{application=>ApplicationId,app=>App,git_path=>GitPath,event=>ApplicationEvent}, 
    ApplicationInfo=maps:get(applications,Candidate),

    UpdatedCandidate=maps:put(applications,[ApplicationInfo2|ApplicationInfo],Candidate),    
    {ok,UpdatedCandidate}.
 



%%%===================================================================
%%% Internal functions
%%%===================================================================
