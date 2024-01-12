%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2024, c50
%%% @doc
%%%
%%% @end
%%% Created : 11 Jan 2024 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(lib_kubelet_cmn).

%% API
-export([cookie_str/0,
	 kubelet_nodename/0,
	 kubelet_dir/0,
	 worker_node/1,
	 worker_dir/1,
	 application_dir/2
	 
	]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
cookie_str()->
    atom_to_list(erlang:get_cookie()).
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
kubelet_nodename()->
    [KubeletNodeName,_HostName]=string:tokens(atom_to_list(node()),"@"),
    KubeletNodeName.    

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
kubelet_dir()->
    KubeletNodeName=kubelet_nodename(),
    KubeletNodeName++".kubelet_dir".

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
worker_node(WorkerNodeName)->
    {ok,HostName}=net:gethostname(),
    list_to_atom(WorkerNodeName++"_"++"worker"++"@"++HostName).

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
worker_dir(WorkerNodeName)->
    KubeletDir=kubelet_dir(),
    Dir=WorkerNodeName++"."++"worker_dir",
    filename:join(KubeletDir,Dir).
   
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
application_dir(WorkerDir,ApplicationId)->
    Dir=ApplicationId++"."++"application_dir",
    filename:join(WorkerDir,Dir).
   

%%%===================================================================
%%% Internal functions
%%%===================================================================
