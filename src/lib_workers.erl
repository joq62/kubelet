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
    Node=node(),
    NodeName="glurk",
    {ok,NodeDir}=file:get_cwd(),
    WhichApplications=[App||{App,_,_}<-application:which_applications()],
    %#{id=>Id,date_time=>DateTime,state=>started_kubelet|started_workers
    WorkerNodeEvents=[#{id=>glurk_id,date_time=>{date(),time()},state=>started_workers}],
    WorkerNodesInfo=[#{node=>Node, nodename=>NodeName, node_dir=>NodeDir, applications=>WhichApplications, events=>WorkerNodeEvents}],
    {ok,WorkerNodesInfo}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
