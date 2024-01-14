%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
% Kubelet is the prime node for the cluster at the each node
% --------------- Functionality ----------------------- 
% - create , monitor and delete worker nodes
% - create and delete worker nodes directories
% - load, start , stop, unload and monitor applications on worker nodes
% - monitor worker node is using erlang:monitor_node(Node) and handles
%   the signal {nodedown,Node}. 
%   Actions when detecting the  signal {nodedown,Node}
%   1. Delete and create worker node Node's directory 
%   2. Start and monitor  worker node Node
%   3. Check which applications Applications that were allocated on the Node
%   4. Load and start Applications
%
% ------------------- Data ---------------------
%
%  WorkerNode = #{node=>Node, nodename=>NodeName, node_dir=>NodeDir, applications=>[Application], events=>[WorkerNodeEvents]}
%  Application = #{application=>ApplName,app=>App,git_path=>GitPath,event=>ApplicationStatus} 
%  WorkerNodeEvent=#{id=>Id,date_time=>DateTime,state=>started_kubelet|started_workers
%  ApplicationEvent= #{id=>Id,date_time=>DateTime,state=>loaded|started|stopped|unloaded|restarted|schduled
%
%  
% ------------------ State ----------------------
% started_kubelet: kubelete node is started and kubelet node dir is createde 
% started_workers: Acted on signal create(NumWorkerNodes), started NumberWorkerNodes worker nodes
%        and corresponding dirs. 
% 
%   
%%% @end
%%% Created : 06 JAn 2024 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(kubelet).

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include 
%%
%%--------------------------------------------------------------------
-include("kubelet.resource_discovery").
-include("log.api").

%% API

-export([
	 create_workers/1,
	 which_workers/0,
	 deploy_application/1,
	 delete_application/1,
	 which_applications/0
	]).
-export([
	 ping/0,
	 start/0,
	 stop/0
	]).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-record(state, {
		worker_node_info
	       }).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Creates NumWorkers worker nodes in the kubelet_node dir 
%% @end
%%--------------------------------------------------------------------
-spec create_workers(NumWorkers :: integer())-> ok|{error,Reason :: term()}.
create_workers(NumWorkers)-> 
    gen_server:call(?SERVER, {create_workers,NumWorkers},infinity).
%%--------------------------------------------------------------------
%% @doc
%% Returns list of running Worker nodes 
%% @end
%%--------------------------------------------------------------------
-spec which_workers()-> WorkerNodesInfo :: term().
which_workers()-> 
    gen_server:call(?SERVER, {which_workers},infinity).
%%--------------------------------------------------------------------
%% @doc
%% Load and start the application ApplicationId on the worker node that has least applications 
%% @end
%%--------------------------------------------------------------------
-spec deploy_application(ApplicationId :: string())-> {ok,{ApplicationId :: string(),WorkerNode :: node()}}|{error,Reason :: term()}.
deploy_application(ApplicationId)-> 
    gen_server:call(?SERVER, {deploy_application,ApplicationId},infinity).

%%--------------------------------------------------------------------
%% @doc
%% Returns list of running applications on all worker nodes 
%% @end
%%--------------------------------------------------------------------
-spec which_applications()-> [{ApplicationId :: string(),WorkerNode :: node()}].
which_applications()-> 
    gen_server:call(?SERVER, {which_applications},infinity).

%%--------------------------------------------------------------------
%% @doc
%% Load and start the application ApplicationId on the worker node that has least applications 
%% @end
%%--------------------------------------------------------------------
-spec delete_application({ApplicationId :: string(),WorkerNode :: node()})-> ok|{error,Reason :: term()}.
delete_application({ApplicationId,WorkerNode})-> 
    gen_server:call(?SERVER, {delete_application,ApplicationId,WorkerNode},infinity).

%%--------------------------------------------------------------------
%% @doc
%%  
%% @end
%%--------------------------------------------------------------------
start()->
    application:start(?MODULE).
%%--------------------------------------------------------------------
%% @doc
%% Check if a gen_server is running
%% @end
%%--------------------------------------------------------------------
-spec ping()-> pong|{error,Reason :: term()}.
ping()-> 
    gen_server:call(?SERVER, {ping},infinity).
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
	  {error, Error :: {already_started, pid()}} |
	  {error, Error :: term()} |
	  ignore.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


stop()-> gen_server:call(?SERVER, {stop},infinity).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
	  {ok, State :: term(), Timeout :: timeout()} |
	  {ok, State :: term(), hibernate} |
	  {stop, Reason :: term()} |
	  ignore.

init([]) ->
    [rd:add_local_resource(ResourceType,Resource)||{ResourceType,Resource}<-?LocalResourceTuples],
    [rd:add_target_resource_type(TargetType)||TargetType<-?TargetTypes],
    rd:trade_resources(),
      
    KubeletDir=lib_kubelet_cmn:kubelet_dir(), 
    file:del_dir_r(KubeletDir),
    ok=file:make_dir(KubeletDir),
    
    ?LOG_NOTICE("Server started ",[?MODULE]),
    
    {ok, #state{}}.


%%--------------------------------------------------------------------
%% @doc
%%  
%% @end
%%--------------------------------------------------------------------
handle_call({create_workers,NumWorkers}, _From, State) when State#state.worker_node_info == undefined->
    Result=try lib_workers:create_workers(NumWorkers) of
	       {ok,WorkerNodesInfo}->
		   {ok,WorkerNodesInfo}
	   catch
	       error:Reason:Stacktrace->
		   {error,Reason,Stacktrace,?MODULE,?LINE};
	       throw:Reason:Stacktrace->
		   {throw,Reason,Stacktrace,?MODULE,?LINE};
	       Event:Reason:Stacktrace ->
		   {Event,Reason,Stacktrace,?MODULE,?LINE}
	   end,
    Reply=case Result of
	      {ok,Info}->
		  NewState=State#state{worker_node_info = Info},
		  ok;
	      ErrorEvent->
		  NewState=State,
		  {error,ErrorEvent}
	  end,
    
    {reply, Reply, NewState};

handle_call({create_workers,_NumWorkers}, _From, State) when State#state.worker_node_info =/= undefined->
    Reply={error,["Already created ",State#state.worker_node_info]},
    {reply, Reply, State};

%%--------------------------------------------------------------------
%% @doc
%%  
%% @end
%%--------------------------------------------------------------------
handle_call({which_workers}, _From, State) when State#state.worker_node_info =/= undefined->
    Reply={ok,State#state.worker_node_info},
    {reply, Reply, State};

handle_call({which_workers}, _From, State) when State#state.worker_node_info == undefined->
    Reply={error,["No workers are created yet "]},
    {reply, Reply, State};


%%--------------------------------------------------------------------
%% @doc
%%  
%% @end
%%--------------------------------------------------------------------

handle_call({deploy_application,ApplicationId}, _From, State) when State#state.worker_node_info =/= undefined->
    Result=case lib_workers:get_candidate(State#state.worker_node_info,ApplicationId) of
	       {error,Reason}->
		   {error,Reason};
	       {ok,Candidate}->
		   try lib_application:deploy(ApplicationId,Candidate) of
		       {ok,UpdatedCandidateInfo}->
			   {ok,UpdatedCandidateInfo}
		   catch
		       error:Reason:Stacktrace->
			   {error,Reason,Stacktrace,?MODULE,?LINE};
		       throw:Reason:Stacktrace->
			   {throw,Reason,Stacktrace,?MODULE,?LINE};
		       Event:Reason:Stacktrace ->
			   {Event,Reason,Stacktrace,?MODULE,?LINE}
		   end
	   end,
    Reply=case Result of
	      {ok,WorkerInfo}->
		  NewWorkerNodeInfo=lib_workers:update_worker_info(WorkerInfo,State#state.worker_node_info),
		  NewState=State#state{worker_node_info = NewWorkerNodeInfo},
		  ok;
	      ErrorEvent->
		  NewState=State,
		  {error,ErrorEvent}
	  end,
    {reply, Reply, NewState};  


handle_call({deploy_application,ApplicationId}, _From, State) when State#state.worker_node_info == undefined->
    Reply={error,["No workers are created yet , you need to call create_workers(NumWorkers) and then it's possible to deploy ",ApplicationId]},
    {reply, Reply, State};

%%--------------------------------------------------------------------
%% @doc
%%  
%% @end
%%--------------------------------------------------------------------

handle_call({delete_application,ApplicationId,WorkerNode}, _From, State) when State#state.worker_node_info == undefined->
    Reply={error,["No workers are created yet , you need to call create_workers(NumWorkers) and then it's possible to deploy ",ApplicationId,WorkerNode]},
    {reply, Reply, State};
  
%%--------------------------------------------------------------------
%% @doc
%%  
%% @end
%%--------------------------------------------------------------------

handle_call({which_applications}, _From, State)  when State#state.worker_node_info == undefined->
    Reply={error,["No workers are created yet , you need to call create_workers(NumWorkers) "]},
    {reply, Reply, State};

handle_call({ping}, _From, State) ->
    Reply=pong,
    {reply, Reply, State};

handle_call(UnMatchedSignal, From, State) ->
    io:format("unmatched_signal ~p~n",[{UnMatchedSignal, From,?MODULE,?LINE}]),
    Reply = {error,[unmatched_signal,UnMatchedSignal, From]},
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
handle_cast(UnMatchedSignal, State) ->
    io:format("unmatched_signal ~p~n",[{UnMatchedSignal,?MODULE,?LINE}]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: normal | term(), NewState :: term()}.

handle_info({nodedown,Node}, State) ->
    [WorkerNodeInfo]=[R||R<-State#state.worker_node_info,
			 Node=:=maps:get(node,R)],
    Result=try lib_kubelet:restart(WorkerNodeInfo) of
	       {ok,UpdatedCandidateInfo}->
		   {ok,UpdatedCandidateInfo}
	   catch
	       error:Reason:Stacktrace->
		   {error,Reason,Stacktrace,?MODULE,?LINE};
	       throw:Reason:Stacktrace->
		   {throw,Reason,Stacktrace,?MODULE,?LINE};
	       Event:Reason:Stacktrace ->
		   {Event,Reason,Stacktrace,?MODULE,?LINE}
	   end,
    case Result of
	{ok,WorkerInfo}->
	    NewWorkerNodeInfo=lib_workers:update_worker_info(WorkerInfo,State#state.worker_node_info),
	    NewState=State#state{worker_node_info = NewWorkerNodeInfo},
	    ok;
	ErrorEvent->
	    NewState=State,
	    {error,ErrorEvent}
    end,
    
    {noreply, NewState};

handle_info(Info, State) ->
    ?LOG_WARNING("Unmatched signal ",[Info]),	  
    io:format("unmatched_signal ~p~n",[{Info,?MODULE,?LINE}]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
		State :: term()) -> any().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
		  State :: term(),
		  Extra :: term()) -> {ok, NewState :: term()} |
	  {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
		    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================

