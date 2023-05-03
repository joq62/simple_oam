%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc: : 
%%% Created :
%%% Node end point  
%%% Creates and deletes Pods
%%% 
%%% API-kube: Interface 
%%% Pod consits beams from all services, app and app and sup erl.
%%% The setup of envs is
%%% -------------------------------------------------------------------
-module(all).      
 
-export([start/0]).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-define(DbEtcdNode,'dbetcd@c50').
-define(TestHosts,["c200","c201"]).
-define(File,"test/production.deployment").
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start()->
   
    ok=setup(),
    [ok,ok]=[oam:stop_controller(HostSpec)||HostSpec<-?TestHosts],

%    ok=stop_start_controllers(),    

  %  ok=load_start_provider("adder","c200"),    
  %  ok=stop_unload_provider("adder","c200"),  
            
 %   ok=sim_orch(),
    
   {
    [{ok,"c200"},{ok,"c201"}],
    [{ok,"test_appl","c201"},
     {ok,"test_appl","c200"},
     {ok,"divi","c201"},
     {ok,"divi","c200"},
     {ok,"dbetcd_appl","c201"},
     {ok,"dbetcd_appl","c200"},
     {ok,"adder","c200"}
    ]
   }=orch(),

    ok=test_controller_kill(),
    ok=test_provider_kill(),
    
    {[],[]}=orch(),

  %  ok=simple_deployment_file_3(),
    % ok=simple_deployment_file(),

    [ok,ok]=[oam:stop_controller(HostSpec)||HostSpec<-?TestHosts],
    [false,false]=[oam:is_controller_started(HostSpec)||HostSpec<-?TestHosts],

    io:format("Test OK !!! ~p~n",[?MODULE]),
    timer:sleep(2000),
  %  init:stop(),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
test_provider_kill()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    
    io:format("Stop  ~p~n",[{"divi","c200","test_appl","c201",?MODULE,?FUNCTION_NAME,?LINE}]),
    ok=kube:stop_provider("divi","c200"),
    ok=kube:unload_provider("divi","c200"),
    ok=kube:stop_provider("test_appl","c201"),
    {
     [],
     [{ok,"test_appl","c201"},{ok,"divi","c200"}]
    }=orch(),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
test_controller_kill()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    
    io:format("Stop c200 ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    ok=oam:stop_controller("c200"),
    {
     [{ok,"c200"}],
     [{ok,"test_appl","c200"},{ok,"divi","c200"},{ok,"dbetcd_appl","c200"},{ok,"adder","c200"}]
    }=orch(),

   %%
    io:format("Stop c201 ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    ok=oam:stop_controller("c201"),
    {
     [{ok,"c201"}],
     [{ok,"test_appl","c201"},{ok,"divi","c201"},{ok,"dbetcd_appl","c201"}]
    }=orch(),
    

    ok.


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
orch()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    
    {ok,WantedState}=kube:wanted_state_from_file(?File),
    ["c200","c201"]=WantedHostSpecs=list_rm_duplicates(WantedState),
   
    %% check and try to start missing controllers
    WantedState_Controllers=WantedHostSpecs,
    MissingControlles=[HostSpec||HostSpec<-WantedState_Controllers,
				 false==kube:is_controller_started(HostSpec)],
    StartControllers=[{kube:start_controller(HostSpec),HostSpec}||HostSpec<-MissingControlles],
  %  io:format("StartControllers ~p~n",[{StartControllers,?MODULE,?FUNCTION_NAME,?LINE}]),

    %% check and try to start missing providers
    io:format("check and try to start missing providers ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
   
    StartMissingProviders=start_missing(lists:sort(WantedState),[]),
 %  io:format("StartMissingProviders ~p~n",[{StartMissingProviders,?MODULE,?FUNCTION_NAME,?LINE}]),
    {StartControllers,StartMissingProviders}.
    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

list_rm_duplicates(L)->
    list_rm_duplicates(L,[]).
list_rm_duplicates([],Acc)->
    Acc;
list_rm_duplicates([{_,HostSpec,_}|T],Acc)->
    NewAcc=case lists:member(HostSpec,Acc) of
	       true->
		   Acc;
	       false ->
		   [HostSpec|Acc]
	   end,
    list_rm_duplicates(T,NewAcc).
    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
sim_orch()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),

    %% check and try to start missing controllers
    io:format("check and try to start missing controllers ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    WantedState_Controllers=?TestHosts,
    MissingControlles=[HostSpec||HostSpec<-WantedState_Controllers,
				 false==kube:is_controller_started(HostSpec)],
    
    ["c200","c201"]=MissingControlles,
    StartControllers=[{kube:start_controller(HostSpec),HostSpec}||HostSpec<-MissingControlles],
    [{ok,"c200"},{ok,"c201"}]=StartControllers,
    
     %% check and try to start missing providers
    io:format("check and try to start missing providers ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    {ok,WantedState}=kube:wanted_state_from_file(?File),
    [{"adder","c200",adder},{"divi","c200",divi},{"divi","c201",divi},
     {"test_appl","c200",test_appl},{"test_appl","c201",test_appl}
    ]=lists:sort(WantedState),
    
    [{"adder","c200",adder},{"divi","c200",divi},{"divi","c201",divi},
     {"test_appl","c200",test_appl},{"test_appl","c201",test_appl}
    ]=[{ProviderSpec,HostSpec,App}||{ProviderSpec,HostSpec,App}<-lists:sort(WantedState),
					   pong=/=sd:call(App,App,ping,[],5000)],
    StartMissingProviders=start_missing(lists:sort(WantedState),[]),

    []=[{ProviderSpec,HostSpec,App}||{ProviderSpec,HostSpec,App}<-lists:sort(WantedState),
				      pong=/=sd:call(App,App,ping,[],5000)],
    [{ok,"adder","c200"},{ok,"divi","c200"},{ok,"divi","c201"},{ok,"test_appl","c200"},{ok,"test_appl","c201"}]=StartMissingProviders,
    StartMissingProviders=[start_missing(ProviderSpec,HostSpec)||{ProviderSpec,HostSpec,_App}<-lists:sort(WantedState)],
    
     ok.


start_missing([],Acc)->
    Acc;
start_missing([{ProviderSpec,HostSpec,_App}|T],Acc)->
    Loaded=kube:is_provider_loaded(ProviderSpec,HostSpec),
    Started=kube:is_provider_started(ProviderSpec,HostSpec),
    io:format("ProviderSpec,HostSpec, Loaded,Started, ~p~n",[{ProviderSpec,HostSpec,Loaded,Started,?MODULE,?FUNCTION_NAME,?LINE}]),
    NewAcc=case {Loaded,Started} of
	       {false,_}->
		   case kube:load_provider(ProviderSpec,HostSpec) of
		       {ok,ProviderSpec,HostSpec,_ProviderNode,ProviderApp}->
			   [{kube:start_provider(ProviderSpec,HostSpec),ProviderSpec,HostSpec}|Acc];
		       {error,Reason}->
			   [{error,[ProviderSpec,HostSpec,Reason,?MODULE,?LINE]}|Acc]
		   end;
	       {true,false}->
		   [{kube:start_provider(ProviderSpec,HostSpec),ProviderSpec,HostSpec}|Acc];
	       {true,true}->
		   Acc
	   end,
    start_missing(T,NewAcc).
    
		     
		       
		   
		   
    



%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
simple_deployment_file_3()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),

    {error,["couldnt read file ","glurk",enoent,lib_provider,_]}=kube:wanted_state_from_file("glurk"),
    {ok,[{"test_appl","c201",test_appl},
	 {"test_appl","c200",test_appl},
	 {"divi","c201",divi},
	 {"divi","c200",divi},
	 {"adder","c200",adder}
	]
    }=kube:wanted_state_from_file(?File),
    
    %% start
    io:format("Start provider test~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    [{ok,"adder","c200",'adder@c200',adder,lib_provider,_},
     {ok,"divi","c200",'divi@c200',divi,lib_provider,_},
     {ok,"divi","c201",'divi@c201',divi,lib_provider,_},
     {ok,"test_appl","c200",'test_appl@c200',test_appl,lib_provider,_},
     {ok,"test_appl","c201",'test_appl@c201',test_appl,lib_provider,_}
    ]=lists:sort(kube:create_deployment_from_file(?File)),
    
    ['adder@c200']=lists:sort(sd:get_node(adder)),
    ['divi@c200','divi@c201']=lists:sort(sd:get_node(divi)),
    ['test_appl@c200','test_appl@c201']=lists:sort(sd:get_node(test_appl)),
    42=sd:call(adder,adder,add,[20,22],5000),
   
    %% stop
    io:format("Stop provider test~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    [{ok,"adder","c200",lib_provider,_},
     {ok,"divi","c200",lib_provider,_},
     {ok,"divi","c201",lib_provider,_},
     {ok,"test_appl","c200",lib_provider,_},
     {ok,"test_appl","c201",lib_provider,_}
    ]=lists:sort(kube:delete_deployment_from_file(?File)),
    
    []=lists:sort(sd:get_node(adder)),
    []=lists:sort(sd:get_node(divi)),
    []=lists:sort(sd:get_node(test_appl)),
    {error,["No node available for app : ",adder,sd,_]}=sd:call(adder,adder,add,[20,22],5000),
   
    
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
simple_deployment_file_2()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),

    {error,["couldnt read file ","glurk",enoent,oam_lib,_]}=kube:wanted_state_from_file("glurk"),
    {ok,[{"adder","c200"},
	 {"divi","c200"},
	 {"divi","c201"},
	 {"test_appl","c200"},
	 {"test_appl","c201"}]}=kube:wanted_state_from_file(?File),

    %% start
    io:format("Start provider test~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    [{ok,"adder","c200",'adder@c200',adder,oam_lib,100},
     {ok,"divi","c200",'divi@c200',divi,oam_lib,100},
     {ok,"divi","c201",'divi@c201',divi,oam_lib,100},
     {ok,"test_appl","c200",'test_appl@c200',test_appl,oam_lib,100},
     {ok,"test_appl","c201",'test_appl@c201',test_appl,oam_lib,100}
    ]=lists:sort(kube:create_deployment_from_file(?File)),
    
    ['adder@c200']=lists:sort(sd:get_node(adder)),
    ['divi@c200','divi@c201']=lists:sort(sd:get_node(divi)),
    ['test_appl@c200','test_appl@c201']=lists:sort(sd:get_node(test_appl)),
    42=sd:call(adder,adder,add,[20,22],5000),
   
    %% stop
    io:format("Stop provider test~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    [{ok,"adder","c200",oam_lib,73},{ok,"divi","c200",oam_lib,73},
     {ok,"divi","c201",oam_lib,73},{ok,"test_appl","c200",oam_lib,73},
     {ok,"test_appl","c201",oam_lib,73}
    ]=lists:sort(kube:delete_deployment_from_file(?File)),
    
    []=lists:sort(sd:get_node(adder)),
    []=lists:sort(sd:get_node(divi)),
    []=lists:sort(sd:get_node(test_appl)),
    {error,["No node available for app : ",adder,sd,_]}=sd:call(adder,adder,add,[20,22],5000),
   
    
    ok.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
simple_deployment_file()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    {error,["No deployment info exists ",oam,_]}=kube:wanted_state(),   
    {error,["couldnt read file ","glurk",enoent,oam_lib,_]}=kube:deploy_w_file("glurk"),
    {error,["No deployment info exists ",oam,_]}=kube:wanted_state(),   
    ok=oam:deploy_w_file(?File),
    {ok,[{"adder","c200"},
	 {"divi","c200"},
	 {"divi","c201"},
	 {"test_appl","c200"},
	 {"test_appl","c201"}
	]}=oam:wanted_state(),  
    ['adder@c200']=lists:sort(sd:get_node(adder)),
    ['divi@c200','divi@c201']=lists:sort(sd:get_node(divi)),
    ['test_appl@c200','test_appl@c201']=lists:sort(sd:get_node(test_appl)),

    42=sd:call(adder,adder,add,[20,22],5000),
    
    ok.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
load_start_provider(ProviderSpec,HostSpec)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    
    false=oam:is_provider_node_started(ProviderSpec,HostSpec),
    false=oam:is_provider_started(ProviderSpec,HostSpec),
    false=oam:is_provider_loaded(ProviderSpec,HostSpec),

    %load
    {ok,"adder","c200",'adder@c200',adder}=oam:load_provider(ProviderSpec,HostSpec),
    true=oam:is_provider_node_started(ProviderSpec,HostSpec),
    true=oam:is_provider_loaded(ProviderSpec,HostSpec),
    false=oam:is_provider_started(ProviderSpec,HostSpec),
    %start
    ok=oam:start_provider(ProviderSpec,HostSpec),
    true=oam:is_provider_node_started(ProviderSpec,HostSpec),
    true=oam:is_provider_loaded(ProviderSpec,HostSpec),
    true=oam:is_provider_started(ProviderSpec,HostSpec),

    %% 
    42=sd:call(adder,adder,add,[20,22],5000),

    ok.
    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
stop_unload_provider(ProviderSpec,HostSpec)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    
    true=oam:is_provider_node_started(ProviderSpec,HostSpec),
    true=oam:is_provider_loaded(ProviderSpec,HostSpec),
    true=oam:is_provider_started(ProviderSpec,HostSpec),
    {ok,"adder","c200",'adder@c200',adder}=oam:load_provider(ProviderSpec,HostSpec),
    ok=oam:start_provider(ProviderSpec,HostSpec),

    %% stop
   {error,["Failed to unload app ",ProviderSpec,HostSpec,{running,_},
	   lib_provider,unload,_]
   }=oam:unload_provider(ProviderSpec,HostSpec),
   
    ok=oam:stop_provider(ProviderSpec,HostSpec),
    true=oam:is_provider_node_started(ProviderSpec,HostSpec),
    true=oam:is_provider_loaded(ProviderSpec,HostSpec),
    false=oam:is_provider_started(ProviderSpec,HostSpec),
   
    %% unload
    {error,[{not_started,_},
	    lib_provider,stop,_]
    }=oam:stop_provider(ProviderSpec,HostSpec),
    ok=oam:unload_provider(ProviderSpec,HostSpec),
    false=oam:is_provider_node_started(ProviderSpec,HostSpec),
    false=oam:is_provider_loaded(ProviderSpec,HostSpec),
    false=oam:is_provider_started(ProviderSpec,HostSpec),

    ok.

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
stop_start_controllers()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    
    [ok,ok]=[oam:stop_controller(HostSpec)||HostSpec<-?TestHosts],
    [false,false]=[oam:is_controller_started(HostSpec)||HostSpec<-?TestHosts],

    [ok,ok]=[oam:start_controller(HostSpec)||HostSpec<-?TestHosts],
    [true,true]=[oam:is_controller_started(HostSpec)||HostSpec<-?TestHosts],

      

    ok.
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% ------------------------------------------------------------------

setup()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
  
    pong=net_adm:ping(?DbEtcdNode),
    ok=application:start(simple_oam),
    pong=oam:ping(),
    pong=kube:ping(),
    pong=dbetcd:ping(),
    pong=common:ping(),
    pong=sd:ping(),
    pong=log:ping(),


    ok.
