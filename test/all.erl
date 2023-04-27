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

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start()->
   
    ok=setup(),
  
    ok=stop_start_controllers(),    

    ok=load_start_provider("adder","c200"),    
    ok=stop_unload_provider("adder","c200"),  
              


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
load_start_provider(ProviderSpec,HostSpec)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    
    false=oam:is_provider_node_started(ProviderSpec,HostSpec),
    false=oam:is_provider_started(ProviderSpec,HostSpec),
    false=oam:is_provider_loaded(ProviderSpec,HostSpec),

    %load
    ok=oam:load_provider(ProviderSpec,HostSpec),
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
    ok=oam:load_provider(ProviderSpec,HostSpec),
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

    %kuk=sd:call(dbetcd_appl,db_host_spec,get_all_id,[],5000),
    
    

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
