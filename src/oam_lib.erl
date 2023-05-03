%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%%
%%% @end
%%% Created :  2 May 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(oam_lib).

%% API
-export([
	 deploy_w_file/1,
	 create_deployment_from_file/1,
	 delete_deployment_from_file/1,
	 wanted_state_from_file/1

	]).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
create_deployment_from_file(FullPathFile)->
 %   io:format("Dbg ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE,FullPathFile}]),
    Result=case file:consult(FullPathFile) of
	       {error,Reason}->
		   {error,["couldnt read file ",FullPathFile,Reason,?MODULE,?LINE]};
	       {ok,List}->
		   io:format("Dbg ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE,List}]),
		   deploy(List,[])
	   end,    
    Result.

delete_deployment_from_file(FullPathFile)->
  %  io:format("Dbg ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE,FullPathFile}]),
    Result=case file:consult(FullPathFile) of
	       {error,Reason}->
		   {error,["couldnt read file ",FullPathFile,Reason,?MODULE,?LINE]};
	       {ok,List}->
		   delete(List,[])
	   end,    
    Result.

wanted_state_from_file(FullPathFile)->
    Result=case file:consult(FullPathFile) of
	       {error,Reason}->
		   {error,["couldnt read file ",FullPathFile,Reason,?MODULE,?LINE]};
	       {ok,List}->
		   {ok,List}
	   end,    
    Result.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
delete([],Acc)->
    Acc;
delete([{ProviderSpec,HostSpec}|T],Acc)->
   % io:format("delete ~p~n",[{ProviderSpec,HostSpec,?MODULE,?FUNCTION_NAME,?LINE}]),
    NewAcc=case kube:is_provider_loaded(ProviderSpec,HostSpec) of
	       true->
		   case kube:stop_provider(ProviderSpec,HostSpec) of
		       ok->
			   case kube:unload_provider(ProviderSpec,HostSpec) of
			       ok->
				   [{ok,ProviderSpec,HostSpec,?MODULE,?LINE}|Acc];
			       {error,Reason}->
				   [{error,Reason,ProviderSpec,HostSpec,?MODULE,?LINE}|Acc]
			   end;
		       {error,Reason}->
			   [{error,Reason,ProviderSpec,HostSpec,?MODULE,?LINE}|Acc]
		   end;
	       false->
		   [{error,not_loaded,ProviderSpec,HostSpec,?MODULE,?LINE}|Acc]
	   end,
    delete(T,NewAcc).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
deploy([],Acc)->
    Acc;
deploy([{ProviderSpec,HostSpec}|T],Acc)->
   % io:format("deploy ~p~n",[{ProviderSpec,HostSpec,?MODULE,?FUNCTION_NAME,?LINE}]),
    NewAcc=case kube:is_provider_loaded(ProviderSpec,HostSpec) of
	       false->
		   case kube:load_provider(ProviderSpec,HostSpec) of
		       {ok,ProviderSpec,HostSpec,ProviderNode,App}->
			   case kube:start_provider(ProviderSpec,HostSpec) of
			       ok->
				   [{ok,ProviderSpec,HostSpec,ProviderNode,App,?MODULE,?LINE}|Acc];
			       {error,Reason}->
				   [{error,Reason,ProviderSpec,HostSpec,?MODULE,?LINE}|Acc]
			   end;
		       {error,Reason}->
			   [{error,Reason,ProviderSpec,HostSpec,?MODULE,?LINE}|Acc]
		   end;
	       true->
		   [{error,already_loaded,ProviderSpec,HostSpec,?MODULE,?LINE}|Acc]
	   end,
    deploy(T,NewAcc).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
deploy_w_file(FullPathFile)->
    Result=case file:consult(FullPathFile) of
	       {error,Reason}->
		   {error,["couldnt read file ",FullPathFile,Reason,?MODULE,?LINE]};
	       {ok,List}->
		   deploy(List,[]),
		   {ok,List}
	   end,
    
    Result.



%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
deploy_w_list(List)->
    
    glurk_not_implmented.
%%%===================================================================
%%% Internal functions
%%%===================================================================




