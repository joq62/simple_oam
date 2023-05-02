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
	 deploy_w_file/1

	]).

%%%===================================================================
%%% API
%%%===================================================================

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


deploy([],Acc)->
    Acc;
deploy([{ProviderSpec,HostSpec}|T],Acc)->
    NewAcc=case kube:is_provider_loaded(ProviderSpec,HostSpec) of
	       false->
		   case kube:load_provider(ProviderSpec,HostSpec) of
		       ok->
			   case kube:start_provider(ProviderSpec,HostSpec) of
			       ok->
				   [{ok,ProviderSpec,HostSpec}|Acc];
			       {error,Reason}->
				   [{error,Reason,ProviderSpec,HostSpec}|Acc]
			   end;
		       {error,Reason}->
			   [{error,Reason,ProviderSpec,HostSpec}|Acc]
		   end;
	       true->
		   [{error,already_loaded,ProviderSpec,HostSpec}|Acc]
	   end,
    deploy(T,NewAcc).

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




