#	Makefile for rebar3 RELEASE 
all:
	rm -rf  *~ apps/dbetcd/src/*~ *~ apps/dbetcd/src/*.beam test/*.beam test/*~ erl_cra* config/*~;
	rm -rf _build ebin test_ebin;
	rm -rf common sd api;
	rm -rf Mnesia.* logs;
	rm -rf rebar.lock;
	rm -rf  _build/test; # A bugfix in rebar3 or 
	rebar3 compile;
	mkdir ebin;
#	Included services
	cp _build/default/lib/cmn_service/ebin/* ebin;
	cp _build/default/lib/sd_service/ebin/* ebin;
	cp _build/default/lib/log_service/ebin/* ebin;
	cp _build/default/lib/dbetcd_service/ebin/* ebin;
	cp _build/default/lib/kube_service/ebin/* ebin;
#	Included application
	cp _build/default/lib/simple_oam/ebin/* ebin;
	rm -rf _build*;
	git add -f *;
	git commit -m $(m);
	git push;
	echo Ok there you go!make
build:
	rm -rf  *~ apps/dbetcd/src/*~ *~ apps/dbetcd/src/*.beam test/*.beam test/*~ erl_cra* config/*~;
	rm -rf common sd api;
	rm -rf Mnesia.* logs;
	rm -rf  _build/test; # A bugfix in rebar3 or OTP
	rm -rf  _build;
	rm -rf rebar.lock;
	rebar3 compile;	

clean:
	rm -rf  *~ apps/dbetcd/src/*~ *~ apps/dbetcd/src/*.beam test/*.beam test/*~ erl_cra* config/*~;
	rm -rf _build ebin test_ebin;
	rm -rf common sd api;
	rm -rf Mnesia.* logs;

eunit:
	rm -rf  *~ apps/dbetcd/src/*~ *~ apps/dbetcd/src/*.beam test/*.beam test/*~ erl_cra* config/*~;
	rm -rf _build ebin test_ebin;
	rm -rf common sd api;
	rm -rf Mnesia.* logs;
	rm -rf rebar.lock;
	rm -rf  _build/test; # A bugfix in rebar3 or OTP
	rebar3 compile;
	mkdir ebin;
#	Included services
	cp _build/default/lib/cmn_service/ebin/* ebin;
	cp _build/default/lib/sd_service/ebin/* ebin;
	cp _build/default/lib/log_service/ebin/* ebin;
	cp _build/default/lib/dbetcd_service/ebin/* ebin;
	cp _build/default/lib/kube_service/ebin/* ebin;
#	Included application
	cp _build/default/lib/simple_oam/ebin/* ebin;
	rm -rf _build*;
	mkdir test_ebin;
	erlc -I api -I /home/joq62/erlang/infra/api_repo -o test_ebin test/*.erl;
	erl -pa ebin\
	    -pa test_ebin\
	    -config config/sys.config\
	    -sname do_test -run $(m) start\
	    -setcookie $(c)
