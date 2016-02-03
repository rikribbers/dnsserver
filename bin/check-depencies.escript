#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname deps verbose

%% Check the rebar.config for the latest and greatest tags for dependacies
%% PREREQUISITE: assumes the depencies are already available locally in
%% directory _build/default/lib/

main(_) ->
  %% process rebar.config for dependancies

  ConfigFilePath = filename:join([".", "rebar.config"]),
  {ok, AppsConfig} = file:consult(ConfigFilePath),

  %% for each entry in rebar.config find the tuple {deps,[]} and {overrides,[]}
  List1 = [case Y of {deps,Deps} -> Deps; _ -> [] end || Y <- AppsConfig ],

  %% Flatten the list so we get the dependancies
  Dependancies = lists:flatten(List1),
  io:fwrite("Dependancies=~p~n",[Dependancies]),

  process_dependancies(Dependancies).

%% Check a local repository for the latest tags using git log command
%% and compare the result with the provided version
%% Inspired from reading:
%% https://stackoverflow.com/questions/20055398/is-it-possible-to-get-commit-logs-messages-of-a-remote-git-repo-without-git-clon

process_dependancies([]) ->
  ok;

%% Example: {lager, {git, "https://github.com/basho/lager.git", {tag, "3.0.2"}}},
process_dependancies([{Lib, {git, GitURL, {tag,Version}}} | T ]) ->

  TagString = os:cmd("cd _build/default/lib/" ++ atom_to_list(Lib) ++
      "; git log -1 --tags --simplify-by-decoration --pretty=\"format:%d\""),
  %%io:fwrite("TagString=~p Version=~p~n",[TagString,Version]),

  %% output is in form: tag: 1.0.4 along with other stuff
  case string:str(TagString,"tag: " ++ Version) > 0 of
    true ->
      io:fwrite("[OK] ~p~n",[GitURL]);
    false ->
      io:fwrite("[NOK] ~p ~p != ~p~n",[GitURL,TagString,Version]),
      halt(1)
  end,
  process_dependancies(T);

%% Example {inoteefy, {git, "https://github.com/LeebDeveloper/inoteefy.git",{ref,"a54935758e6a5dda891647c3c2ae70a0744ab473"}}}
process_dependancies([{Lib, {git, GitURL, {ref,Commit}}} | T ]) ->

  ActualCommit = os:cmd("cd _build/default/lib/" ++ atom_to_list(Lib) ++
      "; git log -1 --simplify-by-decoration --pretty=\"format:%H\""),
  %%io:fwrite("ActualCommit=~p Commit=~p~n",[ActualCommit,Commit]),

  %% output is in form: tag: 1.0.4 along with other stuff
  case string:str(ActualCommit,Commit) > 0 of
    true ->
      io:fwrite("[OK] ~p~n",[GitURL]);
    false ->
      io:fwrite("[NOK] ~p ~p != ~p~n",[GitURL,ActualCommit,Commit]),
      halt(1)
  end,
  process_dependancies(T);

%% Example: {dns_erlang,{git,"https://github.com/rikribbers/dns_erlang.git", {branch,"rebar3"}
process_dependancies([{Lib, {git, GitURL, {branch,Branch}}} | T ]) ->
  %% for branches nothing needs to be done, simply use is
  io:fwrite("[OK] ~p~n",[GitURL]),
  process_dependancies(T).




