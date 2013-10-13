-module(erlpid_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).



%% Interface



start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    write_pid(),
    {ok, { {one_for_one, 5, 10}, []} }.



%% Internals



write_pid() ->
    case application:get_env(erlpid, destination) of
        {ok, Dest} ->
            check_rights(Dest),
            write_pid(Dest);
        undefined ->
            throw({misconfiguration, destination_is_undefined})
    end.

write_pid(File) ->
    Pid = os:getpid(),
    case file:open(File, [write]) of
        {ok, Ptr} ->
            ok = file:write(Ptr, Pid),
            ok = file:close(Ptr),
            ok;
        {error, enoent} = Error ->
            io:format("Cannot write pid file (~p): enoent, ", [File]),
            io:format("check the file system for ability to write the file.\n"),
            throw(Error);
        {error, eacces} = Error ->
            io:format("Cannot write pid file (~p): eacces, ", [File]),
            io:format("check the file rights.\n"),
            throw(Error);
        {error, enospc} = Error ->
            io:format("Cannot write pid file (~p): enospc, ", [File]),
            io:format("file system has no free space.\n"),
            throw(Error);
        {error, Reason} = Error ->
            io:format("Cannot write pid file (~p): ~p, ", [File, Reason]),
            io:format("contact application developers.\n"),
            throw(Error)
    end.




check_rights(_File) ->
    %% TODO: make rights check here
    ok.