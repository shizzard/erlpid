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
            io:format("Cannot write pid file (~p): enoent, check the file system for ability to write the file.\n", [File]),
            throw(Error);
        {error, eacces} = Error ->
            io:format("Cannot write pid file (~p): eacces, check the file rights.", [File]),
            throw(Error);
        {error, enospc} = Error ->
            io:format("Cannot write pid file (~p): enospc, file system has no free space.\n", [File]),
            throw(Error);
        {error, Reason} = Error ->
            io:format("Cannot write pid file (~p): ~p, contact application developers.\n", [File, Reason]),
            throw(Error)
    end.




check_rights(_File) ->
    %% TODO: make rights check here
    ok.