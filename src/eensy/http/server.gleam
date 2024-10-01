// -module(http_server_ffi).

// -export([start_server/2, reply/3, reply/4, parse_query_string/1]).

// start_server(Port, Router) ->
//     case gen_tcp:listen(Port, []) of
//         {ok, ListenSocket} ->
//             spawn(fun() -> accept(ListenSocket, Router) end);
//         Error ->
//             erlang:display(Error)
//     end.
