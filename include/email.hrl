
%% socket 
-define(SOCKET_GEN_TCP, gen_tcp).
-define(SOCKET_SSL, ssl).
-define(SOCKET_OPTS, [binary, {active, false}, {packet, 0}]).

%% recipient type
-define(TO, "TO").     
-define(CC, "CC").
-define(BCC, "BCC").

%% email record
-record(email, {
        host_name
        , conn :: ?SOCKET_GEN_TCP | ?SOCKET_SSL
        , port
        , socket
        , user_name
        , password
        , subject
        , content
        , from 
        , to_list :: [mailbox]
    }).