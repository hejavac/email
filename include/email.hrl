
%% socket 
-record(?SOCKET_GEN_TCP, gen_tcp).
-record(?SOCKET_SSL, ssl).
-record(?SOCKET_OPTS, [binary, {active, false}, {packet, 0}]).

%% recipient type
-record(?TO, "TO").     
-record(?CC, "CC").
-record(?BCC, "BCC").

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
        , to :: [{?TO|?CC|?BCC, mailbox}|...]
    }).