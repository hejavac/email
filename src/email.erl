
-module(email).

-compile(export_all).

send_mail(#email{host_name = undefined}) -> false;
send_mail(#email{port = undefined}) -> false;
send_mail(#email{user_name = undefined}) -> false;
send_mail(#email{password = undefined}) -> false;
send_mail(#email{to = undefined}) -> false;
send_mail(#email{host_name = HostName, conn = Conn, port = Port} = Email) ->
    {ok, Socket} = connect(Conn, HostName, Port),
    SocketEmail = Email#email{socket = Socket}, 
    auth(SocketEmail),


mail_auth(Email) ->
    #email{user_name = UserName, password = Password} = Email,
    send_socket()

mail_head(Email) ->

mail_info() ->

mail_data() ->

mail_end() ->

send_socket(Email, Key, Params) ->
    {ok, BinData} = write_proto(Key, Params),
    send(Email, BinData).

recv_socket(Socket) -> 
    case recv(Socket) of
        {ok, Bin} -> io:format("~n M:~p L:~p recv_socket success:~p~n", [?MODULE, ?LINE, binary_to_list(Bin)]);
        {error, Reason} -> io:format("~n M:~p L:~p recv_socket failed: ~p~n", [?MODULE, ?LINE, Reason])
    end.

write_proto(Key, Params) ->
    Term = proto(Key, Params),
    Bin = make_sure_binary(Term),
    {ok, Bin}.

proto(helo, UserName) -> "HELO " ++ UserName ++ "\r\n";
proto(auth, []) -> "AUTH LOGIN\r\n";
proto(user_name, UserName) -> base64:encode(UserName);
proto(lf, [after_user_name]) -> "\r\n";
proto(password, Password) -> base64:encode(Password);
proto(lf, [after_password]) -> "\r\n";
proto(mail_from, From) -> "MAIL FROM <" ++ From ++ ">\r\n";
proto(data, []) -> "DATA\r\n";
proto(from, From) -> "FROM:<" ++ From ++ ">\r\n";
proto(subject, From) -> "SUBJECT:"++ Subject ++ "\r\n";
proto(mime_version, []) -> "MIME-VERSION: 1.0\r\n";
proto(content_type, )

make_sure_binary(Term) when is_list(Term) ->
    unicode:characters_to_binary(Term);
make_sure_binary(Term) when is_binary(Term) ->
    Term;
make_sure_binary(_) ->
    <<>>.

start() ->
    ok = ssl:start().

stop() ->
    ssl:stop().

connect(Conn, HostName, Port) ->
    {ok, Socket} = Conn:connect(HostName, Port, ?SOCKET_OPTS),
    {ok, Socket}.

close(#email{conn = Conn, socket = Socket}) -> 
    Conn:close(Socket).

send(#email{conn = Conn, socket = Socket}, Data) -> 
    Conn:send(Socket, Data);

recv(#email{conn = Conn, socket = Socket}) -> 
    Conn:recv(Socket, -1);
recv(#email{conn = Conn, socket = Socket}, Opts) -> 
    Conn:recv(Socket, Opts).
