
-module(email).

-compile(export_all).

-include("email.hrl").

send_mail(#email{host_name = undefined}) -> false;
send_mail(#email{port = undefined}) -> false;
send_mail(#email{user_name = undefined}) -> false;
send_mail(#email{password = undefined}) -> false;
send_mail(#email{to_list = undefined}) -> false;
send_mail(#email{host_name = HostName, conn = Conn, port = Port} = Email) ->
    start(),
    {ok, Socket} = connect(Conn, HostName, Port),
    SocketEmail = Email#email{socket = Socket}, 
    mail_auth(SocketEmail),
    mail_head(SocketEmail),
    mail_info(SocketEmail),
    mail_data(SocketEmail),
    mail_end(SocketEmail),
    close(SocketEmail),
    stop(),
    ok.

mail_auth(Email) ->
    #email{user_name = UserName, password = Password} = Email,
    send_socket(Email, helo, UserName),
    recv_socket(Email),

    send_socket(Email, auth, []),
    recv_socket(Email),

    send_socket(Email, user_name, UserName),
    send_socket(Email, lf, after_user_name),
    recv_socket(Email),

    send_socket(Email, password, Password),
    send_socket(Email, lf, after_password),
    recv_socket(Email),
    ok.

mail_head(Email) ->
    #email{from = From, to_list = ToList} = Email,
    send_socket(Email, mail_from, From),
    recv_socket(Email),
    io:format("~n M:~p L:~p dsaaaaaaa:~p ~n", [?MODULE, ?LINE, ToList]),
    [send_socket(Email, mail_to, To) || To <- ToList],
    recv_socket(Email), 
    ok.

mail_info(Email) ->
    #email{subject = Subject, from = From} = Email,
    send_socket(Email, data, []),
    recv_socket(Email),

    send_socket(Email, from, From),
    recv_socket(Email),

    send_socket(Email, subject, Subject),
    ok.

mail_data(#email{content = Content} = Email) ->
    send_socket(Email, mime_version, []),
    send_socket(Email, content_type, []),
    send_socket(Email, lf, [after_conten_type]),

    send_socket(Email, boundary, []),
    send_socket(Email, content_type_title, []),
    send_socket(Email, content_type_value, text),
    send_socket(Email, lf, af_content_type_value),
    send_socket(Email, content, Content),
    send_socket(Email, lf, af_content),
    ok.

mail_end(Email) ->
    send_socket(Email, lf, quit_before),
    recv_socket(Email),
    send_socket(Email, quit, []),
    recv_socket(Email),
    ok.

send_socket(Email, Key, Params) ->
    {ok, BinData} = write_proto(Key, Params),
    send(Email, BinData).

recv_socket(Email) -> 
    case recv(Email) of
        {ok, Bin} -> io:format("~n M:~p L:~p recv_socket success:~p~n", [?MODULE, ?LINE, binary_to_list(Bin)]);
        {error, Reason} -> io:format("~n M:~p L:~p recv_socket failed: ~p~n", [?MODULE, ?LINE, Reason])
    end.

write_proto(Key, Params) ->
    Term = proto(Key, Params),
    io:format("~n M:~p L:~p send_socket:~p~n", [?MODULE, ?LINE, Term]),
    Bin = make_sure_binary(Term),
    {ok, Bin}.

proto(helo, UserName) -> "HELO " ++ UserName ++ "\r\n";
proto(auth, []) -> "AUTH LOGIN\r\n";
proto(user_name, UserName) -> base64:encode(UserName);
proto(lf, after_user_name) -> "\r\n";
proto(password, Password) -> base64:encode(Password);
proto(lf, after_password) -> "\r\n";
proto(mail_from, From) -> "MAIL FROM <" ++ From ++ ">\r\n";
proto(mail_to, To) -> "RCPT TO <" ++ To ++ ">\r\n";
proto(data, []) -> "DATA\r\n";
proto(from, From) -> "FROM:<" ++ From ++ ">\r\n";
proto(subject, Subject) -> "SUBJECT:"++ Subject ++ "\r\n";
proto(mime_version, []) -> "MIME-VERSION: 1.0\r\n";
proto(content_type, []) -> "CONTENT-TYPE: multipart/mixed; BOUNDARY=\"#BOUNDARY#\"\r\n";
proto(lf, [after_conten_type]) -> "\r\n";
proto(boundary, []) -> "--#BOUNDARY#\r\n";
proto(content_type_title, []) -> "CONTENT-TYPE: ";
proto(content_type_value, text) -> "text/plain";
proto(lf, af_content_type_value) -> "\r\n\r\n";
proto(content, Content) -> Content;
proto(lf, af_content) -> "\r\n\r\n";
proto(lf, quit_before) -> "\r\n.\r\n";
proto(quit, []) -> "QUIT\r\n".

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
    Conn:send(Socket, Data).

recv(#email{conn = Conn, socket = Socket}) -> 
    Conn:recv(Socket, 0).
recv(#email{conn = Conn, socket = Socket}, Opts) -> 
    Conn:recv(Socket, Opts).

send_mail_test() ->
    send_mail(#email{host_name   = "smtp.sina.cn",
                user_name   = "asdfasn",
                password    = "adfas",
                subject     = "smtp邮件测试",
                from = "adsf",
                to_list   = ["asdfas"],
                content = "my test",
                conn = ?SOCKET_SSL,
                port = 465
                }).