# email
erlang mail

理解原理
http://www.cnblogs.com/the-tops/p/5640462.html
[lix@nslix]$telnet smtp.163. com 25
220 163 .com Anti-spam GT for Coremail System (163com[071018])
HELO smtp.163 .com
250 OK
auth login
334 dXNlcm5hbWU6
USERbase64加密后的用户名
334 UGFzc3dvcmQ6
PASSbase64加密后的密码
235 Authentication successful
MAILFROM:XXX@163 .COM
250 Mail OK
RCPTTO:XXX@163 .COM
250 Mail OK
DATA
354 End data with .
QUIT

SMTP要经过建立连接、传送邮件和释放连接3个阶段。具体为：
（1）建立TCP连接。
（2）客户端向服务器发送HELO命令以标识发件人自己的身份，然后客户端发送MAIL命令。
（3）服务器端以OK作为响应，表示准备接收。
（4）客户端发送RCPT命令。
（5）服务器端表示是否愿意为收件人接收邮件。
（6）协商结束，发送邮件，用命令DATA发送输入内容。
（7）结束此次发送，用QUIT命令退出。