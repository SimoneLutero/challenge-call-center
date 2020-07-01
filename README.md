# erl_playground

An OTP application to start coding without the boring stuff.

## Prerequisites
This project has been written for Mac and Linux environments, theoretically speaking it can run on any environment where a Erlang system is correcty installed, but consider that MS Windows and Erlang are not best buddies. Nowadays it is pretty easy to have Linux systems running in minutes using Virtual Machines, Containers, USB distro or simply double booting your laptop.

In case you use a Mac system, we strongly recommend using [homebrew](https://brew.sh/) to manage all your packages.

**OpenSSL**

Check the correct installation process for you environment.

**Erlang/OTP 21.3**

If you are on Mac, we strongly suggest using [kerl](https://github.com/kerl/kerl) to build and install the proper Erlang version on your system. For other environments you can easily find your installation package on [ErlangSolutions](https://www.erlang-solutions.com/).

## Build & Run

This is a [rebar3](https://www.rebar3.org/) project.

The command used to build the project is `rebar3 compile`.
The command used to run the project is `rebar3 shell`.
The command used to release the project is `rebar3 release`

## Compile GPB

Google Protocol Buffer is automatically compiled starting from the included proto file.
[Here](https://developers.google.com/protocol-buffers/) you can find all the information about it.

## What you have out of the box
This is a playgrounf application that allows you to focus on the logic of your system, rather than the boring technical stuff. It includes a basic Erlang/OTP application structure with a TCP client and a TCP server.

# Candidate comments
I am Simone Lutero and this is my implementation of a call-center simulator, based on the application erl_playground.

## Implemented features
You can use this application to simulate a call to a call center, which provide you the following features:

1. Get tomorrow weather forecast for Genoa (Italy).
2. Get a random joke.
3. Get the call ID.
4. Ask for an operator who can tell you his Pid or say if a number is even or odd.

The **call_center_client** module provide a few simple functions with wich the user can interact with the server, connecting and disconnecting to it and sending messages.

## How it works?
The **erl_playground** application starts a supervisor which handles two processes that run **node_boot** and **ranch_sup**.

**ranch_sup** must be started to make ranch listeners available.

The **node_boot** module reads from *sys.config* the environment variable *boot_node* and start **sockclient** and **sockserv** based on this value which can be:

* client_server: both client and server will be started.
* only_server: only the server will be started.
* only_client: only the client will be started.

After this, user is ready to interact with the module **call_center_client** and the firt things he has to do are to create a connection using function *connect/0* and initiate a call providing his username with the method *login/1*.

**sockclient** take the address and the port of the listening host from *sys.config*, then enstabilshes connection with it and send the create_session message enveloped as a google protocol buffers message to the server.

When **sockserv** module receive messages it opens them and process the content; in case of create_session message it spawn a new process which will run the method *handle_client* of **call_center_serv_logic** module, registering it with the *Username* he received.

The *handle_client* method is an automatic responder, when it spawns it shows a menu with the possible actions it can do in response to the requests it handles.

To send a message user must use the function *send_message/2* of **call_center_client**, providing as attributes his Username and the content of the message to send; like the create_session message during login also this client_message will be enveloped and sent by **sockclient** to **sockserv**.

If the automatic responder recognize an accepted message it elaborates the response and send it to **sockclient** using the client socket.

In case of request of weather forecast or random joke the *handle_client* process calls methods of modules that interact with web API through http protocol and return the obtained results.

If the user choose to speak to an operator the *handle_client* process spawns a new process that acts as defined in the **call_center_operator_logic** module.

This operator has a timeout of 10 seconds and a max number of response (3), then it terminate itself.

When the operator is active the *handle_client* process becomes a bridge between **sockserv** and **operator** process, passing client messages from the first one to the second one, until the termination of **operator** process.

When the client decide to disconnect he has to call the *logout/1* method providing his username, or he can close session and connection with the *disconnect/1* method; this will result in a close_session message the will make the *handle_client* process exit and will interrupt the connection to the server.

## Diagram

![Diagram](images/call-center-simulator-schema.jpg)

## Example

```
call_center_client:connect().
17:46:31.571 [info] sockserv init'ed #Port<0.34>
ok
2> call_center_client:login("simone").  
ok
3> 17:47:02.208 [info] create_session received from user <<"simone">>
17:47:02.209 [info] server_message received: <<"session created">>
17:47:02.211 [info] server_message received: <<"Hi! Send 1 to receive the weather forecast for tomorrow 2 - Send 2 to receive a random joke 3 - Send 3 to request your call ID 4 - Send 4 to ask for an operator">>
3> call_center_client:send_message("simone",1).
ok
17:47:27.631 [info] client_message received from <<"simone">>
4> 17:47:27.980 [info] server_message received: <<"few clouds">>
17:47:27.980 [info] server_message received: <<"Hi! Send 1 to receive the weather forecast for tomorrow 2 - Send 2 to receive a random joke 3 - Send 3 to request your call ID 4 - Send 4 to ask for an operator">>
4> call_center_client:send_message("simone",2).
ok
17:47:31.305 [info] client_message received from <<"simone">>
5> 17:47:31.527 [info] server_message received: <<"What do you call a dad that has fallen through the ice? A Popsicle.">>
17:47:31.527 [info] server_message received: <<"Hi! Send 1 to receive the weather forecast for tomorrow 2 - Send 2 to receive a random joke 3 - Send 3 to request your call ID 4 - Send 4 to ask for an operator">>
5> call_center_client:send_message("simone",3).
17:47:41.297 [info] client_message received from <<"simone">>
ok
17:47:41.297 [info] server_message received: <<"<0.384.0>">>
17:47:41.297 [info] server_message received: <<"Hi! Send 1 to receive the weather forecast for tomorrow 2 - Send 2 to receive a random joke 3 - Send 3 to request your call ID 4 - Send 4 to ask for an operator">>
6> call_center_client:send_message("simone",4).
ok
17:47:50.493 [info] client_message received from <<"simone">>
7> 17:47:50.495 [info] server_message received: <<"Hi, I'm the operator <0.390.0>, how can I help you?">>
7> call_center_client:send_message("simone",4).
ok
17:47:52.749 [info] client_message received from <<"simone">>
17:47:52.750 [info] server_message received: <<"The number is even">>
8> call_center_client:send_message("simone","ciao").
17:47:57.042 [info] client_message received from <<"simone">>
ok
17:47:57.042 [info] server_message received: <<"This process Pid is <0.390.0>">>
9> call_center_client:send_message("simone",2).     
17:48:02.781 [info] client_message received from <<"simone">>
ok
17:48:02.782 [info] server_message received: <<"The number is even">>
10> 17:48:02.782 [info] server_message received: <<"Max number of questions reached, see you soon">>
17:48:02.782 [info] server_message received: <<"Conversation with operator is terminated">>
17:48:02.782 [info] server_message received: <<"Hi! Send 1 to receive the weather forecast for tomorrow 2 - Send 2 to receive a random joke 3 - Send 3 to request your call ID 4 - Send 4 to ask for an operator">>
10> call_center_client:logout("simone").             
ok
17:48:22.354 [info] close_session received from <<"simone">>
11> 17:48:22.354 [info] server_message received: <<"session closed">>
11> call_center_client:disconnect().                 
** exception error: undefined function call_center_client:disconnect/0
12> 
```

