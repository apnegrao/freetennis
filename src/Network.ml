open Unix

exception CouldNotConnectToServer

type serverData = Server of (Unix.file_descr * Unix.file_descr) * in_channel * out_channel
                | Client of Unix.file_descr * in_channel * out_channel
                | NeitherServerNorClient

let setupNetwork ~server ~client ~port =
  let rec tryToConnectNTimes n ~soc ~inet_a ~port=
    try
      Unix.connect  soc (Unix.ADDR_INET  (inet_a, port) )
    with Unix.Unix_error _ ->
      if n = 0 then
        raise CouldNotConnectToServer
      else
        ( print_endline ( "The server is down. Retrying " ^ string_of_int (n-1) ^ " times.");
          Unix.sleep 1;
          tryToConnectNTimes (n-1) ~soc ~inet_a ~port )
  in
  if !server  then
    let soc = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    ( 
      Unix.setsockopt soc SO_REUSEADDR true;
      (try
         Unix.bind soc (Unix.ADDR_INET (Unix.inet_addr_any, !port))
       with Unix.Unix_error (err, _, _) ->
         (print_endline ("Error: " ^(Unix.error_message err) ^ ". This is a known bug. Please wait a few seconds or simply change the port number with the -port option");
          exit 0 ));
      Unix.listen soc 5;
      print_endline "waiting for client to connect...";
      let clientSocket, _ = Unix.accept soc in
      Server ( (soc, clientSocket), Unix.in_channel_of_descr clientSocket, Unix.out_channel_of_descr clientSocket)
    )

  else  if 0 != compare !client  ""  then
    let soc = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    let inet_a = Unix.inet_addr_of_string !client in
    print_endline "Connecting to server...";
    ( tryToConnectNTimes 60 ~soc ~inet_a ~port:!port;
      print_endline "Connected to server";
      Client (soc, Unix.in_channel_of_descr  soc, Unix.out_channel_of_descr soc) )
  else
    NeitherServerNorClient
