open Eio.Std

let run_client ~net ~addr =
  traceln "Connecting to server...";
  Switch.run @@ fun sw ->
  let flow = Eio.Net.connect ~sw net addr in
  Eio.Flow.copy_string "Hello from client" flow

let run_server socket =
  Switch.run @@ fun sw ->
  let rec loop () =
    Eio.Net.accept_sub socket ~sw
      (fun ~sw:_ flow _addr ->
        traceln "Server accepted connection from client";
        let b = Buffer.create 100 in
        Eio.Flow.copy flow (Eio.Flow.buffer_sink b);
        traceln "Server received: %S" (Buffer.contents b))
      ~on_error:(traceln "Error handling connection: %a" Fmt.exn);
    loop ()
  in
  loop ()

let main ~net ~addr =
  Switch.run @@ fun sw ->
  let server = Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:5 addr in
  traceln "Server ready...";
  Fibre.both (fun () -> run_server server) (fun () -> run_client ~net ~addr)

let () =
  Eio_luv.run @@ fun env ->
  main ~net:(Eio.Stdenv.net env) ~addr:(`Tcp (Eio.Net.Ipaddr.V4.loopback, 1337))

(*
   open EffectHandlers
   open EffectHandlers.Deep

   type ('elt, 'container) iter = ('elt -> unit) -> 'container -> unit
   type 'elt gen = unit -> 'elt option

   let generate (type elt) (i : (elt, 'container) iter) (c : 'container) : elt gen
       =
     let module M = struct
       type _ eff += Yield : elt -> unit eff
     end in
     let open M in
     let rec step =
       ref (fun () ->
           i (fun v -> perform (Yield v)) c;
           (step := fun () -> None);
           None)
     in
     let loop () =
       try_with !step ()
         {
           effc =
             (fun (type a) (e : a eff) ->
               match e with
               | Yield v ->
                   Some
                     (fun (k : (a, _) continuation) ->
                       step := continue k;
                       Some v)
               | _ -> None);
         }
     in
     loop

   let gen_list : 'a list -> 'a gen = generate List.iter
   let gl : int gen = gen_list [ 1; 2; 3 ]

   let () =
     assert (Some 1 = gl ());
     assert (Some 2 = gl ());
     assert (Some 3 = gl ());
     assert (None = gl ());
     assert (None = gl ()) *)
