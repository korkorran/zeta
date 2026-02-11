open Mapping.Command

let () = print_endline "Hello, World!"

let () = print "This is a message from the OpenCommand library."

let git = Native_registry.Git.git

let () = print (Printf.sprintf "Registered command: git - %s"
                  (StringMap.find "git" git).description)

let c = parseEncodedCommand git

let () = ignore c
