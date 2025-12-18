let pangram (inFile : string) (outFile : string) : unit =
  (* Here we open an input channel for first argument, inFile,
and bind it to a variable ic so that we can refer it
later in loop_read function. *)
  let ic = open_in inFile in

  (* Use the second argument as file name to open an output channel,
and bind it to variable oc for later reference. *)
  let oc = open_out outFile in 

(* Helper function: file input function. It reads file line by line
and return the result as a list of string. *)
  let rec loop_read acc =
    try 
      let l = input_line ic in
      loop_read (l :: acc)
    with
    | End_of_file -> List.rev acc
  in
(* This variable contains the result of input file from helper
function, loop_read. Please remember this is a list of string. *)
  let ls_str = loop_read [] in


  (* ***** Code From Here, Replace () above and write your code ***** *)
(* Make sure to include this in your submission *)

  let alphabet = ['a';'b';'c';'d';'e';'f';'g';'h';'i';'j';
                  'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';
                  'u';'v';'w';'x';'y';'z'] in

  let rec checking ls_str =
    match ls_str with
    | [] -> ()
    | h::t ->
        let is_pangram = List.for_all (fun a -> String.contains h a) alphabet in
        Printf.fprintf oc "%b\n" is_pangram;  
        checking t
  in

  checking ls_str;

  close_in ic;
  close_out oc

let () =
  pangram "input.txt" "output.txt"
;;
