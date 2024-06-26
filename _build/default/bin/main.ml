open Bloom_filter

(* This function works if there is more than one word per line *)
(* let read_words filename =
  let ic = open_in filename in
  let rec read_lines acc =
    try
      let line = input_line ic in
      let words = String.split_on_char ' ' line |> List.map String.trim in
      read_lines (List.append acc words)
    with End_of_file ->
      close_in ic;
      acc
  in
  read_lines [] *)

let read_words filename =
  let ic = open_in filename in
  let rec read_lines acc =
    try
      let word = input_line ic |> String.trim in
      read_lines (word :: acc)
    with End_of_file ->
      close_in ic;
      acc
  in
  read_lines []

let build_filter input_file output_file version =
  let words = read_words input_file in
  let word_count = List.length words in
  
  (* Create a Bloom filter with 0.01% false positive rate *)
  let filter = BloomFilter.create word_count 0.0001 in
  
  (* Insert all words into the Bloom filter *)
  List.iter (BloomFilter.add filter) words;
  
  (* Save the Bloom filter to a file *)
  BloomFilter.save_to_file filter output_file (int_of_string version);
  
  Printf.printf "Processed %d words from %s\n" word_count input_file;
  Printf.printf "Bloom filter saved to %s (version %s)\n" output_file version

let check_words filter_file version words =
  let filter = BloomFilter.load_from_file filter_file version in
  List.iter (fun word ->
    let result = BloomFilter.probably_contains filter word in
    Printf.printf "%s: %s\n" word (if result then "Probably in set" else "Not in set")
  ) words

let () =
  match Array.to_list Sys.argv with
  | _ :: "-build" :: input_file :: output_file :: version :: _ ->
      build_filter input_file output_file version
  | _ :: filter_file :: version :: words when List.length words > 0 ->
      check_words filter_file (int_of_string version) words
  | _ ->
      Printf.printf "Usage:\n";
      Printf.printf "  Build: %s -build <input_file.txt> <output_file.bf> <version>\n" Sys.argv.(0);
      Printf.printf "  Check: %s <filter_file.bf> word1 word2 ...\n" Sys.argv.(0);
      exit 1