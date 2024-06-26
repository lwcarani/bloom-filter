(** 
 * BloomFilter module provides an implementation of a Bloom filter,
 * a space-efficient probabilistic data structure used to test whether
 * an element is a member of a set.
 *)
 module BloomFilter = struct
  (** 
   * Type representing a Bloom filter.
   * @param size The number of bits in the filter.
   * @param bits The bit array used to store the filter's state.
   * @param hash_count The number of hash functions used.
   * @param false_pos_prob The desired false positive probability.
   * @param items_count The expected number of items to be inserted.
   *)
  type t = {
    size: int;
    bits: Bytes.t;
    hash_count: int;
    false_pos_prob: float;
    items_count: int;
  }

  (**
   * Creates a new Bloom filter.
   * @param items_count The expected number of items to be inserted.
   * @param false_pos_prob The desired false positive probability.
   * @return A new Bloom filter instance.
   *)
  let create items_count false_pos_prob =
    (* Calculate optimal filter size *)
    let num = (-. (float_of_int items_count)) *. (log false_pos_prob) in 
    let size = (num) /. ((log 2.)**2.) |> ceil |> int_of_float in
    (* Calculate optimal number of hash functions *)
    let hash_count = ((float_of_int size) /. (float_of_int items_count)) *. (log 2.) |> int_of_float in
    { 
      size; 
      bits = Bytes.make size '\000';
      hash_count;
      false_pos_prob;
      items_count;
    }

  (**
   * Computes a hash value for a given string.
   * @param str The string to hash.
   * @param i The index of the hash function (used to create multiple hash functions).
   * @param size The size of the Bloom filter (used to constrain the hash output).
   * @return An integer hash value in the range [0, size-1].
   *)
  let hash str i size =
    let h = Hashtbl.hash (str ^ string_of_int i) in
    h mod size

  (**
   * Adds an element to the Bloom filter.
   * @param filter The Bloom filter to add to.
   * @param str The string to add to the filter.
   *)
  let add filter str =
    List.init filter.hash_count (fun i -> hash str i filter.size)
    |> List.iter (fun index -> Bytes.set filter.bits index '\001')

  (**
   * Checks if an element might be in the Bloom filter.
   * @param filter The Bloom filter to check.
   * @param str The string to check for.
   * @return true if the element might be in the set, false if it definitely is not.
   *)
  let probably_contains filter str =
    let rec check i =
      if i = filter.hash_count then true
      else
        let index = hash str i filter.size in
        if Bytes.get filter.bits index = '\000' then false
        else check (i + 1)
    in
    check 0

  (**
   * Clears all elements from the Bloom filter.
   * @param filter The Bloom filter to clear.
   *)
  let clear filter =
    Bytes.fill filter.bits 0 filter.size '\000'

  let save_to_file filter filename version =
    let oc = open_out_bin filename in
    (* Write identifier (4 bytes) *)
    output_string oc "CCBF";
    
    (* Write version (2 bytes) *)
    output_byte oc (version lsr 8);  (* high byte *)
    output_byte oc (version land 0xFF);  (* low byte *)
    
    (* Write number of hash functions (2 bytes) *)
    output_byte oc (filter.hash_count lsr 8);  (* high byte *)
    output_byte oc (filter.hash_count land 0xFF);  (* low byte *)
    
    (* Write number of bits (4 bytes) *)
    output_byte oc ((filter.size lsr 24) land 0xFF);  (* highest byte *)
    output_byte oc ((filter.size lsr 16) land 0xFF);
    output_byte oc ((filter.size lsr 8) land 0xFF);
    output_byte oc (filter.size land 0xFF);  (* lowest byte *)
    
    (* Write the bit array *)
    output_bytes oc filter.bits;
    close_out oc

  let load_from_file (filename : string) (version_check : int) =
    let ic = open_in_bin filename in
    (* Read and check identifier (4 bytes) *)
    let id = really_input_string ic 4 in
    if id <> "CCBF" then failwith "Invalid file format";

    (* Read version (2 bytes) *)
    let version_bytes = really_input_string ic 2 in
    let version = 
      (int_of_char version_bytes.[0] lsl 8) + 
      int_of_char version_bytes.[1] in
    if version <> version_check then failwith "Unsupported version";

    (* Read number of hash functions (2 bytes) *)
    let hash_count_bytes = really_input_string ic 2 in
    let hash_count = 
      (int_of_char hash_count_bytes.[0] lsl 8) + 
      int_of_char hash_count_bytes.[1] in
    
    (* Read number of bits (4 bytes) *)
    let size_bytes = really_input_string ic 4 in
    let size = 
      (int_of_char size_bytes.[0] lsl 24) +
      (int_of_char size_bytes.[1] lsl 16) +
      (int_of_char size_bytes.[2] lsl 8) +
      int_of_char size_bytes.[3] in
      
    (* Read the bit array *)
    let bits = really_input_string ic size in
    close_in ic;
    { 
      size;
      bits = Bytes.of_string bits;
      hash_count;
      false_pos_prob = 0.; (* We don't have this information in the file, and don't need it *)
      items_count = 0; (* We don't have this information in the file, and don't need it *)
    }
end