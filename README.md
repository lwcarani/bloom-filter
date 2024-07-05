# bloom_filter

## About
A bloom filter is a space-efficient, probabilistic data structure used to test whether an element is a member of a set. For this repo, I implemented a bloom filter in OCaml that we can use as a spell checker. `bloom_filter` can be used from the command line. 

Bloom filters are really quite clever data structures. They are memory-efficient, because instead of storing an entire set of particular values, we store a `bit array` of `m` bits. The trade-off here is that although we create a set that is more memory-efficient, we may get false positives when checking if an element is a member of our set (bloom filter). Let's see why.

We start with an empty bit array, with all bits set to 0. Then, for every element we want to add to the bloom filter, we hash it with `k` hash functions to compute `k` hash values for the given value. Suppose we have three hash functions, a bit array of length `m`, and we are adding the word `"walrus"` to the bloom filter. We first compute the three hash values:

```
hash1("walrus") % m = 1
hash2("walrus") % m = m - 10
hash3("walrus") % m = m - 3
```

Next, we locate the bits at index `1`, index `m - 10`, and index `m - 3`, and set them to 1. Then later, if we want to check if `"walrus"` is in the set, we reverse the process. We calculate the respective hash values using `hash1`, `hash2`, and `hash3`, and check if these indices are all set to 1 in the bit array. If all the bits are set to 1, we can say that the value `"walrus"` is *probably present*. If any of the bits at these indices are set to 0 we can say that `"walrus"` is *definitely not present* in the set. 

The reason for this is that separately, other values could end up flipping the same bits at index `1`, index `m - 10`, and index `m - 3` to 1, without ever adding `"walrus"` to the set, that's why we must say an element is *probably present*. But, if we ever encounter a zero bit when checking for membership we can say that element **definitely** does not exist in the set, because we would have flipped that zero bit to one at the time we added that element to the set.

## Implementation

Here's the OCaml code that implements the bloom filter module:

```ocaml
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
```

## Examples

First, we can run all tests directly from the command line to make sure everything is working correctly:

```cmd
lwcarani@DESKTOP:~/bloom_filter$ dune test
.....                              
Ran: 5 tests in: 0.11 seconds.
OK
```

Next, let's create a new bloom filter, and load some words so we can test it! By including the `-build` flag, a path to a file containing all the words you want to add to the bloom filter, the name you want for the generated bloom filter, and the version number, you can create a new bloom filter directly from the command line:

```cmd
lwcarani@DESKTOP:~/bloom_filter$ dune exec bloom_filter -- -build words.txt words.bf 1
Processed 466549 words from words.txt
Bloom filter saved to words.bf (version 1)
```

Now let's use the `xxd` tool to check that the bloom filter was generated correctly:

```cmd
lwcarani@DESKTOP:~/bloom_filter$ xxd -l 32 words.bf
00000000: 4343 4246 0001 000d 0088 78b7 0001 0101  CCBF......x.....
00000010: 0001 0001 0001 0001 0001 0001 0101 0101  ................
```

- The first four bytes are a unique identifier (CCBF in this case, since I was following the [coding challenges](https://codingchallenges.substack.com/p/coding-challenge-53-bloom-filter) prompt)
- The next two bytes are the version number (in this example, 1)
- The next two bytes are the number of hash functions used for this bloom filter (in this example, 13)
- The next four bytes will be the number of bits used for the filter (in this example, `0x008878b7` bits, or `8,943,799` bits in decimal)

Now, the user can load the newly generated bloom filter, specify the version number, and any words that the user wants to check and see if they are included in the bloom filter:

```cmd
lwcarani@DESKTOP:~/bloom_filter$ dune exec bloom_filter words.bf 1 foobar baz apple imadethisup coding challenge
foobar: Not in set                
baz: Not in set
apple: Not in set
imadethisup: Not in set
coding: Probably in set
challenge: Probably in set
```

```cmd
lwcarani@DESKTOP:~/bloom_filter$ dune exec bloom_filter words.bf 1 walrus aardvark lion biscuit ocean ship brunch
walrus: Probably in set           
aardvark: Probably in set
lion: Not in set
biscuit: Probably in set
ocean: Probably in set
ship: Probably in set
brunch: Probably in set
```

```
If you pass in the wrong version number, you'll get an error:
```cmd
lwcarani@DESKTOP:~/bloom_filter$ dune exec bloom_filter words.bf 42 walrus aardvark lion biscuit ocean ship brunch
Fatal error: exception Failure("Unsupported version")
```

## Acknowledgements
Thanks to [John Crickett](https://github.com/JohnCrickett) for the idea from his site, [Coding Challenges](https://codingchallenges.substack.com/p/coding-challenge-53-bloom-filter)!

Feedback, bug reports, issues, and pull requests welcome!
