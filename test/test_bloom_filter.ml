open OUnit2
open Bloom_filter

let test_bloom_filter_init _ =
  let filter = BloomFilter.create 1000 0.1 in

  assert_bool "Size should be 4793" (filter.size == 4793);
  assert_bool "Number of hash functions should be 3" (filter.hash_count == 3);
  assert_bool "Acceptable false positive probability should be 0.1" (filter.false_pos_prob == 0.1);
  assert_bool "Number of expected items to be inserted in filter should be 1000" (filter.items_count == 1000);

  let filter = BloomFilter.create 943532 0.0001 in

  assert_bool "Size should be 18087619" (filter.size == 18087619);
  assert_bool "Number of hash functions should be 13" (filter.hash_count == 13);
  assert_bool "Acceptable false positive probability should be 0.0001" (filter.false_pos_prob == 0.0001);
  assert_bool "Number of expected items to be inserted in filter should be 943532" (filter.items_count == 943532);;

  let test_bloom_filter _ =
    let filter = BloomFilter.create 1000 0.1 in
  
    BloomFilter.add filter "apple";
    BloomFilter.add filter "banana";
    BloomFilter.add filter "aardvark";
    BloomFilter.add filter "zulu";
    BloomFilter.add filter "abcdefghijklmnop";
    BloomFilter.add filter "helloworldfoobarbaz";
    BloomFilter.add filter "foo";
    BloomFilter.add filter "bar";
    
    assert_bool "Should contain apple" (BloomFilter.probably_contains filter "apple");
    assert_bool "Should contain banana" (BloomFilter.probably_contains filter "banana");
    assert_bool "Should contain aardvark" (BloomFilter.probably_contains filter "aardvark");
    assert_bool "Should contain zulu" (BloomFilter.probably_contains filter "zulu");
    assert_bool "Should contain abcdefghijklmnop" (BloomFilter.probably_contains filter "abcdefghijklmnop");
    assert_bool "Should contain helloworldfoobarbaz" (BloomFilter.probably_contains filter "helloworldfoobarbaz");
    assert_bool "Should contain foo" (BloomFilter.probably_contains filter "foo");
    assert_bool "Should contain bar" (BloomFilter.probably_contains filter "bar");
    assert_bool "Should not contain cherry" (not (BloomFilter.probably_contains filter "cherry"));
    
    BloomFilter.clear filter;
    assert_bool "Should not contain apple after clear" (not (BloomFilter.probably_contains filter "apple"));;

  let test_bloom_filter_add _ =
    let filter = BloomFilter.create 5 0.5 in

    BloomFilter.add filter "apple";

    assert_bool "Should be 6" ((BloomFilter.hash "apple" filter.hash_count filter.size) = 6);
    assert_bool "Should match" ((filter.bits) = (Bytes.of_string "\000\000\000\000\000\001\000\000"));

    BloomFilter.add filter "zoo";
    assert_bool "Should be 3" ((BloomFilter.hash "zoo" filter.hash_count filter.size) = 3);
    assert_bool "Should match" ((filter.bits) = (Bytes.of_string "\000\000\000\001\000\001\000\000"));
    
    assert_bool "Should contain apple" (BloomFilter.probably_contains filter "apple");
    assert_bool "Should contain zoo" (BloomFilter.probably_contains filter "zoo");
    assert_bool "Should not contain cherry" (not (BloomFilter.probably_contains filter "cherry"));
    
    BloomFilter.clear filter;
    assert_bool "Should not contain apple after clear" (not (BloomFilter.probably_contains filter "apple"));;

  let test_bloom_filter_hash _ =
    let filter = BloomFilter.create 1000 0.0001 in
    assert_bool "Should be 442" ((BloomFilter.hash "apple" 1 filter.size) = 442);
    assert_bool "Should be 18958" ((BloomFilter.hash "apple" 2 filter.size) = 18958);;

  let test_bloom_filter_load _ =
    let filter = BloomFilter.load_from_file "/home/lwcarani/bloom_filter/test.bf" 42 in

    assert_bool "Should contain AARC" (BloomFilter.probably_contains filter "AARC");
    assert_bool "Should contain aardvark" (BloomFilter.probably_contains filter "aardvark");
    assert_bool "Should contain aardvarks" (BloomFilter.probably_contains filter "aardvarks");
    assert_bool "Should contain aardwolf" (BloomFilter.probably_contains filter "aardwolf");
    assert_bool "Should contain Aarika" (BloomFilter.probably_contains filter "Aarika");
    assert_bool "Should contain Aarhus" (BloomFilter.probably_contains filter "Aarhus");
    assert_bool "Should contain aardwolves" (BloomFilter.probably_contains filter "aardwolves");
    assert_bool "Should not contain zillow" (not (BloomFilter.probably_contains filter "zillow"));
    assert_bool "Should not contain foo" (not (BloomFilter.probably_contains filter "foo"));
    assert_bool "Should not contain zoo" (not (BloomFilter.probably_contains filter "zoo"));
    assert_bool "Should not contain bar" (not (BloomFilter.probably_contains filter "bar"));
    assert_bool "Should not contain baz" (not (BloomFilter.probably_contains filter "baz"));
    
    BloomFilter.clear filter;
    assert_bool "Should not contain aardwolf after clear" (not (BloomFilter.probably_contains filter "aardwolf"));;
  
let suite =
  "BloomFilter Tests" >::: [
    "test_bloom_filter_init" >:: test_bloom_filter_init;
    "test_bloom_filter" >:: test_bloom_filter;
    "test_bloom_filter_load" >:: test_bloom_filter_load;
    "test_bloom_filter_add" >:: test_bloom_filter_add;
    "test_bloom_filter_hash" >:: test_bloom_filter_hash;
  ]

let () = run_test_tt_main suite