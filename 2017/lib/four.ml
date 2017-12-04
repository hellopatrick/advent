open Core

let all_unique_words words =
    let set = String.Set.of_list words in
    String.Set.length set = List.length words

let sort_chars word =
    String.to_list word
    |> List.sort ~cmp:Char.compare
    |> String.of_char_list

let sort_words words =
    List.map words ~f:sort_chars

let all_anagrams = Fn.compose all_unique_words sort_words

let solve () =
    let split_words = String.split ~on:' ' in
    let passphrases = In_channel.read_lines "./2017/data/4.txt" |> List.map ~f:split_words in

    List.filter passphrases ~f:all_unique_words
    |> List.length
    |> printf "a: %d\n";

    List.filter passphrases ~f:all_anagrams
    |> List.length
    |> printf "b: %d\n";