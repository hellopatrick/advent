open Core

let no_repeats words =
    let set = String.Set.of_list words in
    String.Set.length set = List.length words

let sort_chars word =
    String.to_list word
    |> List.sort ~cmp:Char.compare
    |> String.of_char_list

let sort_words words =
    List.map words ~f:sort_chars

let no_anagrams = Fn.compose no_repeats sort_words

let _ =
    let split_words = String.split ~on:' ' in
    let passphrases = In_channel.read_lines "./passphrases.txt" |> List.map ~f:split_words in

    List.filter passphrases ~f:no_repeats
    |> List.length
    |> printf "a: %d\n";

    List.filter passphrases ~f:no_anagrams
    |> List.length
    |> printf "b: %d\n";