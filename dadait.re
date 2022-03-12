open Lwt
open Cohttp_lwt_unix
open Soup
open Str

let join_words = (arr) =>
  Array.fold_left((res, word)  => res ++ word ++ " ", "", arr)
  |> String.trim

let fetchBody = (url) => {
  let url = Uri.of_string(url);
  Client.get(url) >>= (
    ((_, body)) =>  Cohttp_lwt.Body.to_string(body)
  );
};

let text_from_body = (body: string) : string => {
  let soup = parse(body)
  let blocks = soup $$ "p"
    |> to_list
    |> List.map(li => trimmed_texts(li) |> String.concat(""))
  
  List.fold_left((a, b) => a ++ " " ++ b, List.hd(blocks), List.tl(blocks));
}

let get_lines =  (max_words: int, text: string) : array(string) => {
  let words =  regexp(" ") |> split(_, text) |> Array.of_list
  let rec process = (counter: int, acc: array(string)) : array(string) => {
    let delim = counter + max_words;
    switch(Array.length(words) > delim) {
      | true =>
          Array.sub(words, counter, max_words)
          |> join_words
          |> Array.make(1)
          |> Array.append(acc) 
          |> process(counter+(max_words))
      | false => acc
    }
  }

  process(0, [||])
}

let cut_strings = (left: bool, by: int, lines: array(string)) : array(string) => {
  lines |> Array.map(line => {
    let words =  regexp(" ") |> split(_, line) |> Array.of_list
    switch(left) {
      | true => Array.sub(words, 0, by) |> join_words
      | false => 
          let l = Array.length(words) 
          Array.sub(words, by, l-by) 
          |> join_words
    };
  })
}

let combine_halfs = (left: array(string), right: array(string)) : array(string) => {
  left |> Array.mapi((idx, line) => line ++ " " ++ right[idx])
} 

let create_fold_up = (url_left: string, url_right: string, line_size: int) : string => {
  let left_half = fetchBody(url_left)
    |> Lwt_main.run
    |> text_from_body
    |> get_lines(line_size)
    |> cut_strings(true, line_size)  
  let right_half = fetchBody(url_right)
    |> Lwt_main.run
    |> text_from_body
    |> get_lines(line_size)
    |> cut_strings(false, line_size/2)

    combine_halfs(left_half, right_half)
      |> Array.fold_left((res, line)  => res ++ "\n" ++ line, "")
}


let url_left = "https://www.theatlantic.com/magazine/archive/2022/04/jack-kerouac-neal-cassady-friendship/622829/"
let url_right = "https://www.theatlantic.com/international/archive/2022/03/afghanistan-withdrawal-left-behind-women-soldiers/627022/"
create_fold_up(url_left, url_right, 16)
  |> print_string
