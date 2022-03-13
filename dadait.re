open Lwt
open Cohttp_lwt_unix
open Soup
open Str

Random.init(int_of_float(Unix.time()));

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

let rand_get_lines =  (max_words: int, words: array(string)) : array(string) => {
  let rec process = (counter: int, acc: array(string)) : array(string) => {
    let limit = Random.int(max_words)
    let delim = counter + limit;
    switch(Array.length(words) > delim) {
      | true =>
          Array.sub(words, counter, limit)
          |> join_words
          |> Array.make(1)
          |> Array.append(acc) 
          |> process(counter+(limit))
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

let rdn_cut_up = (max_words: int, text: string) : array(string) => {
  let words =  regexp(" ") |> split(_, text) |> Array.of_list
  let rec process = (counter: int, acc: array(string)) : array(string) => {
    let limit = Random.int(max_words)
    let delim = counter + limit;
    switch(Array.length(words) > delim) {
      | true =>
          // TODO: consider trimming extra white space here
          Array.sub(words, counter, limit)
          |> join_words
          |> Array.make(1)
          |> Array.append(acc) 
          |> process(counter+(limit))
      | false => acc
    }
  }

  process(0, [||])
}

let combine_halfs = (left: array(string), right: array(string)) : array(string) => {
  left |> Array.mapi((idx, line) => line ++ " " ++ right[idx])
} 

let shuffle = (words: array(string)) : array(string) => {
  let len = Array.length(words)
  let rec process = (idx: int) : array(string) => {
    if (idx > 0) {
      let r = Random.int(idx+1)
      let temp = words[idx]
      words[idx] = words[r]
      words[r] = temp
      process(idx-1)
    } else {
      words
    }
  }

  process(len-1)
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

let get_stanzas = (num_lines: int, lines: array(string)) => {
  let len = Array.length(lines)
  let start = Random.int(len-1)
  if (start + num_lines > len-1) {
    Array.sub(lines, start, len)
  } else {
    Array.sub(lines, start, num_lines)
  }
}

let create_cut_up = (url: string, max: int) : string => {
  fetchBody(url)
    |> Lwt_main.run
    |> text_from_body
    |> rdn_cut_up(max)
    |> shuffle
    |> rand_get_lines(max + 4)
    |> get_stanzas(12) 
    // |> Array.iter(print_endline)
    |> Array.fold_left((res, line)  => res ++ "\n" ++ "<p>" ++ line ++ "</p>", "")
    |> global_replace(regexp("[\"\\(\\)]"), "")
    |> global_replace(regexp("  +/g"), " ")
        //  |> global_replace(regexp("^.*[\\(\\)].*$"), "")
}

let get_template = (file: string) => {
  let file_in_ch = open_in(file);
  let fs = Stream.from(_i => {
    switch(input_line(file_in_ch)) {
      | line => Some(line)
      | exception(End_of_file) => None
    };
  });

  let rec process = (text: string) : string => {
     switch(Stream.next(fs)) {
       | line => process(text ++ line)
       | exception Stream.Failure => text
     }
  }

  process("")
}
// let url_left = Array.get(Sys.argv, 1)
// let url_right = Array.get(Sys.argv, 2)
// let words_num = Array.get(Sys.argv, 3)

// int_of_string(words_num) |> create_fold_up(url_left, url_right)
//   |> print_string

let cu = create_cut_up("https://theanarchistlibrary.org/library/david-graeber-what-s-the-point-if-we-can-t-have-fun-2?utm_source=pocket_mylist", 4) 
let t = Unix.localtime (Unix.time ())
let (day, month, year) = (t.tm_mday, t.tm_mon, t.tm_year)
let ds = Printf.sprintf("Generated on %04d-%02d-%02d\n", (1900 + year), (month + 1), day)
let tmpl = get_template("./template.html")
let idx = global_replace(regexp("POEMHERE"), cu, tmpl) |> global_replace(regexp("DATEHERE"), ds)

let out = open_out("index.html")
Printf.fprintf(out, "%s\n", idx)
close_out(out)


// let soup = parse(tmpl)
// let tn = create_text("yayyyy")
// let r = wrap((soup $ ".poem" |> R.child),(create_element("p", ~inner_text="asasasasasasasasasasasasasasasasas")))
// soup |> to_string |> print_endline
// "https://www.theatlantic.com/magazine/archive/2022/04/jack-kerouac-neal-cassady-friendship/622829/"
// "https://www.theatlantic.com/international/archive/2022/03/afghanistan-withdrawal-left-behind-women-soldiers/627022/"