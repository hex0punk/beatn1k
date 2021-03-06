open Lwt
open Cohttp_lwt_unix
open Soup
open Str

// Global variables
let cutup_counter = "./web/cutup_count.txt"
let foldup_counter = "./web/foldup_count.txt"
let file_sources = "./web/sources.txt"
let file_template = "./web/template.html"
let file_fold_up = "web/fold-up.html"
let file_index = "web/index.html"

// Initialization of structures needed for multiple functions
Random.init(int_of_float(Unix.time()));

let update_file = (path: string, text: string) : unit => {
  let out = open_out(path)
  Printf.fprintf(out, "%s\n", text)
  close_out(out)
}

let get_date_str = () : string => {
  let t = Unix.localtime (Unix.time ())
  let (day, month, year) = (t.tm_mday, t.tm_mon, t.tm_year)
  Printf.sprintf("Generated on %04d-%02d-%02d at %02d:%02d UTC\n", (1900 + year), (month + 1), day, t.tm_hour, t.tm_min)
}

let join_words = (arr: array(string)) : string => {
  Array.fold_left((res, word) => 
    // fix to a weird bug
    res ++ word ++ " " |> global_replace(regexp("\n"), ""),"", arr) 
  |> String.trim
}

let fetchBody = (url) => {
  let url = Uri.of_string(url);
  Client.get(url) >>= (
    ((_, body)) =>  Cohttp_lwt.Body.to_string(body)
  );
};

let text_from_body = (body: string) : string => {
  let soup = parse(body)
  let blocks = soup 
  |> select("p")
  |> to_list
  |> List.map(li => trimmed_texts(li) |> String.concat(""))
  
  List.fold_left((a, b) => a ++ " " ++ b, List.hd(blocks), List.tl(blocks));
}

let cut_array = (arr: array(string), from: int, len: int, res: array(string)) : array(string) => {
  Array.sub(arr, from, len)
  |> join_words
  |> Array.make(1)
  |> Array.append(res)
}

let rand_partition = (len: int, arr: array(string)) : array(string) => {
  let rnd = Random.int(len-1)
  Array.sub(arr, rnd, len)
}

let rand_int = (max) => {
  let rnd = Random.int(max);
  if (rnd == 0) 1 else rnd
}

let section = (~rand=false, ~max: int, arr: array(string), ()) : array(string) => {
  let rec pick = (counter: int, acc: array(string)) : array(string) => {
    let limit = if (rand) rand_int(max) else max;
    let delim = counter + limit;
    // I like switches, better than turtles
    switch(Array.length(arr)-1 > delim) {
    | true => cut_array(arr, counter, limit, acc) |> pick(counter+(limit))
    | false => acc
    }
  }
  pick(0, [||])
}

let rand_section = section(~rand=true)

let words_from_text = (text: string) : array(string) => {
  regexp(" ") |> split(_, text) |> Array.of_list
}

let cut_lines = (left: bool, by: int, lines: array(string)) : array(string) => {
  lines |> Array.map(line => {
    let words =  regexp(" ") |> split(_, line) |> Array.of_list
    let l = Array.length(words) 
    switch(left) {
    | true => Array.sub(words, 0, by) |> join_words
    | false => Array.sub(words, by, l-by) |> join_words
    };
  })
}

let combine_halfs = (left: array(string), right: array(string)) : array(string) => {
  left |> Array.mapi((idx, line) => {
    if (idx < Array.length(right) - 2){
      (line ++ " " ++ right[idx])
    } else ""
  })
} 

let shuffle_array = (words: array(string)) : array(string) => {
  let len = Array.length(words)
  let rec shuffle = (idx: int) : array(string) => {
    if (idx > 0) {
      let r = Random.int(idx+1)
      let temp = words[idx]
      words[idx] = words[r]
      words[r] = temp
      shuffle(idx-1)
    } else {
      words
    }
  }

  shuffle(len-1)
}

let lines_from_url = (url: string, line_size: int) : array(string) => {
  fetchBody(url)
  |> Lwt_main.run
  |> text_from_body
  |> words_from_text
  |> section(~max=line_size, _, ()) 
}

let get_stanzas = (num_lines: int, lines: array(string)) => {
  let len = Array.length(lines);
  let start = Random.int(len-1);
  (start + num_lines > len-1) ? Array.sub(lines, start, len) : Array.sub(lines, start, num_lines);
}


let format_text = (web: bool, lines: array(string)) : string => {
  switch (web) {
  | true => Array.fold_left((res, line)  => res ++ "\n" ++ "<p>" ++ line ++ "<p>", "", lines)
  | false => Array.fold_left((res, line)  => res ++ "\n" ++ line, "", lines) 
  };
}

let clean_text = (text: string) : string => {
  text
  |> global_replace(regexp("[\"\\(\\)]"), "")
  |> global_replace(regexp("  +/g"), " ")
}

let create_fold_up = (web: bool, url_left: string, url_right: string, line_size: int, stanza_s: int) : string => {
  let left_half = lines_from_url(url_left, line_size) |> cut_lines(true, line_size/2)
  let right_half = lines_from_url(url_right, line_size) |> cut_lines(false, line_size/2)

  combine_halfs(left_half, right_half)
  |> rand_partition(stanza_s)
  |> format_text(web)
}

let create_cut_up = (web: bool, url: string, max_words: int, max_line: int, stanza_s: int) : string => {
  fetchBody(url)
  |> Lwt_main.run
  |> text_from_body
  |> words_from_text
  |> rand_section(~max=max_words, _, ())
  |> shuffle_array
  |> rand_section(~max=max_line, _, ())
  |> get_stanzas(stanza_s) 
  |> format_text(web)
  |> clean_text
}

let get_file = (file: string) => {
  let file_in_ch = open_in(file);
  let fs = Stream.from(_i => {
    switch(input_line(file_in_ch)) {
    | line => Some(line)
    | exception(End_of_file) => None
    };
  });

  let rec process = (text: string) : string => {
     switch(Stream.next(fs)) {
     | line => process(text ++ line ++ "\n")
     | exception Stream.Failure => text
     }
  }

  process("")
}

let random_source = (text: string) : string => {
  let sources =  regexp("\n") |> split(_, text) |> Array.of_list
  let idx = Array.length(sources) |> Random.int
  sources[idx]
}

let random_foldup_sources = (text: string) : (string, string) => {
  let sources =  regexp("\n") |> split(_, text) |> Array.of_list
  let idx_left = Array.length(sources) |> Random.int
  let idx_right = Array.length(sources) |> Random.int
  if (idx_left == idx_right) {
    if (idx_right == 0) {
      (sources[idx_left], sources[idx_right + 1])
    } else {
      // safe to go down the index by one on idx_right then
      (sources[idx_left], sources[idx_right - 1])
    }
  } else {
    (sources[idx_left], sources[idx_right])
  }
}

let source_footer = (~source_r="", ~source_l: string) : string => {
  if (source_r != ""){
    "Source Left: " ++ source_l ++ "<br>Source Right: " ++ source_r
  } else {
    "Source: " ++ source_l
  }
}

let to_html = (~source_r="", source_l: string, text: string, counter_file: string, out_file: string) : unit => {
  let run_num = (get_file(counter_file) |> String.trim |> int_of_string) + 1 |> string_of_int
  let tmpl = get_file(file_template)
  let ds = get_date_str()
  let text_type = source_r == "" ? "Cut-up" : "Fold-up"
  global_replace(regexp("POEMHERE"), text, tmpl)
  |> global_replace(regexp("SOURCEHERE"), source_footer(~source_r, ~source_l)) 
  |> global_replace(regexp("RUNHERE"), text_type ++ " #" ++ run_num)
  |> global_replace(regexp("DATEHERE"), ds)
  |> update_file(out_file)

  update_file(counter_file, run_num)
}

let all_sources = get_file(file_sources)

let run_cutup = (~web: bool, url: string, max_words: int, max_line: int, stanza_s: int) => {
  let source = ref("")
  if (url == "") {
    source := random_source(all_sources);
  } else {
    source := url
  }
  print_string("Using source " ++ source^ ++ "\n")
  let cu = create_cut_up(web, source^, max_words, max_line, stanza_s) 
  web ? to_html(source^, cu, cutup_counter, file_index) : print_endline(cu)
}

let run_foldup = (~web: bool, url_l: string, url_r: string, max_line: int, stanza_s: int) => {
  let source_l = ref("")
  let source_r = ref("")
  if (url_l == "" || url_r == ""){
    let (url_left, url_right) = random_foldup_sources(all_sources)
    if (url_l == "") {
      source_l := url_left
    } 
    if (url_r == "") {
      source_r := url_right
    }
  }
  let fu = create_fold_up(web, source_l^, source_r^, max_line, stanza_s)
  web ? to_html(~source_r=source_r^, source_l^, fu, foldup_counter, file_fold_up) : print_endline(fu)
}