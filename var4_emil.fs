// 1
let rec comp (prev:char) count acc (str: char list) =
match str with
| [] -> acc
| h::t when h = prev -> comp prev (count+1) acc t
| h::t -> comp h 1 ((prev, count)::acc) t

let compress (str: char list) =
let rec comp (prev:char) count acc (str: char list) =
match str with
| [] -> 
List.rev ((prev,count)::acc)
| h::t when h = prev -> comp prev (count+1) acc t
| h::t -> comp h 1 ((prev,count)::acc) t
match str with
| h::t -> comp h 1 [] t
| _ -> []

compress ['a'; 'a'; 'b'; 'b'; 'a']

// 2
let decompress (cps: (char*int) list) =
let rec decomp (acc:char list) cps = 
match cps with
| (c,n)::t -> decomp (List.append acc [for i in 1..n -> c]) t
| _ -> acc
decomp [] cps
