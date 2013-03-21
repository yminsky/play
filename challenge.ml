open Core.Std

let rec apply (start:'a) (f:'a->'a) (test:'a->bool) : 'a =
  if test start
  then start
  else apply (f start) f test
;;


let () = assert (apply 0 (fun x -> x + 5) (fun x -> x > 12) = 15)
let () = assert (apply 0 (fun x -> x + 5) (fun x -> x > -10) = 0)
let () = assert (apply "foo" (fun x -> x ^ x) (fun x -> String.length x > 10)
		 = "foofoofoofoo")
let () = assert (apply 3 (fun x -> x * 2) (fun x -> x > 20)
		 = 24)
let () = assert (apply 3 (fun qbert -> qbert * 2) (fun y -> y > 1)
		 = 3)

let rec sum l =
  match l with
  |[] -> 0
  |f :: e -> f + sum e

let () = assert (sum [1;2;3] = 6)
let () = assert (sum [] = 0)


let rec hfind_smallest l s =
  match l with
  |[]->  s
  |f :: e -> if f < s then hfind_smallest e f else hfind_smallest e s

let find_smallest l =
  match l with
  |[]-> assert false
  |f :: e ->  hfind_smallest e f

let () = printf "%d\n" (find_smallest [1;2;3])
let () = printf "%d\n" (find_smallest [2;1;3])

let () = assert (find_smallest [1;2;3]    = 1)
let () = assert (find_smallest [3;2;9;20] = 2)
let () = assert (try ignore (find_smallest []); false with _ -> true)

