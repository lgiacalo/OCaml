(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   encode.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ybarbier <ybarbier@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2016/06/15 13:27:08 by ybarbier          #+#    #+#             *)
(*   Updated: 2016/06/16 13:55:22 by ybarbier         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec encode lst = match lst with
    | hd::tl -> (
        let rec loop lst el c acc = match lst with
        | hd::tl -> if (hd = el) then loop tl hd (c+1) acc else loop tl hd 1 (acc @ [(c, el)])
        | [] -> acc @ [(c, el)]
        in loop lst hd 0 []
    )
    | [] -> []

(*--INT--*)
let rec print_list_int lst = match lst with
    | hd::tl -> (
        print_int hd;
        print_string "; ";
        print_list_int tl
    )
    | [] -> print_string "\n"

let rec print_list_tuple_int lst = match lst with
    | hd::tl -> (
        let (n, x) = hd in
        print_string "(";
        print_int n;
        print_string ", ";
        print_int x;
        print_string ")";
        print_string "; ";
        print_list_tuple_int tl
    )
    | [] -> print_string "\n"

let rec print_list_encoded_int lst = match lst with
    | hd::tl -> (
        let (n, x) = hd in
        let rec print_loop_int n x tl =
        if n > 0 then (
            print_int x;
            print_loop_int (n-1) x tl
        )
        else print_list_encoded_int tl
        in print_loop_int n x tl
    )
    | [] -> print_string "\n"

(*--STRING--*)
let rec print_list_string lst = match lst with
    | hd::tl -> (
        print_string hd;
        print_string "; ";
        print_list_string tl
    )
    | [] -> print_string "\n"

let rec print_list_tuple_string lst = match lst with
    | hd::tl -> (
        let (n, x) = hd in
        print_string "(";
        print_int n;
        print_string ", ";
        print_string x;
        print_string ")";
        print_string "; ";
        print_list_tuple_string tl
    )
    | [] -> print_string "\n"

let rec print_list_encoded_string lst = match lst with
    | hd::tl -> (
        let (n, x) = hd in
        let rec print_loop_string n x tl =
        if n > 0 then (
            print_string x;
            print_loop_string (n-1) x tl
        )
        else print_list_encoded_string tl
        in print_loop_string n x tl
    )
    | [] -> print_string "\n"


let main () =
    (*--INT--*)
    let list_int1 = [2; 2; 2; 2; 42; 42; 100; 101] in
    print_string "List: ";
    print_list_int list_int1;
    print_string "Tuple: ";
    print_list_tuple_int (encode list_int1);
    print_string "Decode: ";
    print_list_encoded_int (encode list_int1);
    print_string "\n";


    let list_int2 = [2] in
    print_string "List: ";
    print_list_int list_int2;
    print_string "Tuple: ";
    print_list_tuple_int (encode list_int2);
    print_string "Decode: ";
    print_list_encoded_int (encode list_int2);
    print_string "\n";

    let list_int3 = [] in
    print_string "List: ";
    print_list_int list_int3;
    print_string "Tuple: ";
    print_list_tuple_int (encode list_int3);
    print_string "Decode: ";
    print_list_encoded_int (encode list_int3);
    print_string "\n";

    (*--STRING--*)
    let list_string1 = ["a";"a";"a";"b";"b";"b";"a"] in
    print_string "List: ";
    print_list_string list_string1;
    print_string "Tuple: ";
    print_list_tuple_string (encode list_string1);
    print_string "Decode: ";
    print_list_encoded_string (encode list_string1);
    print_string "\n";

    let list_string2 = ["OK"] in
    print_string "List: ";
    print_list_string list_string2;
    print_string "Tuple: ";
    print_list_tuple_string (encode list_string2);
    print_string "Decode: ";
    print_list_encoded_string (encode list_string2);
    print_string "\n";

    let list_string3 = ["Yay"; "Yay"; "YO";"Hey";"Hey"] in
    print_string "List: ";
    print_list_string list_string3;
    print_string "Tuple: ";
    print_list_tuple_string (encode list_string3);
    print_string "Decode: ";
    print_list_encoded_string (encode list_string3);
    print_string "\n"


let () = main ()