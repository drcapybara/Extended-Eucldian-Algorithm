(* 
    This is a simple functional style OCAML implementation of
    the Euclidian Algorithm. I chose OCAML because of the simplicity
    that functional languages offer in writing what can be larger
    and more complex code pieces in other languages. This program
    accepts user input and returns the modular multiplicative inverse
    of the 2 integers entered. 

    Author: Dustin Ray
    TCSS 581
    Winter 2020
*)

print_string "\nOCAML Extended Euclidian Algorithm v1.0 \n\n";;

(*  Function solves Bezouts Identity of the form 
        aS + bT = GCD(a, b) 
    We accept a and b as input, and recursively
    mod a by b, mapping d (the divisor) along with
    S and T to the function d, t, s - the result of
    a divided by (b multipled by t). If b is zero, we exit
    recursion and return a and 1 as coefficients and zero as GCD.*)
let rec ex_euclid a b =
	
    if b = 0 then a, 1, 0
	
    else match (ex_euclid b (a mod b)) with
		(d, s, t) -> d, t, s - a/b*t;;


print_string "Enter an Integer (a): \n";;
    let a = read_int();;

print_string "Enter an integer (b): \n";;
    let b = read_int();;

print_string "Results as follows:\n";;

match ex_euclid a b with
    (d, t, s) -> Printf.printf "
    (GCD): %d\n 
    (Coefficient for bigger integer): %d\n 
    (Coefficient for smaller integer): %d\n\n" 
    d s t;;
