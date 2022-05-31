

type test = int

type 'a test2 = 'a

type ('a, 'b) test3 = 'a * 'b

datatype test4 = A of test4

datatype 'a test5 = Empty | Node of 'a test5 * 'a * 'a test5

datatype ('a, 'b) test6 = Empty | Node of ('a, 'b) test6 * ('a * 'b) * ('a, 'b) test6

(* NJ extension *)
datatype test7 = datatype list

