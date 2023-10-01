(**************************************************************************)
(*  Bitwuzla: Satisfiability Modulo Theories (SMT) solver.                *)
(*                                                                        *)
(*  Copyright (C) 2023 by the authors listed in the AUTHORS file at       *)
(*  https://github.com/bitwuzla/bitwuzla/blob/main/AUTHORS                *)
(*                                                                        *)
(*  This file is part of Bitwuzla under the MIT license.                  *)
(*  See COPYING for more information at                                   *)
(*  https://github.com/bitwuzla/bitwuzla/blob/main/COPYING                *)
(**************************************************************************)

(* The inline tests were moved here to avoid depending on ppx outside tests.  *)
(* The goal of these tests are to catch errors in the function and constant   *)
(* mappings. They do not try to catch functional errors in Bitwuzla.          *)

open Bitwuzla_cxx
open Format

let bool_sort = mk_bool_sort ()
let fun1_1_1_sort = mk_fun_sort [| bool_sort; bool_sort |] bool_sort

let bv8_sort = mk_bv_sort 8

let bv32_sort = mk_bv_sort 32

let ar32_8_sort = mk_array_sort bv32_sort bv8_sort

let%test "version" = 0 <> String.length @@ version ()

let () = eprintf "XXX\n"

let%expect_test "copyright" =
  print_string @@ copyright ();
  [%expect
    {|
    Bitwuzla is a Satisfiability Modulo Theories (SMT) Solver for bit-vectors,
    floating-points, arrays and uninterpreted functions.

    Copyright (C) 2018-2023 by its authors and contributors and their institutional
    affiliations as listed in file AUTHORS.

    MIT License

    Permission is hereby granted, free of charge, to any person obtaining a
    copy of this software and associated documentation files (the "Software"),
    to deal in the Software without restriction, including without limitation
    the rights to use, copy, modify, merge, publish, distribute, sublicense,
    and/or sell copies of the Software, and to permit persons to whom the
    Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included
    in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
    THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
    OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
    ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
    OTHER DEALINGS IN THE SOFTWARE.


    This version of Bitwuzla is linked against the following
    third party libraries. For copyright information of each
    library see the corresponding url.

      CaDiCaL
      https://github.com/arminbiere/cadical

      GMP - GNU Multiple Precision Arithmetic Library
      https://gmplib.org

      SymFPU
      https://github.com/martin-cs/symfpu |}]

let%expect_test "mk_array_sort" =
  Sort.pp Format.std_formatter bv8_sort;
  Gc.full_major ();
  [%expect {| (Array (_ BitVec 32) (_ BitVec 8)) |}]

let%expect_test "mk_array_sort" =
  Sort.pp Format.std_formatter bv32_sort;
  Gc.full_major ();
  [%expect {| (Array (_ BitVec 32) (_ BitVec 8)) |}]

let%expect_test "mk_array_sort" =
  Sort.pp Format.std_formatter ar32_8_sort;
  Gc.full_major ();
  [%expect {| (Array (_ BitVec 32) (_ BitVec 8)) |}]

let%expect_test "mk_array_sort" =
  Sort.pp Format.std_formatter ar32_8_sort;
  Gc.full_major ();
  [%expect {| (Array (_ BitVec 32) (_ BitVec 8)) |}]

let%expect_test "mk_array_sort" =
  Sort.pp Format.std_formatter ar32_8_sort;
  Gc.full_major ();
  [%expect {| (Array (_ BitVec 32) (_ BitVec 8)) |}]


let%expect_test "mk_fun_sort" =
  Gc.full_major ();
  Sort.pp Format.std_formatter fun1_1_1_sort;
  [%expect {| Bool Bool -> Bool |}]

let%expect_test "mk_fun_sort" =
  Gc.full_major ();
  Sort.pp Format.std_formatter fun1_1_1_sort;
  [%expect {| Bool Bool -> Bool |}]
