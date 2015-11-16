(* Code taken from the original Lacaml distribution.
   Added the copyright from the Lacaml_io.ml file here.

   Copyright (C) 2005-

     Markus Mottl
     email: markus.mottl@gmail.com
     WWW: http://www.ocaml.info

     Jane Street Holding, LLC
     Author: Markus Mottl
     email: markus.mottl@gmail.com
     WWW: http://www.ocaml.info

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

let printers =
  [
    (* Vectors *)
    "Bautop.Pp.Toplevel.pp_rfvec";
    "Bautop.Pp.Toplevel.pp_rcvec";
    "Bautop.Pp.Toplevel.pp_rivec";
    "Bautop.Pp.Toplevel.pp_rinavec";
    "Bautop.Pp.Toplevel.pp_ri32vec";
    "Bautop.Pp.Toplevel.pp_ri64vec";
    "Bautop.Pp.Toplevel.pp_rcharvec";

    (* Matrices *)
    "Bautop.Pp.Toplevel.pp_fmat";
    "Bautop.Pp.Toplevel.pp_cmat";
    "Bautop.Pp.Toplevel.pp_imat";
    "Bautop.Pp.Toplevel.pp_inamat";
    "Bautop.Pp.Toplevel.pp_i32mat";
    "Bautop.Pp.Toplevel.pp_i64mat";
    "Bautop.Pp.Toplevel.pp_charmat";

    (* Array3 *)
    "Bautop.Pp.Toplevel.pp_far3";
    "Bautop.Pp.Toplevel.pp_car3";
    "Bautop.Pp.Toplevel.pp_iar3";
    "Bautop.Pp.Toplevel.pp_inaar3";
    "Bautop.Pp.Toplevel.pp_i32ar3";
    "Bautop.Pp.Toplevel.pp_i64ar3";
    "Bautop.Pp.Toplevel.pp_charar3";
  ]

let eval_string
      ?(print_outcome = false) ?(err_formatter = Format.err_formatter) str =
  let lexbuf = Lexing.from_string str in
  let phrase = !Toploop.parse_toplevel_phrase lexbuf in
  Toploop.execute_phrase print_outcome err_formatter phrase

let rec install_printers = function
  | [] -> true
  | printer :: printers ->
      let () = Printf.eprintf "installing %s" printer in
      let cmd = Printf.sprintf "#install_printer %s;;" printer in
      eval_string cmd && install_printers printers

let () =
  if not (install_printers printers) then
    Format.eprintf "Problem installing LACAML-printers@."
