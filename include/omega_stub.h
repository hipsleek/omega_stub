#ifndef _OMEGA_STUB_H
#define _OMEGA_STUB_H

#include <stdio.h>

extern "C" {
  
  Relation* relation_new0();
  Relation* relation_new1(int n_input);
  Relation* relation_new2(int n_input, int n_output);

  void relation_delete(Relation* r);

  void relation_finalize(Relation* r);

  Variable_ID relation_set_var(Relation* r, int nth);
  Variable_ID relation_input_var(Relation* r, int nth);
  Variable_ID relation_output_var(Relation* r, int nth);
  Variable_ID relation_get_local1(Relation* r, Variable_ID var);
  Variable_ID relation_get_local_global1(Relation* r, Global_Var_ID gvar);
  Variable_ID relation_get_local_global2(Relation* r, Global_Var_ID gvar, Argument_Tuple of);

  void relation_name_set_var(Relation* r, int nth, char* name);
  void relation_name_input_var(Relation* r, int nth, char* name);
  void relation_name_output_var(Relation* r, int nth, char* name);

  F_And* relation_add_and(Relation* r);
  F_Or* relation_add_or(Relation* r);
  F_Not* relation_add_not(Relation* r);
  F_Forall* relation_add_forall(Relation* r);
  F_Exists* relation_add_exists(Relation* r);

  F_And* f_and_add_and(F_And* r);
  F_Or* f_and_add_or(F_And* r);
  F_Not* f_and_add_not(F_And* r);
  F_Forall* f_and_add_forall(F_And* r);
  F_Exists* f_and_add_exists(F_And* r);

  GEQ_Handle* f_and_add_GEQ(F_And* f);
  EQ_Handle* f_and_add_EQ(F_And* f);
  Stride_Handle* f_and_add_stride(F_And* f, int step);

  Variable_ID f_declare(F_Declaration* f, char* name);

  void constraint_handler_update_const(Constraint_Handle* ch, coef_t delta);
  void constraint_handler_update_coef(Constraint_Handle* ch, Variable_ID v, coef_t delta);
  
  void relation_print(Relation* r);
  void relation_print_file(Relation* r, FILE* output_file);
  void relation_print_with_subs(Relation* r, FILE* output_file, bool printSym);
  char* relation_print_with_subs_to_string(Relation* r, bool printSym);
  char* relation_print_formula_to_string(Relation* r);

  Free_Var_Decl* free_var_decl0(char* name);
  Free_Var_Decl* free_var_decl1(char* name, int arity);
}

#endif
