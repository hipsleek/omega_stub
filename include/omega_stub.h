/*
 * $Id: omega_stub.h,v 1.10 2003-07-28 09:38:05 raz Exp $
 */

#ifndef _OMEGA_STUB_H
#define _OMEGA_STUB_H

#include <stdio.h>

extern "C" {
  
  Relation* relation_new0();
  Relation* relation_new1(int n_input);
  Relation* relation_new2(int n_input, int n_output);
  Relation* relation_copy(Relation* r);

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

  bool relation_is_set(Relation* r);
  int relation_n_inp(Relation* r);
  int relation_n_out(Relation* r);
  int relation_n_set(Relation* r);

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

  F_And* f_or_add_and(F_Or* f);
  F_Or* f_or_add_or(F_Or* r);
  F_Not* f_or_add_not(F_Or* r);
  F_Forall* f_or_add_forall(F_Or* r);
  F_Exists* f_or_add_exists(F_Or* r);

  F_And* f_not_add_and(F_Not* f);
  F_Or* f_not_add_or(F_Not* r);
  F_Not* f_not_add_not(F_Not* r);
  F_Forall* f_not_add_forall(F_Not* r);
  F_Exists* f_not_add_exists(F_Not* r);

  F_And* f_forall_add_and(F_Forall* f);
  F_Or* f_forall_add_or(F_Forall* r);
  F_Not* f_forall_add_not(F_Forall* r);
  F_Forall* f_forall_add_forall(F_Forall* r);
  F_Exists* f_forall_add_exists(F_Forall* r);

  F_And* f_exists_add_and(F_Exists* f);
  F_Or* f_exists_add_or(F_Exists* r);
  F_Not* f_exists_add_not(F_Exists* r);
  F_Forall* f_exists_add_forall(F_Exists* r);
  F_Exists* f_exists_add_exists(F_Exists* r);

  Variable_ID f_forall_declare(F_Forall* f, char* name);
  Variable_ID f_exists_declare(F_Exists* f, char* name);

  Free_Var_Decl* free_var_decl0(char* name);
  Free_Var_Decl* free_var_decl1(char* name, int arity);

  void constraint_handler_update_const(Constraint_Handle* ch, coef_t delta);
  void constraint_handler_update_coef(Constraint_Handle* ch, Variable_ID v, coef_t delta);
  
  // printing
  void relation_print(Relation* r);
  void relation_print_to_file(Relation* r, FILE* output_file);
  const char* relation_print_to_string(Relation* r);
  void relation_print_with_subs(Relation* r, bool printSym);
  void relation_print_with_subs_to_file(Relation* r, FILE* output_file, bool printSym);
  const char* relation_print_with_subs_to_string(Relation* r, bool printSym);
  const char* relation_print_formula_to_string(Relation* r);
  const char* relation_print_outputs_with_subs_to_string(Relation* r);

  // simplification and satisfiability
  bool is_upper_bound_satisfiable(Relation* r);
  bool is_lower_bound_satisfiable(Relation* r);
  bool is_satisfiable(Relation* r);
  bool is_obvious_tautology(Relation* r);
  bool is_definite_tautology(Relation* r);
  bool is_exact(Relation* r);
  bool is_inexact(Relation* r);
  bool is_unknown(Relation* r);

  // upper and lower bounds
  Relation* upper_bound(Relation* r);
  Relation* lower_bound(Relation* r);
  
  // binary relational operators
  Relation* union_relation(Relation *r1, Relation *r2);
  Relation* intersection(Relation *r1, Relation *r2);
  Relation* composition(Relation *r1, Relation *r2);
  Relation* join(Relation *r1, Relation *r2);
  Relation* restrict_domain(Relation *r1, Relation *r2);
  Relation* restrict_range(Relation *r1, Relation *r2);
  Relation* difference(Relation *r1, Relation *r2);
  Relation* cross_product(Relation *r1, Relation *r2);
  Relation* gist(Relation *r1, Relation *r2);

  // unary relation operations
  Relation* transitive_closure1(Relation *r);
  Relation* transitive_closure2(Relation *r1, Relation *r2);
  Relation* domain(Relation *r);
  Relation* range(Relation *r);
  Relation* inverse(Relation *r);
  Relation* complement(Relation *r);
  Relation* project(Relation *r, Global_Var_ID v);
  Relation* project_sym(Relation *r);
  Relation* project_on_sym(Relation *r);
  Relation* extend_domain1(Relation *r);
  Relation* extend_domain2(Relation *r, int n);
  Relation* range_domain1(Relation *r);
  Relation* range_domain2(Relation *r, int n);
  Relation* extend_set1(Relation *r);
  Relation* extend_set2(Relation *r, int n);
  Relation* deltas1(Relation *r);
  Relation* deltas2(Relation *r, int n);
  Relation* approximate1(Relation *r);
  Relation* approximate2(Relation *r, int flag);
  Relation* egs_to_geqs1(Relation *r);
  Relation* egs_to_geqs2(Relation *r, bool flag);
  Relation* sample_solution(Relation *r);
  Relation* symbolic_solution(Relation *r);
  
  // advance operations
  Relation* convex_hull(Relation* r);
  Relation* decoupled_convex_hull(Relation* r);
  Relation* affine_hull(Relation* r);
  Relation* linear_hull(Relation* r);
  Relation* conic_hull(Relation* r);
  Relation* fast_tight_hull(Relation* r1, Relation* r2);
  Relation* hull0(Relation* r);
  Relation* hull1(Relation* r, bool flag);
  Relation* hull2(Relation* r, bool flag, int effort);
  Relation* hull3(Relation* r1, bool flag, int effort, Relation* r2);
  Relation* check_for_convex_pairs(Relation* r);
  Relation* check_for_convex_representation(Relation* r);

  // relational function that return boolean values
  bool must_be_subset(Relation* r1, Relation* r2);
  bool might_be_subset(Relation* r1, Relation* r2);
  bool is_obvious_subset(Relation* r1, Relation* r2);

  // query
  DNF_Iterator* dnf_iterator_new1(Relation* r);
  DNF_Iterator* dnf_iterator_new3(Relation* r, int rdt_conjs, int rdt_constrs);
  void dnf_iterator_next(DNF_Iterator* dnfi);
  bool dnf_iterator_more(DNF_Iterator* dnfi);

  EQ_Iterator* eq_iterator_new(DNF_Iterator* dnfi);
  void eq_iterator_next(EQ_Iterator* eq);
  bool eq_iterator_more(EQ_Iterator* eq);
  Constr_Vars_Iter* eq_constr_iter_new(EQ_Iterator* eq);

  GEQ_Iterator* geq_iterator_new(DNF_Iterator* dnfi);
  void geq_iterator_next(GEQ_Iterator* geq);
  bool geq_iterator_more(GEQ_Iterator* eq);
  Constr_Vars_Iter* geq_constr_iter_new(GEQ_Iterator* geq);

  Variable_ID constr_iter_get_variable(Constr_Vars_Iter* cvi);
  int constr_iter_get_coef(Constr_Vars_Iter* cvi);
  void constr_iter_next(Constr_Vars_Iter* cvi);
  bool constr_iter_more(Constr_Vars_Iter* cvi);

  Variable_Iterator* var_iter_new(DNF_Iterator* dnfi);
  void var_iter_next(Variable_Iterator* vi);
  bool var_iter_more(Variable_Iterator* vi);
  Variable_ID var_iter_get_variable(Variable_Iterator* vi);

  int eq_get_const(EQ_Iterator* eq);
  int geq_get_const(GEQ_Iterator* qi);

  void relation_setup_names(Relation* r);

  void query_experiment(Relation* r);
  
  void relation_simplify1(Relation* r);
  void relation_simplify3(Relation* r, int rdt_conjs, int rdt_constrs);
  int relation_number_of_conjuncts(Relation* r);


  const char* variable_name(Variable_ID v);
  Var_Kind variable_kind(Variable_ID v);
  int variable_get_position(Variable_ID v);


/*     for(DNF_Iterator di(R.query_DNF()); di; di++) */
/*         { */
/*         printf("In next conjunct,\n"); */
/*         for(EQ_Iterator ei = (*di)->EQs(); ei; ei++) */
/*             { */
/*             printf("  In next equality constraint,\n"); */
/*             for(Constr_Vars_Iter cvi(*ei); cvi; cvi++) */
/*                 printf("    Variable \%s has coefficient \%d\n", */
/*                        (*cvi).var->char_name(), */
/*                        (*cvi).coef); */
/*             } */
/*         for(GEQ_Iterator gi = (*di)->GEQs(); gi; gi++) */
/*             { */
/*             printf("  In next inequality constraint,\n"); */
/*             for(Constr_Vars_Iter cvi(*gi); cvi; cvi++) */
/*                 printf("    Variable \%s has coefficient \%d\n", */
/*                        (*cvi).var->char_name(), */
/*                        (*cvi).coef); */
/*             } */
/*         printf("\n"); */
/*         } */


}

#endif
