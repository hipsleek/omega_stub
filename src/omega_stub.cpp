/*
 * $Id: omega_stub.cpp,v 1.6 2003-07-07 04:24:48 raz Exp $
 */

#include <omega.h>
#include "../include/omega_stub.h"

Relation* relation_new0()
{
  return new Relation();
}

Relation* relation_new1(int n_input)
{
  return new Relation(n_input);
}

Relation* relation_new2(int n_input, int n_output)
{
  return new Relation(n_input, n_output);
}


void relation_delete(Relation* r)
{
  delete r;
}


void relation_finalize(Relation* r)
{
  r->finalize();
}


Variable_ID relation_set_var(Relation* r, int nth)
{
  return r->set_var(nth);
}

Variable_ID relation_input_var(Relation* r, int nth)
{
  return r->input_var(nth);
}

Variable_ID relation_output_var(Relation* r, int nth)
{
  return r->output_var(nth);
}


Variable_ID relation_get_local1(Relation* r, Variable_ID var)
{
  return r->get_local(var);
}

Variable_ID relation_get_local_global1(Relation* r, Global_Var_ID gvar)
{
  return r->get_local(gvar);
}

Variable_ID relation_get_local_global2(Relation* r, Global_Var_ID gvar, Argument_Tuple of)
{
  return r->get_local(gvar, Input_Tuple);
}


void relation_name_set_var(Relation* r, int nth, char* name)
{
  r->name_set_var(nth, String(name));
}
void relation_name_input_var(Relation* r, int nth, char* name)
{
  r->name_input_var(nth, String(name));
}
void relation_name_output_var(Relation* r, int nth, char* name)
{
  r->name_output_var(nth, String(name));
}


F_And* relation_add_and(Relation* r)
{
  return r->add_and();
}

F_Or* relation_add_or(Relation* r)
{
  return r->add_or();
}

F_Not* relation_add_not(Relation* r)
{
  return r->add_not();
}

F_Forall* relation_add_forall(Relation* r)
{
  return r->add_forall();
}

F_Exists* relation_add_exists(Relation* r)
{
  return r->add_exists();
}

F_And* f_and_add_and(F_And* f)
{
  return f->add_and();
}
F_Or* f_and_add_or(F_And* f)
{
  return f->add_or();
}
F_Not* f_and_add_not(F_And* f)
{
  return f->add_not();
}
F_Forall* f_and_add_forall(F_And* f)
{
  return f->add_forall();
}
F_Exists* f_and_add_exists(F_And* f)
{
  return f->add_exists();
}


GEQ_Handle* f_and_add_GEQ(F_And* f)
{
  GEQ_Handle* h = new GEQ_Handle();
  *h = f->add_GEQ();
  return h;
  //  return &(f->add_GEQ());
}

EQ_Handle* f_and_add_EQ(F_And* f)
{
  EQ_Handle* h = new EQ_Handle();
  *h = f->add_EQ();
  return h;
  //  return &(f->add_EQ());
}

Stride_Handle* f_and_add_stride(F_And* f, int step)
{
  Stride_Handle* h = new Stride_Handle();
  *h = f->add_stride(step);
  return h;
  //  return &(f->add_stride(step));
}


F_And* f_or_add_and(F_Or* f)
{
  return f->add_and();
}
F_Or* f_or_add_or(F_Or* f)
{
  return f->add_or();
}
F_Not* f_or_add_not(F_Or* f)
{
  return f->add_not();
}
F_Forall* f_or_add_forall(F_Or* f)
{
  return f->add_forall();
}
F_Exists* f_or_add_exists(F_Or* f)
{
  return f->add_exists();
}


F_And* f_not_add_and(F_Not* f)
{
  return f->add_and();
}
F_Or* f_not_add_or(F_Not* f)
{
  return f->add_or();
}
F_Not* f_not_add_not(F_Not* f)
{
  return f->add_not();
}
F_Forall* f_not_add_forall(F_Not* f)
{
  return f->add_forall();
}
F_Exists* f_not_add_exists(F_Not* f)
{
  return f->add_exists();
}


F_And* f_forall_add_and(F_Forall* f)
{
  return f->add_and();
}
F_Or* f_forall_add_or(F_Forall* f)
{
  return f->add_or();
}
F_Not* f_forall_add_not(F_Forall* f)
{
  return f->add_not();
}
F_Forall* f_forall_add_forall(F_Forall* f)
{
  return f->add_forall();
}
F_Exists* f_forall_add_exists(F_Forall* f)
{
  return f->add_exists();
}


F_And* f_exists_add_and(F_Exists* f)
{
  return f->add_and();
}
F_Or* f_exists_add_or(F_Exists* f)
{
  return f->add_or();
}
F_Not* f_exists_add_not(F_Exists* f)
{
  return f->add_not();
}
F_Forall* f_exists_add_forall(F_Exists* f)
{
  return f->add_forall();
}
F_Exists* f_exists_add_exists(F_Exists* f)
{
  return f->add_exists();
}


Variable_ID f_forall_declare(F_Forall* f, char* name)
{
  return f->declare(name);
}
Variable_ID f_exists_declare(F_Exists* f, char* name)
{
  return f->declare(name);
}

void constraint_handler_update_const(Constraint_Handle* ch, coef_t delta)
{
  ch->update_const(delta);
}

void constraint_handler_update_coef(Constraint_Handle* ch, Variable_ID v, coef_t delta)
{
  ch->update_coef(v, delta);
}

  
void relation_print(Relation* r)
{
  r->print();
}

void relation_print_to_file(Relation* r, FILE* output_file)
{
  r->print(output_file);
}

void relation_print_with_subs_to_file(Relation* r, FILE* output_file, bool printSym)
{
  r->print_with_subs(output_file, printSym);
}


const char* relation_print_with_subs_to_string(Relation* r, bool printSym)
{
  return r->print_with_subs_to_string(printSym);
}

void relation_print_with_subs(Relation* r, bool printSym)
{
  printf(r->print_with_subs_to_string(printSym));
}

const char* relation_print_formula_to_string(Relation* r)
{
  return r->print_formula_to_string();
}


Free_Var_Decl* free_var_decl0(char* name)
{
  return new Free_Var_Decl(name);
}

Free_Var_Decl* free_var_decl1(char* name, int arity)
{
  return new Free_Var_Decl(name, arity);
}

// binary relational operators

Relation* union_relation(Relation *r1, Relation *r2)
{
  Relation* ro = new Relation();
  *ro = Union(*r1, *r2);
  return ro;
}

Relation* intersection(Relation *r1, Relation *r2)
{
  Relation* ro = new Relation();
  *ro = Intersection(*r1, *r2);
  return ro;
}

Relation* composition(Relation *r1, Relation *r2)
{
  Relation* ro = new Relation();
  *ro = Composition(*r1, *r2);
  return ro;
}

Relation* join(Relation *r1, Relation *r2)
{
  Relation* ro = new Relation();
  *ro = Join(*r1, *r2);
  return ro;
}

Relation* restrict_domain(Relation *r1, Relation *r2)
{
  Relation* ro = new Relation();
  *ro = Restrict_Domain(*r1, *r2);
  return ro;
}

Relation* restrict_range(Relation *r1, Relation *r2)
{
  Relation* ro = new Relation();
  *ro = Restrict_Range(*r1, *r2);
  return ro;
}

Relation* difference(Relation *r1, Relation *r2)
{
  Relation* ro = new Relation();
  *ro = Difference(*r1, *r2);
  return ro;
}

Relation* cross_product(Relation *r1, Relation *r2)
{
  Relation* ro = new Relation();
  *ro = Cross_Product(*r1, *r2);
  return ro;
}

Relation* gist(Relation *r1, Relation *r2)
{
  Relation* ro = new Relation();
  *ro = Gist(*r1, *r2);
  return ro;
}


// simplification and satisfiability
bool is_upper_bound_satisfiable(Relation* r)
{
  return r->is_upper_bound_satisfiable();
}

bool is_lower_bound_satisfiable(Relation* r)
{
  return r->is_lower_bound_satisfiable();
}

bool is_satisfiable(Relation* r)
{
  return r->is_satisfiable();
}

bool is_obvious_tautology(Relation* r)
{
  return r->is_obvious_tautology();
}

//bool is_definite_tautology(Relation* r)
//{
//  return r->is_definite_tautology();
//}

bool is_exact(Relation* r)
{
  return r->is_exact();
}

bool is_inexact(Relation* r)
{
  return r->is_inexact();
}

bool is_unknown(Relation* r)
{
  return r->is_unknown();
}


// unary relation operations
Relation* transitive_closure1(Relation *r)
{
  Relation* ro = new Relation();
  *ro = TransitiveClosure(*r);
  return ro;
}

Relation* transitive_closure2(Relation *r1, Relation *r2)
{
  Relation* ro = new Relation();
  *ro = TransitiveClosure(*r1, 1, *r2);
  return ro;
}

Relation* domain(Relation *r)
{
  Relation* ro = new Relation();
  *ro = Domain(*r);
  return ro;
}

Relation* range(Relation *r)
{
  Relation* ro = new Relation();
  *ro = Range(*r);
  return ro;
}

Relation* inverse(Relation *r)
{
  Relation* ro = new Relation();
  *ro = Inverse(*r);
  return ro;
}

Relation* complement(Relation *r)
{
  Relation* ro = new Relation();
  *ro = Complement(*r);
  return ro;
}

Relation* project(Relation *r, Global_Var_ID v)
{
  Relation* ro = new Relation();
  *ro = Project(*r, v);
  return ro;
}

Relation* project_sym(Relation *r)
{
  Relation* ro = new Relation();
  *ro = Project_Sym(*r);
  return ro;
}

Relation* project_on_sym(Relation *r)
{
  Relation* ro = new Relation();
  *ro = Project_On_Sym(*r);
  return ro;
}

Relation* extend_domain1(Relation *r)
{
  Relation* ro = new Relation();
  *ro = Extend_Domain(*r);
  return ro;
}

Relation* extend_domain2(Relation *r, int n)
{
  Relation* ro = new Relation();
  *ro = Extend_Domain(*r, n);
  return ro;
}

Relation* range_domain1(Relation *r)
{
  Relation* ro = new Relation();
  *ro = Extend_Range(*r);
  return ro;
}

Relation* range_domain2(Relation *r, int n)
{
  Relation* ro = new Relation();
  *ro = Extend_Range(*r, n);
  return ro;
}

Relation* extend_set1(Relation *r)
{
  Relation* ro = new Relation();
  *ro = Extend_Set(*r);
  return ro;
}

Relation* extend_set2(Relation *r, int n)
{
  Relation* ro = new Relation();
  *ro = Extend_Set(*r, n);
  return ro;
}

Relation* deltas1(Relation *r)
{
  Relation* ro = new Relation();
  *ro = Deltas(*r);
  return ro;
}

Relation* deltas2(Relation *r, int n)
{
  Relation* ro = new Relation();
  *ro = Deltas(*r, n);
  return ro;
}

Relation* approximate1(Relation *r)
{
  Relation* ro = new Relation();
  *ro = Approximate(*r);
  return ro;
}

Relation* approximate2(Relation *r, int flag)
{
  Relation* ro = new Relation();
  *ro = Approximate(*r, flag);
  return ro;
}

Relation* egs_to_geqs1(Relation *r)
{
  Relation* ro = new Relation();
  *ro = EQs_to_GEQs(*r);
  return ro;
}

Relation* egs_to_geqs2(Relation *r, bool flag)
{
  Relation* ro = new Relation();
  *ro = EQs_to_GEQs(*r, flag);
  return ro;
}

Relation* sample_solution(Relation *r)
{
  Relation* ro = new Relation();
  *ro = Sample_Solution(*r);
  return ro;
}

Relation* symbolic_solution(Relation *r)
{
  Relation* ro = new Relation();
  *ro = Symbolic_Solution(*r);
  return ro;
}


// advance operations
Relation* convex_hull(Relation* r)
{
  Relation* ro = new Relation();
  *ro = ConvexHull(*r);
  return ro;
}

Relation* decoupled_convex_hull(Relation* r)
{
  Relation* ro = new Relation();
  *ro = DecoupledConvexHull(*r);
  return ro;
}

Relation* affine_hull(Relation* r)
{
  Relation* ro = new Relation();
  *ro = AffineHull(*r);
  return ro;
}

Relation* linear_hull(Relation* r)
{
  Relation* ro = new Relation();
  *ro = LinearHull(*r);
  return ro;
}

Relation* conic_hull(Relation* r)
{
  Relation* ro = new Relation();
  *ro = ConicHull(*r);
  return ro;
}

Relation* fast_tight_hull(Relation* r1, Relation* r2)
{
  Relation* ro = new Relation();
  *ro = FastTightHull(*r1, *r2);
  return ro;
}

Relation* hull0(Relation* r)
{
  Relation* ro = new Relation();
  *ro = Hull(*r);
  return ro;
}

Relation* hull1(Relation* r, bool flag)
{
  Relation* ro = new Relation();
  *ro = Hull(*r, flag);
  return ro;
}

Relation* hull2(Relation* r, bool flag, int effort)
{
  Relation* ro = new Relation();
  *ro = Hull(*r, flag, effort);
  return ro;
}

Relation* hull3(Relation* r1, bool flag, int effort, Relation* r2)
{
  Relation* ro = new Relation();
  *ro = Hull(*r1, flag, effort, *r2);
  return ro;
}

Relation* check_for_convex_pairs(Relation* r)
{
  Relation* ro = new Relation();
  *ro = CheckForConvexPairs(*r);
  return ro;
}

Relation* check_for_convex_representation(Relation* r)
{
  Relation* ro = new Relation();
  *ro = CheckForConvexRepresentation(*r);
  return ro;
}


// relational function that return boolean values
bool must_be_subset(Relation* r1, Relation* r2)
{
  return Must_Be_Subset(*r1, *r2);
}

bool might_be_subset(Relation* r1, Relation* r2)
{
  return Might_Be_Subset(*r1, *r2);
}

bool is_obvious_subset(Relation* r1, Relation* r2)
{
  return Is_Obvious_Subset(*r1, *r2);
}


