/*
 * $Id: example.c,v 1.4 2003-05-25 02:48:07 raz Exp $
 */

//#include <presburger.h>
#include <stdlib.h>
#include "../include/omega_stub_types.h"
#include "../include/omega_stub.h"



//THIS NEEDS TO BE A LOT SIMPLER - I'M CUTTING IT DOWN

// R := { [i,j] -> [i', j'] :
//        1 <= i, i' <= n && 1 <= j <= L(i) && 1 <= j' <= m &&
//        j = j' && i < i' }
//
// S := { [x,y] : 1 <= x <= n && y <= x + 5 && x is divisible by 17 &&
//        there exists z such that y <= z <= x &&
//        ( z is divisible by 8 || z+5x is divisible by 12 ) }


int main ()
{
  //  Relation R (2, 2), S (2);
  Relation* R = relation_new2(2, 2);
  Relation* S = relation_new1(2);

  //  S.name_set_var (1, "x");
  //  S.name_set_var (2, "y");
  relation_name_set_var(S, 1, "x");
  relation_name_set_var(S, 2, "y");

  //  assert (!R.is_set ());
  //  assert (S.is_set ());

  //  Free_Var_Decl n ("n");
  //  Free_Var_Decl m ("m");
  //  Free_Var_Decl l ("L", 1);
  Free_Var_Decl* n = free_var_decl0("n");
  Free_Var_Decl* m = free_var_decl0("m");
  Free_Var_Decl* l = free_var_decl0("l");

  //  Variable_ID local_n = R.get_local (&n);
  //  Variable_ID local_m = R.get_local (&m);
  //  Variable_ID l_in = R.get_local (&l, Input_Tuple);
  //  Variable_ID l_out = R.get_local (&l, Output_Tuple);
  Variable_ID local_n = relation_get_local_global1(R, n);
  Variable_ID local_m = relation_get_local_global1(R, m);
  Variable_ID l_in = relation_get_local_global2(R, l, Input_Tuple);
  Variable_ID l_out = relation_get_local_global2(R, l, Output_Tuple);

  //  Variable_ID in1 = R.input_var (1);
  //  Variable_ID in2 = R.input_var (2);
  //  Variable_ID out1 = R.output_var (1);
  //  Variable_ID out2 = R.output_var (2);
  Variable_ID in1 = relation_input_var(R, 1);
  Variable_ID in2 = relation_input_var(R, 2);
  Variable_ID out1 = relation_output_var(R, 1);
  Variable_ID out2 = relation_output_var(R, 1);

  //  Variable_ID x = S.set_var (1);
  //  Variable_ID y = S.set_var (2);
  Variable_ID x = relation_set_var(S, 1);
  Variable_ID y = relation_set_var(S, 2);

  //  F_And *S_root = S.add_and ();
  F_And* S_root = relation_add_and(S);

  //  GEQ_Handle xmin = S_root->add_GEQ ();	// x-1 >= 0
  //  xmin.update_coef (x, 1);
  //  xmin.update_const (-1);
  GEQ_Handle* xmin = f_and_add_GEQ(S_root);
  constraint_handler_update_coef(xmin, x, 1);
  constraint_handler_update_const(xmin, -1);

  //  GEQ_Handle xmax = S_root->add_GEQ ();	// n-x >= 0
  //  xmax.update_coef (x, -1);
  //  xmax.update_coef (S.get_local (&n), 1);
  GEQ_Handle xmax = f_and_add_GEQ(S_root);
  constraint_handler_update_coef(xmax, x, -1);
  constraint_handler_update_coef(xmax, relation_get_local_global1(S, n), 1);
  
  //  GEQ_Handle ymax = S_root->add_GEQ ();	// x+5-y >= 0
  //  ymax.update_coef (x, 1);
  //  ymax.update_coef (y, -1);
  //  ymax.update_const (5);
  GEQ_Handle ymax = f_and_add_GEQ(S_root);
  constraint_handler_update_coef(ymax, x, 1);
  constraint_handler_update_coef(ymax, y, -1);
  constraint_handler_update_const(ymax, 5);

  // x is divisible by 17
  //  S_root->add_stride (17).update_coef (x, 1);
  constraint_handler_update_coef(f_and_add_stride(S_root, 17), x, 1);

  //  F_Exists *e = S_root->add_exists ();
  F_Exists* e = f_and_add_exists(S_root);

  //  Variable_ID z = e->declare ("z");	// exists z
  //  F_And *z_stuff = e->add_and ();
  Variable_ID z = f_declare(e, "z");
  F_And* z_stuff = f_and_add_and(e);

  //  GEQ_Handle zmin = z_stuff->add_GEQ ();	// z-y >= 0
  //  zmin.update_coef (z, 1);
  //  zmin.update_coef (y, -1);
  GEQ_Handle zmin = f_and_add_GEQ(z_stuff);
  constraint_handler_update_coef(zmin, z, 1);
  constraint_handler_update_coef(zmin, y, -1);

  //  GEQ_Handle zmax = z_stuff->add_GEQ ();	// x-z >= 0
  //  zmax.update_coef (x, 1);
  //  zmax.update_coef (z, -1);
  GEQ_Handle zmax = f_and_add_GEQ(z_stuff);
  constraint_handler_update_coef(zmax, x, 1);
  constraint_handler_update_coef(zmax, z, -1);

  //  F_Or *o = z_stuff->add_or ();
  //  Stride_Handle z8 = o->add_and ()->add_stride (8);
  //  z8.update_coef (z, 1);	// z divisible by 8
  F_Or* o = f_and_add_or(z_stuff);
  Stride_Handle z8 = f_and_add_stride(f_and_add_and(o), 8);
  constraint_handler_update_coef(z8, z, 1);

  //  Stride_Handle z12 = o->add_and ()->add_stride (12);
  //  z12.update_coef (z, 1);
  //  z12.update_coef (x, 5);	// z+5x divisible by 12
  Stride_Handle z12 = f_and_add_stride(f_and_add_and(o), 12);
  constraint_handler_update_coef(z12, z, 1);
  constraint_handler_update_coef(z12, x, 5);


  //  S.finalize ();
  relation_finalize(S);
  //  S.prefix_print ();
  relation_print(S);
  //  S.is_upper_bound_satisfiable ();
  //  S.prefix_print ();

  return EXIT_SUCCESS;
}
