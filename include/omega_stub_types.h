/*
 * $Id: omega_stub_types.h,v 1.4 2004-07-21 11:36:19 popeeaco Exp $
 */

#ifndef _OMEGA_STUB_TYPES_H
#define _OMEGA_STUB_TYPES_H

extern "C" {
  
  typedef void Relation;
  typedef void* Variable_ID;
  typedef void* Global_Var_ID;
  typedef void F_And;
  typedef void F_Or;
  typedef void F_Not;
  typedef void F_Forall;
  typedef void F_Exists;
  typedef void F_Declaration;
  typedef void* GEQ_Handle;
  typedef void* EQ_Handle;
  typedef void* Stride_Handle;
  typedef void Free_Var_Decl;
  typedef void Constraint_Handle;
  typedef enum {
    Unknown_Tuple = 0,
    Input_Tuple = 1,
    Output_Tuple = 2,
    Set_Tuple = Input_Tuple
  } Argument_Tuple;
  typedef int coef_t;

  typedef void DNF;
  typedef void DNF_Iterator;
  typedef void Constr_Vars_Iter;
  typedef void Variable_Iterator;
  typedef void EQ_Iterator;
  typedef void GEQ_Iterator;
  
  typedef void* Var_Kind;
}
#endif
