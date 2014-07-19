/*
 * $Id: chain2.h,v 1.1.1.1 1996/03/01 11:39:14 mclareni Exp $
 *
 * $Log: chain2.h,v $
 * Revision 1.1.1.1  1996/03/01 11:39:14  mclareni
 * Paw
 *
 */
/*CMZ :  2.03/07 23/08/93  19.27.01  by  Fons Rademakers*/
/*-- Author :*/
#pragma linkage(pchlst,FORTRAN)
#pragma linkage(pchdel,FORTRAN)
#pragma linkage(pchadd,FORTRAN)
#pragma linkage(pchpat,FORTRAN)
#pragma linkage(pchset,FORTRAN)
#pragma linkage(pchnxt,FORTRAN)
#pragma linkage(pischn,FORTRAN)
#pragma linkage(piscpf,FORTRAN)
#pragma linkage(pchevt,FORTRAN)
#pragma linkage(pchcnt,FORTRAN)
#pragma linkage(pchtop,FORTRAN)
#define  delete_chain_entry        _chn001
#define  delete_chain_references   _chn002
#define  delete_a_chain            _chn003
#define  delete_all_chains         _chn004
#define  print_chains              _chn005
#define  print_chain_tree          _chn006
