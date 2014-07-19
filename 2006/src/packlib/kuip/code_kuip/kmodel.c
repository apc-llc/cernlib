/*
 * $Id: kmodel.c,v 1.2 1997/03/17 16:55:00 mclareni Exp $
 *
 * $Log: kmodel.c,v $
 * Revision 1.2  1997/03/17 16:55:00  mclareni
 * WNT mods
 *
 * Revision 1.1.1.1.2.1  1997/01/21 11:32:51  mclareni
 * All mods for Winnt 96a on winnt branch
 *
 * Revision 1.1.1.1  1996/03/08 15:32:58  mclareni
 * Kuip
 *
 */
/*CMZ :  2.00/09 18/01/93  15.07.23  by  Alfred Nathaniel*/
/*-- Author :    Alfred Nathaniel   05/11/92*/
/* kmodel.c: dummy routines for Model Human Interface */

#include "kuip/kuip.h"
#include "kuip/kfor.h"


void F77_ENTRY(Mhi_close)
/* { */
  /* do nothing */
}


void F77_ENTRY(Mhi_open)
/* { */
  printf( " *** Cannot change style to MHI\
 - Model Human Interface not available\n" );
  ku_exel( "/KUIP/SET_SHOW/STYLE 'C'" );
}


void F77_ENTRY(Mdmenu)
/* { */
  /* do nothing */
}


