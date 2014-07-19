*
* $Id: hint.inc,v 1.1.1.1 1996/02/14 13:10:13 mclareni Exp $
*
* $Log: hint.inc,v $
* Revision 1.1.1.1  1996/02/14 13:10:13  mclareni
* Higz
*
*
#ifndef CERNLIB_HIGZ_HINT_INC
#define CERNLIB_HIGZ_HINT_INC
*
*
* hint.inc
*
*CMZ :  1.18/00 23/04/93  17.27.56  by  O.Couet
*-- Author :
      PARAMETER (INWDNT=2)
      PARAMETER (NBNTMX=100)
      COMMON /HINT/ NTSTOR(INWDNT*NBNTMX+1),WNVPST(8*NBNTMX)


#endif
