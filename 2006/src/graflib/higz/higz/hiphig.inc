*
* $Id: hiphig.inc,v 1.1.1.1 1996/02/14 13:10:18 mclareni Exp $
*
* $Log: hiphig.inc,v $
* Revision 1.1.1.1  1996/02/14 13:10:18  mclareni
* Higz
*
*
#ifndef CERNLIB_HIGZ_HIPHIG_INC
#define CERNLIB_HIGZ_HIPHIG_INC
*
*   COMMON BLOCK for the PHIGS interface
*
* hiphig.inc
*
#if defined(CERNLIB_PHIGS)
      COMMON /HIPHIG/ LOSTOP,LOSTEX,ICSTN
      LOGICAL LOSTOP,LOSTEX
#endif

#endif
