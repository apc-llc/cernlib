*
* $Id: caopti.inc,v 1.1.1.1 1996/02/28 16:24:04 mclareni Exp $
*
* $Log: caopti.inc,v $
* Revision 1.1.1.1  1996/02/28 16:24:04  mclareni
* Hepdb, cdlib, etc
*
*
#ifndef CERNLIB_HEPDB_CAOPTI_INC
#define CERNLIB_HEPDB_CAOPTI_INC
*
*
* caopti.inc
*
      COMMON /CAOPTI/ CALFCA(62), CSTRCA
      CHARACTER       CALFCA*1, CSTRCA*132
      COMMON /CAOPT1/ MPAKCA(2)
      INTEGER         MPAKCA
*

#endif
