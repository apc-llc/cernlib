*
* $Id: qnatch.inc,v 1.1.1.1 1996/02/15 17:54:13 mclareni Exp $
*
* $Log: qnatch.inc,v $
* Revision 1.1.1.1  1996/02/15 17:54:13  mclareni
* Kernlib
*
*
*  Siemens compiler
*
* qnatch.inc
*
#if defined(CERNLIB_QF_SIEM)
      COMMON /QNATCH/ NATCH(256)
      INTEGER*2       NATCH
#include "qnatchdata.inc"
#endif
      INTEGER*2       NATCH(256)
#include "qnatchdata.inc"
