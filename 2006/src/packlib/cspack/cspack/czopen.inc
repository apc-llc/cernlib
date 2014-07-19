*
* $Id: czopen.inc,v 1.1.1.1 1996/03/08 15:44:18 mclareni Exp $
*
* $Log: czopen.inc,v $
* Revision 1.1.1.1  1996/03/08 15:44:18  mclareni
* Cspack
*
*
#ifndef CERNLIB_CSPACK_CZOPEN_INC
#define CERNLIB_CSPACK_CZOPEN_INC
*
*
* czopen.inc
*
      IF(ISKIN.LT.0)THEN
         PRINT *,' Use the OPEN command to establish connection'
         GO TO 99
      ENDIF
*

#endif
