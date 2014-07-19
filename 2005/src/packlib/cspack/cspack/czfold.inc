*
* $Id: czfold.inc,v 1.1.1.1 1996/03/08 15:44:17 mclareni Exp $
*
* $Log: czfold.inc,v $
* Revision 1.1.1.1  1996/03/08 15:44:17  mclareni
* Cspack
*
*
#ifndef CERNLIB_CSPACK_CZFOLD_INC
#define CERNLIB_CSPACK_CZFOLD_INC
*
*
* czfold.inc
*
      IF(INDEX(CHOPT,'C').EQ.0) THEN
         CALL CLTOU(LOCAL)
         CALL CLTOU(REMOTE)
      ENDIF

#endif
