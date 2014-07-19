*
* $Id: xzmvsd.F,v 1.1.1.1 1996/03/08 15:44:31 mclareni Exp $
*
* $Log: xzmvsd.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:31  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
#if defined(CERNLIB_IBMMVS)
      SUBROUTINE XZMVSD(CDSNIN,CDSNOUT,LDSNOUT,ICUT,IRC)
*     **************************************************
*
*     Returns dataset name with high level qualifier
*
*     INPUT : CDSNIN , dataset name without high level qualifier
*     OUTPUT: CDSNOUT, dataset with high level qualifier and dot !!!
*     OUTPUT: ICUT   , legth of the high level qualifier
*     OUTPUT: IRC    , error return code
*
      CHARACTER*(*) CDSNIN,CDSNOUT
      CHARACTER*80 RPREFI
 
      IRC  = 0
      ICUT = 2
      LDSNOUT  = LENOCC(CDSNIN)
      CALL CLTOU(CDSNIN(1:LDSNOUT))
      CALL CFILL(' ',CDSNOUT,1,LDSNOUT)
*
*     Simple Error Checking
*                           to overcome some problems with listc
*                           cut ending with * or .
 100  LDSNIN = LENOCC(CDSNIN)
      IF         ((CDSNIN(LDSNIN:LDSNIN).EQ.'*')
     1        .OR.(CDSNIN(LDSNIN:LDSNIN).EQ.'.')) THEN
        CDSNIN(LDSNIN:LDSNIN) = ' '
        LDSNIN = LDSNIN - 1
      ELSE
      GOTO 200
      ENDIF
      GOTO 100
 
 200  CONTINUE
 
      IF(LDSNIN.EQ.0) THEN
C        -------------------------------- no dataset specification
C        -------------------------------- use XZRPRE
         CALL XZRPRE(RPREFI,LRPREFI)
         CDSNOUT  = RPREFI(1:LRPREFI)
         LDSNOUT  = LRPREFI - 1
         ICUT     = LRPREFI
      ELSE
C        -------------------------------- dataset is specified
         IF (CDSNIN(1:1).EQ.'.')  THEN
C          ------------------------------ dataset is full specified
           CDSNOUT = CDSNIN(1:LDSNIN)
           LDSNOUT = LDSNIN
           ICUT    =  1
         ELSE
C          ------------------------------ dataset is not full
C          ------------------------------ specified  add XZRPRE
           CALL XZRPRE(RPREFI,LRPREFI)
 
           CDSNOUT = RPREFI(1:LRPREFI) // CDSNIN(1:LDSNIN)
           LDSNOUT = LRPREFI+LDSNIN
           ICUT    = LRPREFI
         ENDIF
      ENDIF
 
      RETURN
      END
#endif
