*
* $Id: fmfnms.F,v 1.1.1.1 1996/03/07 15:18:10 mclareni Exp $
*
* $Log: fmfnms.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:10  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMFNMS(PATH,FILES,KEYS,NKEYS,MAXKEY,IRC)
*
*     Return file names and keys vector for named directory
*
#include "fatmen/fatpara.inc"
#include "fatmen/fatbug.inc"
      CHARACTER*(*) PATH
      CHARACTER*255 CHPATH
      CHARACTER  FILES(MAXKEY)*(*)
      PARAMETER (LURCOR=200000)
      COMMON/CRZT/IXSTOR,IXDIV,IFENCE(2),LEV,LEVIN,BLVECT(LURCOR)
      DIMENSION    LQ(999),IQ(999),Q(999)
      EQUIVALENCE (IQ(1),Q(1),LQ(9)),(LQ(1),LEV)
*
#include "fatmen/fatusr.inc"
#include "zebra/quest.inc"
      DIMENSION IQSAVE(100)
#include "fatmen/fmnkeys.inc"
      DIMENSION       KEYS(LKEYFA,MAXKEY)
      CHARACTER*255   LINE
*
*     Set current directory
*
      LPATH = LENOCC(PATH)
      NKEYS = 0
      IF(IDEBFA.GE.3) PRINT *,'FMFNMS. enter for path = ',
     +   PATH(1:LPATH)
      CHPATH = PATH(1:LPATH)
      CALL CLTOU(CHPATH(1:LPATH))
#if !defined(CERNLIB_CZ)
      CALL FACDIR(CHPATH(1:LPATH),' ')
      IF(IQUEST(1).NE.0) THEN
         IRC = -1
         RETURN
      ENDIF
#endif
#if defined(CERNLIB_CZ)
      CALL CZPUTA('MESS :CD '//CHPATH(1:LPATH)),ISTAT)
#endif
*
*     Get keys from current directory and match against current file
*
#if !defined(CERNLIB_CZ)
      CALL RZKEYS(LKEYFA,MAXKEY,KEYS,NKEYS)
#endif
#if defined(CERNLIB_CZ)
*
*     Get keys from remote server
*
      CALL CZPUTA('MESS :KE',ISTAT)
1     CONTINUE
 
      CALL CZGETA(LINE,ISTAT)
*
      IF (LINE(1:1) .EQ. '0') GOTO 2
      IF (INDEX(LINE,'No files found') .NE. 0) THEN
         ELSE
         NKEYS = NKEYS + 1
         READ(LINE,9001) (KEYS(J,NKEYS),J=1,10)
         ENDIF
 
9001  FORMAT(2X,I5,5A4,4I5)
      IF (LINE(1:1) .EQ. '2') GOTO 1
2     CONTINUE
#endif
*
*     Loop over keys returned
*
      IRC = 0
      IF(NKEYS.GT.MAXKEY) IRC = 1
      DO 10 I=1,MIN(NKEYS,MAXKEY)
      CALL CFILL(' ',FILES(I),1,LENOCC(FILES(I)))
      CALL UHTOC(KEYS(MKFNFA,I),4,FILES(I),(MKCLFA-MKFNFA)*4)
10    CONTINUE
 
      END
