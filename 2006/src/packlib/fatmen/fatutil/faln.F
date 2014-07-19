*
* $Id: faln.F,v 1.1.1.1 1996/03/07 15:18:07 mclareni Exp $
*
* $Log: faln.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:07  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FALN(GENAM,KEYS,IRC)
      CHARACTER*(*) GENAM
*CMZ :          03/09/91  17.30.54  by  Jamie Shiers
*-- Author :    Jamie Shiers   03/09/91
#include "fatmen/fmnkeys.inc"
      DIMENSION KEYS(LKEYFA)
      CHARACTER*255 CHLINK
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/falncm.inc"
*
*     Options: P - print names of dangling links
*              D - write names of dangling links in 'rm ksn' format
*              R - remove dangling links
*              F - redirect output to CHFILE on LWRITE
*
      IRC = 0
      LGN = LENOCC(GENAM)
 
      IF(IDEBFA.GE.2) PRINT *,'FALN. processing ',
     +   GENAM(1:LGN)
      NFILES = NFILES + 1
 
      CALL FMGETK(GENAM(1:LGN),LTDSFA,KEYS,IRC)
      IF(IRC.NE.0) THEN
         IF(IDEBFA.GE.0) PRINT *,'FALN. error ',IRC,
     +      ' retrieving catalogue entry for ',GENAM(1:LGN)
      ELSE
         CALL UHTOC(IQ(LTDSFA+KOFUFA+MFQNFA),4,CHLINK,NFQNFA)
         LLINK = LENOCC(CHLINK)
*
*     Does an entry exist for this name?
*
         CALL FMEXST(CHLINK(1:LLINK),IEXIST)
         IF(IEXIST.EQ.0) THEN
            NLINKS = NLINKS + 1
            LUN = LPRTFA
            IF(IOPTTF.NE.0) LUN = LWRITE
            IF(IOPTTD.NE.0) THEN
               WRITE(LUN,9001) GENAM(1:LGN),KEYS(MKSRFA)
            ELSEIF(IOPTTP.NE.0) THEN
               WRITE(LUN,9002) GENAM(1:LGN)
            ENDIF
            IF(IOPTTR.NE.0) THEN
               CALL FMRM(GENAM(1:LGN),LTDSFA,KEYS,IRC)
            ENDIF
         ENDIF
 
         CALL MZDROP(IXSTOR,LTDSFA,' ')
         LTDSFA = 0
      ENDIF
 
 9001 FORMAT(' rm ',A,1X,I10)
 9002 FORMAT('FALN. ',A)
 
      END
