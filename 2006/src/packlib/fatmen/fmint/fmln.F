*
* $Id: fmln.F,v 1.1.1.1 1996/03/07 15:18:13 mclareni Exp $
*
* $Log: fmln.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:13  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMLN(CHSRCE,CHTRGT,CHCOMM,IVECT,CHOPT,IRC)
*
*     Make a link to an existing entry. If the existing entry
*     is itself a link, the link points to the target of that
*     link.
*
#include "fatmen/faust.inc"
      CHARACTER*(*)   CHSRCE,CHTRGT
      CHARACTER*(*)   CHCOMM
      CHARACTER*255   CHFILE
      DIMENSION       IVECT(10)
#include "fatmen/fmnkeys.inc"
      DIMENSION       KEYS(LKEYFA),KEYSIN(LKEYFA),KEYSOU(LKEYFA)
      PARAMETER       (MAXCOP=1)
      PARAMETER       (JBIAS=2)
#include "fatmen/fatpara.inc"
#include "fatmen/fatbank.inc"
#include "fatmen/fatopts.inc"
 
      NFADDL = NFADDL + 1
      IRC    = 0
      LSRCE  = LENOCC(CHSRCE)
      LTRGT  = LENOCC(CHTRGT)
      LCOMM  = LENOCC(CHCOMM)
 
      IF(IOPTC.NE.0.AND.LCOMM.EQ.0) THEN
         CHCOMM = ' '
         LCOMM  = 80
      ENDIF
 
      IF(IDEBFA.GE.1) PRINT *,'FMLN. source: ',CHSRCE(1:LSRCE),
     +   ' target: ',CHTRGT(1:LTRGT)
*
*     Check if target file already exists
*     (A link must differ from an existing generic name)
*
      CALL FMEXST(CHTRGT(1:LTRGT),IRC)
      IF(IRC.NE.0) THEN
         IF(IOPTF.EQ.0) THEN
            IF(IDEBFA.GE.-1) WRITE(LPRTFA,9001) IRC
 9001 FORMAT(' FMLN. ',I6,' entries already exist. Link refused')
         ELSE
            IF(IDEBFA.GE.-1) WRITE(LPRTFA,9002) IRC
 9002 FORMAT(' FMLN. adding link even though ',I6,
     +       ' entries already exist.')
         ENDIF
      ENDIF
*
*     Check if source and target are the same
*
      IF(CHSRCE(1:LSRCE).EQ.CHTRGT(1:LTRGT)) THEN
         IRC = -1
         IF(IDEBFA.GE.-3) PRINT *,'FMLN. error - source and target ',
     +      'are the same'
         GOTO 999
      ENDIF
*
*     Check if source file is a link
*     (location code 0 = link)
*
      KEYSIN(MKSRFA) = -1
      KEYSIN(MKMTFA) = -1
      KEYSIN(MKLCFA) = 0
      KEYSIN(MKCLFA) = -1
      CALL FMSELK(CHSRCE(1:LSRCE),KEYSIN,KEYSOU,NFOUND,MAXCOP,IRC)
*
*     Did we find a link?
*
      IF(NFOUND.NE.0) THEN
         CALL FMGETK(CHSRCE(1:LSRCE),LTDSFA,KEYSOU,IRC)
         IF(IRC.NE.0) THEN
            IF(IDEBFA.GE.-3) PRINT *,'FMLN. error ',IRC,
     +         ' from FMGETK for ',CHSRCE(1:LSRCE)
            GOTO 999
         ENDIF
*
*     Get target of existing link
*
         CALL UHTOC(IQ(LTDSFA+KOFUFA+MFQNFA),4,CHFILE,NFQNFA)
         LFILE = LENOCC(CHFILE)
         CALL MZDROP(IDIVFA,LTDSFA,' ')
         LTDSFA = 0
      ELSE
         CHFILE = CHSRCE(1:LSRCE)
         LFILE  = LSRCE
      ENDIF
*
*     Now lift bank for new entry
*
      CALL FMBOOK(CHTRGT(1:LTRGT),KEYS,LTDSFA,LSUP,JBIAS,IRC)
      KEYS(MKLCFA) = 0
      IQ(LTDSFA+KOFUFA+MLOCFA) = 0
      CALL FMPUTC(LTDSFA,CHFILE,MFQNFA,LFILE,IRC)
      IF(IRC.NE.0) GOTO 999
*
*     Update comment and user words if requested
*
      IF(IOPTC.NE.0.AND.LCOMM.GT.0) THEN
         CALL FMPUTC(LTDSFA,CHCOMM,MUCMFA,LCOMM,IRC)
         IF(IRC.NE.0) GOTO 999
      ENDIF
 
      IF(IOPTU.NE.0) THEN
         CALL UCOPY(IVECT,IQ(LTDSFA+KOFUFA+MUSWFA),10)
      ENDIF
*
*     Now add the entry
*
      CALL FMPUT(CHTRGT(1:LTRGT),LTDSFA,IRC)
      CALL MZDROP(IDIVFA,LTDSFA,' ')
      LTDSFA = 0
 
  999 END
