*
* $Id: mcline.F,v 1.1.1.1 1996/02/26 17:16:17 mclareni Exp $
*
* $Log: mcline.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:17  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :          29/09/95  13.08.46  by  Julian Bunn
*-- Author : V.Berezhnoi
      FUNCTION MCLINE(REC)
***-----------------------------------
*      14/04/95
*   don't convert to upper case in the directive case
***-------------------------------------------------
*      converts string to uppercase
*      treatments tabs for VAX
*      returns length of string
***-----------------------------------
**     tests for in-line comments !blablabla
**     use char(9) for tab-treatment
***-----------------------------------
      CHARACTER *(*)REC,LINE*120,CH*1,TAB*1
      PARAMETER (KTAB=9, LTAB=8)
      LOGICAL CONVERT
      TAB=CHAR(KTAB)
      LINE=' '
      N=0
      CONVERT=.TRUE.
      LREC=LEN(REC)
      IF(LREC.GT.72)LREC=72
      IR=1
      IL=1
      IB=1
  1   CH=REC(IR:IR)
      IF(CH.EQ.' ')THEN
        IL=IL+1
      ELSEIF(CH.EQ.'''')THEN
        IF(CONVERT)CALL CLTOU(LINE(IB:IL))
  2     LINE(IL:IL)=CH
        IL=IL+1
        IR=IR+1
        IF(IR.GT.LREC)GO TO 4
        CH=REC(IR:IR)
        IF(CH.NE.'''')GO TO 2
        LINE(IL:IL)=CH
        IL=IL+1
        IR=IR+1
        IF(IR.GT.LREC)THEN
           N=IL-1
           IB=IL
           GO TO 3
        ENDIF
        CH=REC(IR:IR)
        IF(CH.EQ.'''')GO TO 2
        IR=IR-1
        N=IL-1
        IB=IL
****************************************
      ELSEIF(CH.EQ.'!')THEN
        IF(N.GT.1) GO TO 3 !skip inline comment
        CONVERT=.FALSE.
        LINE(IL:IL)=CH
        N=IL
        IL=IL+1
      ELSEIF(CH.EQ.TAB)THEN
        ILL=((IL-1)/LTAB+1)*LTAB
        LINE(IL:ILL)=' '
        IL=ILL+1
      ELSE
        LINE(IL:IL)=CH
        N=IL
        IL=IL+1
      ENDIF
      IR=IR+1
      IF(IR.LE.LREC)GO TO 1
  3   IF(IB.LE.N .AND. CONVERT)CALL CLTOU(LINE(IB:N))
      REC=LINE(1:N)
      MCLINE=N
      RETURN
  4   N=IL-1
      GO TO 3
      END
