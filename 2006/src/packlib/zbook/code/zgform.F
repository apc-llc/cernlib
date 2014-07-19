*
* $Id: zgform.F,v 1.1.1.1 1996/03/08 12:01:12 mclareni Exp $
*
* $Log: zgform.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:12  mclareni
* Zbook
*
*
#include "zbook/pilot.h"
      SUBROUTINE ZGFORM(IZ,SNAME,SFORM)
*
*--- RETURNS A BANK FORMAT AS CHARACTER STRING
*
* author HG       4/12/84
*
*--- INPUT
*    IZ          STRUCTURE
*    SNAME       BANK NAME, TYPE CHARACTER
*--- OUTPUT
*    SFORM       FORMAT AS STRING, OR ALL BLANK IF NONE
*
* For description of formats, see routine ZECFOR.
*
      DIMENSION IZ(*),IW(50)
      CHARACTER SNAME*(*), SFORM*(*), SLOC*200, STEMP*1, STYP(4)*1
      DATA STYP/'X','I','F','A'/
      SFORM=' '
      CALL UCTOH(SNAME,NAME,4,4)
      CALL ZNAMSR(IZ,NAME,IST,LAST)
      IF(IST.GT.0)  THEN
*--- name is in table - get format in IW
         CALL ZGTFOR(IZ,IST,NW,IW)
         NCH=0
         DO 10 I=1,NW
            NF=IW(I)/10000
            N=IW(I)-10000*NF
            IF (NF.EQ.0)  THEN
*--- repetition factor, or start of endless loop
               IF (NCH.GT.0)  THEN
*--- add comma to previous items
                  NCH=NCH+1
                  SLOC(NCH:NCH)=','
               ENDIF
               IF (N.GT.0)  THEN
                  WRITE (SLOC(NCH+1:),'(I4)') N
                  NCH=NCH+4
               ENDIF
               NCH=NCH+1
               SLOC(NCH:NCH)='('
            ELSEIF (NF.GT.0.AND.NF.LT.5)  THEN
*--- format of type NF
               IF (NCH.GT.0)  THEN
                  IF (SLOC(NCH:NCH).NE.'(')  THEN
*--- add comma to previous items
                     NCH=NCH+1
                     SLOC(NCH:NCH)=','
                  ENDIF
               ENDIF
               WRITE (SLOC(NCH+1:),'(I4)') N
               NCH=NCH+5
               SLOC(NCH:NCH)=STYP(NF)
            ELSE
*--- 50000 = end of bracket
               NCH=NCH+1
               SLOC(NCH:NCH)=')'
            ENDIF
   10    CONTINUE
*--- store in SFORM without blanks
         N=0
         NMAX=LEN(SFORM)
         DO 20 J=1,NCH
            STEMP=SLOC(J:J)
            IF (STEMP.NE.' ')  THEN
               IF (N.EQ.NMAX) GOTO 999
               N=N+1
               SFORM(N:N)=STEMP
            ENDIF
   20    CONTINUE
      ENDIF
  999 END
