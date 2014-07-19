*
* $Id: tmread.F,v 1.1.1.1 1996/02/15 17:50:26 mclareni Exp $
*
* $Log: tmread.F,v $
* Revision 1.1.1.1  1996/02/15 17:50:26  mclareni
* Kernlib
*
*
      SUBROUTINE TMREAD (MAXCH, LINE, NCH, ISTAT)
C
C CERN PROGLIB# Z313    TMREAD          .VERSION KERNVAX  2.45  940228
C ORIG. 16/02/94, JZ
C
C     Read the next line from stdin
C
      CHARACTER    LINE*512

      READ (5,8000,ERR=91,END=49)  LG, LINE(1:MIN(LG,MAXCH))
 8000 FORMAT (Q,A)

      NCH   = MIN(LG,MAXCH)
      ISTAT = 0
      RETURN

   49 ISTAT = -1
      RETURN

   91 ISTAT = 7
      RETURN
      END
