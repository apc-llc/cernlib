*
* $Id: csfmtc.F,v 1.1.1.1 1996/02/26 17:16:24 mclareni Exp $
*
* $Log: csfmtc.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:24  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :          29/09/95  13.10.01  by  Julian Bunn
*-- Author : V.Berezhnoi
      SUBROUTINE CSFMTC(LAST)
      COMMON/MDPOOL/IQ(256)
#include "comis/csfmt.inc"
C
      INTEGER STACK(24),TOP,IBPOS,IPOS
      LOGICAL ISQUOT
      SAVE STACK,TOP,IBPOS,IPOS
*
*     last       --> last=-1  initialition;
*                    last=0   i/o element exist;
*                    last=1   end of format
*     fmt*256   --> character presentation of format
*     iq(ifmt)  --> internal  presentation of format
*     ibpos      --> position for return after end of format
*     nrep       --> counter of repetitions
* current element: nrep,icode,iwidth,ipos1,ipos2
*         fmt(ipos1:ipos2) is char. presentation of format descriptor
*     buf*132    --> internal buffer for i/o
*     ibf        --> current position in buf for i/o
*     stack(24)  --> stack for return after ')'; ( nrep, ibpos )
*     top       --> head of stack; nrep=stack(top-2); ibpos=stack(top-1)
*     keyrw      --> for input keyrw=1; for output keyrw=2
*
*     DATA FDS/'0123456789LIZOFEDGAHXTPBS/()'',:$'/
*     K=        12345678911111111112222222222 333
*                        01234567890123456789 012
      IF(LAST.LT.0)THEN
        NREP=0
        IFMTST=0
        TOP=1
        IPOS=IFMT
        IBPOS=IPOS
        IBF=1
        IBFLST=1
        RETURN
      ENDIF
      IF(IFMTST.NE.0)RETURN
      NREP=NREP-1
      IF(NREP.GT.0)THEN
        IWIDTH=IQ(IPOS-3)
        RETURN
      ENDIF
      NREP=0
 99   CONTINUE
      IF(IFMTST.NE.0)RETURN
      GO TO (1 ,2 ,3 ,4 ,5 ,33,33,33,33,33,
*     K=     (1 (  )  )0 /  -  -  -  -  -
     +       11,11,11,11,15,15,15,15,19,20,
*     K=     L  I  Z  O  F  E  D  G  A  H
     +       21,22,23,24,33,33,33,33,29,33,31,31
*     K=     X  T  TL TR -  -  -  -  '' -  :  $
     +       ),IQ(IPOS)
      GO TO 33
 1    CONTINUE
*     (1 mark position for format return
      IPOS=IPOS+1
      IBPOS=IPOS
      GO TO 99
 2    CONTINUE
*     r(
      TOP=TOP+2
      IPOS=IPOS+2
      STACK(TOP-2)=IQ(IPOS-1)
      STACK(TOP-1)=IPOS
      GO TO 99
 3    CONTINUE
*     )
      N=STACK(TOP-2)
      N=N-1
      IF(N.GT.0)THEN
        STACK(TOP-2)=N
        IPOS=STACK(TOP-1)
      ELSE
        TOP=TOP-2
        IPOS=IPOS+1
      END IF
      GO TO 99
 4    CONTINUE
*     )0 end of format
      IF(LAST.EQ.1)RETURN
      IPOS=IBPOS-1
 5    CONTINUE
*     /
      IF(JSTR.NE.0)THEN
        IFMTST=5
        RETURN
      END IF
      IF(KEYRW.EQ.1)THEN
        IF(NDAREC.EQ.-1)THEN
          READ(LUNIO,77,ERR=111,IOSTAT=ISTA,END=222)BUF
        ELSE
          READ(LUNIO,77,ERR=111,IOSTAT=ISTA,REC=NDAREC)BUF
          NDAREC=NDAREC+1
        ENDIF
      ELSE
*       OUTPUT
        IF(IBFLST.LE.1)IBFLST=2
        IF(NDAREC.EQ.-1)THEN
          WRITE(LUNIO,77,ERR=111,IOSTAT=ISTA)BUF(1:IBFLST-1)
        ELSE
          WRITE(LUNIO,77,REC=NDAREC,ERR=111,IOSTAT=ISTA)
     +          BUF(1:IBFLST-1)
          NDAREC=NDAREC+1
        ENDIF
        BUF=' '
      END IF
      IBF=1
      IBFLST=1
      IPOS=IPOS+1
      GO TO 99
 77   FORMAT(A)
 11   CONTINUE
*     Lw or Iw or Zw or Ow
 15   CONTINUE
*     Fw.d or Ew.d or Dw.d or Gw.d
C      NREP=IQ(IPOS+1)
C      IWIDTH=IQ(IPOS+2)
C      IPOS1=IQ(IPOS+3)
C      IPOS2=IQ(IPOS+4)
C      IPOS=IPOS+5
C      RETURN
 19   CONTINUE
*     A or Aw
      ICODE=IQ(IPOS)
      NREP=IQ(IPOS+1)
      IWIDTH=IQ(IPOS+2)
      IPOS1=IQ(IPOS+3)
      IPOS2=IQ(IPOS+4)
      IPOS=IPOS+5
      RETURN
 20   CONTINUE
*     nH
      IF(KEYRW.EQ.2)THEN
        N=IQ(IPOS+1)
        I1=IQ(IPOS+2)
        I2=I1+N-1
        IBF1=IBF+N-1
        IF(IBF1.GT.LBUF)THEN
          IF(LIOERR.EQ.-1) PRINT *,' Too long output record'
          IFMTST=3
          RETURN
        ENDIF
        BUF(IBF:IBF1)=FMT(I1:I2)
        IBF=IBF1+1
        IF(IBF.GT.IBFLST)IBFLST=IBF
      ENDIF
      IPOS=IPOS+3
      GO TO 99
 21   CONTINUE
*     nX
      IBF=IBF+IQ(IPOS+1)
      IF(IBF.GT.IBFLST)IBFLST=IBF
      IPOS=IPOS+2
      GO TO 99
 22   CONTINUE
*     Tw
      IBF=IQ(IPOS+1)
      IF(IBF.GT.IBFLST)IBFLST=IBF
      IPOS=IPOS+2
      GO TO 99
 23   CONTINUE
*     TLw
      IBF=IBF-IQ(IPOS+1)
      IPOS=IPOS+2
      GO TO 99
 24   CONTINUE
*     TRw
      IBF=IBF+IQ(IPOS+1)
      IF(IBF.GT.IBFLST)IBFLST=IBF
      IPOS=IPOS+2
      GO TO 99
 29   CONTINUE
*     '....'
      IF(KEYRW.EQ.2)THEN
        I1=IQ(IPOS+1)
        I2=IQ(IPOS+2)
        ISQUOT=.FALSE.
        DO 290 I=I1,I2
          IF(IBF.GT.LBUF)THEN
            IF(LIOERR.EQ.-1) PRINT *,' Too long output record'
            IFMTST=3
            RETURN
          ENDIF
          IF(FMT(I:I).EQ.'''')THEN
            IF(ISQUOT)THEN
              ISQUOT=.FALSE.
            ELSE
              ISQUOT=.TRUE.
              BUF(IBF:IBF)=FMT(I:I)
              IBF=IBF+1
              IF(IBF.GT.IBFLST)IBFLST=IBF
            ENDIF
          ELSE
            BUF(IBF:IBF)=FMT(I:I)
            IBF=IBF+1
            IF(IBF.GT.IBFLST)IBFLST=IBF
          ENDIF
 290    CONTINUE
      ENDIF
      IPOS=IPOS+3
      GO TO 99
 31   CONTINUE
*     :
      ICODE=IQ(IPOS)
      IF(LAST.EQ.1)THEN
        RETURN
      ELSE
        IPOS=IPOS+1
      ENDIF
      GO TO 99
 33   CONTINUE
* error in comis format processing
      PRINT *,' error in comis format processing'
      IFMTST=77
      RETURN
 111  CONTINUE
* error during i/o
      IF(LIOERR.EQ.-1) PRINT *,' error during i/o '
      IFMTST=1
      RETURN
 222  CONTINUE
      IF(LIOEND.EQ.-1) PRINT *,' end of file during input'
      IFMTST=-1
      RETURN
      END
