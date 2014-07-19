*
* $Id: csioed.F,v 1.1.1.1 1996/02/26 17:16:24 mclareni Exp $
*
* $Log: csioed.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:24  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/14 18/01/95  10.33.32  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      INTEGER FUNCTION CSIOED(N)
#include "comis/cspar.inc"
#include "comis/cspnts.inc"
#include "comis/mdpool.inc"
#include "comis/comis.inc"
#include "comis/csfmt.inc"
      CSIOED=1
      IF(IFMTST.NE.0)THEN
        CSIOED=0
      ELSEIF(KEYRW.EQ.2)THEN
        IF(IFMT.GT.0)CALL CSFMTC(1)
        IF(IFMTST.NE.0)THEN
          CSIOED=0
        ENDIF
        IF(IFMT.EQ.-1)THEN
          IF(NDAREC.EQ.-1)THEN
*          unformatted sequential output
C      WRITE(*,*) 'SEQUNOUT:',IUBS,LULIST,
C     +           (IQ(I),I=IUBS,IUBS+LULIST-1)
            WRITE(LUNIO,IOSTAT=ISTA,ERR=110)
     +         (IQ(I),I=IUBS,IUBS+LULIST-1)
          ELSE
*          unformatted direct output
C      WRITE(*,*) 'DIRUNOUT:',IUBS,LULIST,NDAREC,
C     +           (IQ(I),I=IUBS,IUBS+LULIST-1)
            WRITE(LUNIO,REC=NDAREC,IOSTAT=ISTA,ERR=110)
     +         (IQ(I),I=IUBS,IUBS+LULIST-1)
            NDAREC=NDAREC+1
          ENDIF
          CALL MHFREE(IUBS)
        ELSEIF(IBFLST.GT.1)THEN
          IF(JSTR.EQ.0)THEN
            IF(NDAREC.EQ.-1)THEN
              IF(ICODE.NE.32)THEN
              WRITE(LUNIO,77,IOSTAT=ISTA,ERR=111)BUF(1:IBFLST-1)
              ELSE
*desc         last fmt code is $
              WRITE(LUNIO,78,IOSTAT=ISTA,ERR=111)BUF(1:IBFLST-1)
                ICODE=0
              ENDIF
            ELSE
              WRITE(LUNIO,77,REC=NDAREC,IOSTAT=ISTA,ERR=111)
     +              BUF(1:IBFLST-1)
              NDAREC=NDAREC+1
            ENDIF
 77         FORMAT(A)
#if defined(CERNLIB_IBM)
 78         FORMAT(A)
#endif
#if !defined(CERNLIB_IBM)
 78         FORMAT(A,$)
#endif
          ELSE
            J=MJSCHA(BUF)
            CALL CCOPYS(J,JSTR,IBFLST-1)
          ENDIF
        ENDIF
      ELSE
        IF(IFMT.EQ.-1)THEN
          IF(LULIST.LE.0)THEN
            LULIST=1
            IFMTST=8
          ENDIF
          IUBS=MHLOC(LULIST)
*         unformatted input
          IF(NDAREC.EQ.-1)THEN
*          unformatted sequential input
            READ(LUNIO,IOSTAT=ISTA,ERR=110,END=222)
     +         (IQ(I),I=IUBS,IUBS+LULIST-1)
C      WRITE(*,*) 'SEQUNINP:',IUBS,LULIST,
C     +           (IQ(I),I=IUBS,IUBS+LULIST-1)
          ELSE
*          unformatted direct input
            READ(LUNIO,REC=NDAREC,IOSTAT=ISTA,ERR=110)
     +         (IQ(I),I=IUBS,IUBS+LULIST-1)
            NDAREC=NDAREC+1
C      WRITE(*,*) 'DIRUNINP:',IUBS,LULIST,
C     +           (IQ(I),I=IUBS,IUBS+LULIST-1)
          ENDIF
          K=IUBS
          DO 1 I=1,IUBTOP,2
            J=IUBUF(I)
            L=IUBUF(I+1)
            CALL CCOPYA(IQ(K),IA(J-JTOPA),L)
            K=K+L
  1       CONTINUE
          CALL MHFREE(IUBS)
        ENDIF
      ENDIF
      RETURN
 110  CALL MHFREE(IUBS)
 111  IFMTST=ISTA
      IF(LIOERR.EQ.-1) PRINT *,' Error during output'
      CSIOED=0
      RETURN
 222  IFMTST=-1
      IF(LIOEND.EQ.-1) PRINT *,' i/o end of file during read'
      CSIOED=0
      END
