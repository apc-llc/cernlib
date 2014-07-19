*
* $Id: ehist.F,v 1.11 2001/10/08 14:03:14 mclareni Exp $
*
* $Log: ehist.F,v $
* Revision 1.11  2001/10/08 14:03:14  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE EHIST
C
C          Print HBOOK histograms with contents and errors in E format.
C          Supports both HBOOK3 and HBOOK4.
C
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
#include "itapes.inc"
#include "myhist.inc"
C
      LOGICAL HEXIST
      CHARACTER*1 APRNT(10,120)
      CHARACTER*10 WORD
#if defined(CERNLIB_HBOOK4)
      CHARACTER*80 TITLE
#endif
#if defined(CERNLIB_HBOOK3)
      REAL TITLE(8)
#endif
      INTEGER ID1,ID,NWT,NY,NX,IAD,NCH,J,K
      REAL XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX,HMIN,HMAX,Y,HI,HIE
C
C          Print histogram index
      CALL HINDEX
C
C          Loop over weighted histograms
C
      DO 1000 ID1=1,MXHIST
        ID=ID1+NHSHFT
        IF(.NOT.HEXIST(ID)) GO TO 1000
C          Print histogram in standard way
        CALL HPRINT(ID)
C
C          Check for 2 dimensional histogram
        NWT=8
        NY=0
        CALL HGIVE(ID,TITLE,NX,XMIN,XMAX,NY,YMIN,YMAX,NWT,IAD)
        IF(NY.NE.0) THEN
          ZMAX=HMAX(ID)
          ZMIN=HMIN(ID)
          WRITE(ITLIS,2010) ZMIN,ZMAX
2010      FORMAT(/'  * CONTENT MIN = ',E12.5,'  MAX = ',E12.5)
          GO TO 1000
        ENDIF
C
C          Print 1-dimensional channel contents in E format.
C
        NCH=NX
        IF(NCH.GT.100) NCH=100
C          Clear APRNT array
        DO 100 J=1,100
        DO 100 K=1,10
100     APRNT(K,J)=' '
C          Extract contents and convert to E format
        DO 110 J=1,NCH
          Y=HI(ID,J)
          WRITE(WORD,'(E10.4)') Y
          READ(WORD,'(10A1)') (APRNT(K,J),K=1,10)
110     CONTINUE
C          Print channel marks
        WRITE(ITLIS,121)
121     FORMAT('1'//
     1  ' CHANNELS',6X,'0',99X,'1'/
     2  15X,'0',9X,'1',9X,'2',9X,'3',9X,'4',9X,'5',9X,'6',9X,'7',9X,'8',
     3  9X,'9',9X,'0'/
     4  15X,'0',10('1234567890')/)
C          Print channel contents
        DO 130 K=1,10
          IF(K.EQ.1) WRITE(ITLIS,131) (APRNT(K,J),J=1,NCH)
131       FORMAT(' CONTENTS',7X,100A1)
          IF(K.GT.1) WRITE(ITLIS,132) (APRNT(K,J),J=1,NCH)
132       FORMAT(16X,100A1)
130     CONTINUE
C
C          Print 1-dimensional errors in E format.
C
C          Clear APRNT array
        DO 200 J=1,100
        DO 200 K=1,10
200     APRNT(K,J)=' '
C          Extract errors and convert to E format
        DO 210 J=1,NCH
          Y=HIE(ID,J)
          WRITE(WORD,'(E10.4)') Y
          READ(WORD,'(10A1)') (APRNT(K,J),K=1,10)
202       FORMAT(10A1)
210     CONTINUE
C          Print channel marks
        WRITE(ITLIS,221)
221     FORMAT(//
     1  ' CHANNELS',6X,'0',99X,'1'/
     2  15X,'0',9X,'1',9X,'2',9X,'3',9X,'4',9X,'5',9X,'6',9X,'7',9X,'8',
     3  9X,'9',9X,'0'/
     4  15X,'0',10('1234567890')/)
C          Print channel errors
        DO 230 K=1,10
          IF(K.EQ.1) WRITE(ITLIS,231) (APRNT(K,J),J=1,NCH)
231       FORMAT(' ERRORS  ',7X,100A1)
          IF(K.GT.1) WRITE(ITLIS,232) (APRNT(K,J),J=1,NCH)
232       FORMAT(16X,100A1)
230     CONTINUE
1000  CONTINUE
C
      RETURN
      END
