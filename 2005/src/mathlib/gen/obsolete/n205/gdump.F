*
* $Id: gdump.F,v 1.1.1.1 1996/04/01 15:03:21 mclareni Exp $
*
* $Log: gdump.F,v $
* Revision 1.1.1.1  1996/04/01 15:03:21  mclareni
* Mathlib gen
*
*
#include "sys/CERNLIB_machine.h"
#include "_gen/pilot.h"
#if defined(CERNLIB_CDC)
      SUBROUTINE GDUMP (IBUF,N,MESSAG)
      DIMENSION IBUF(N),MESSAG(3)
C
C     INTERPRETED DUMP OF AN ARRAY.
C
C     IBUF     ARRAY TO BE DUMPED
C     N        ARRAY LENGTH
C     MESSAG   THREE WORDS FOR DUMP HEADING
C
C     A BIG INTEGER MAY BE MIS-INTERPRETED AS A FLOATING POINT NUMBER
C     A FLOATING POINT NUMBER MAY BE MIS-INTERPRETED AS ALPHABETIC
C     NOT ALL SPECIAL CHARACTERS ARE RECOGNIZED AS ALPHABETIC.
C
      DIMENSION IFMT(23),IFLOAT(2)
      LOGICAL ISI,ISA,EQ
C
C-----------------------------------------------------------------------
C     M A C H I N E   D E P E N D E N T   V A R I A B L E S
C     -----------------------------------------------------
C              LOGICAL UNIT FOR PRINTER
      DATA NTBCD /2/
C              MAXIMUM POSSIBLE INTEGER, MACHINE DEPENDENT
      DATA MAXINT / 00077777777777777777B  /
C              NWF=2 IF ONLY 4 CHARACTERS IN A WORD, OTHERWISE CAN BE SE
C              SET TO 1
      DATA NWF /1/
C     DATA NWF /2/
      DATA IFLOAT(2) /1H /
      DATA IFLOAT /8HG12.5          /
C              IF FORMAT G DOES NOT WORK PROPERLY, USE THE FOLL. FORMAT
C     DATA IFLOAT /8HE12.6         /
C
C     NOTE
C     ----
C       SEE ALSO ROUTINE ISA
C-----------------------------------------------------------------------
C              FUNCTION ISI CHECKS IF ITEM IS INTEGER
      ISI(IARG)=ABS(IARG).LE.MAXINT
C
      DATA IFMT /8H(I5,2H *  , 20*1H  , 1H)/
      DATA IALPHA /4H,A12/
      DATA  INTEGR /4H,I12/
      DATA ICLOSE /1H) /
C
      WRITE (NTBCD,1) MESSAG
      NEQP=1
C
      DO 200 N1=1,N,10
      N2=MIN(N1+9,N)
      IPOINT = 3
      EQ=N1.NE.1
C
      DO 160 JP=N1,N2
      IW=IBUF(JP)
      IF (ISI(IW)) GO TO 110
      IF (ISA(IW)) GO TO 120
C              FLOAT
      IFMT(IPOINT)=IFLOAT(1)
      IFMT(IPOINT+1)=IFLOAT(2)
      IPOINT=IPOINT+NWF
      GO TO 150
C              INTEGER
 110  IFMT(IPOINT)=INTEGR
      GO TO 140
C              ALPHABETIC
 120  IFMT(IPOINT)=IALPHA
C
 140  IPOINT=IPOINT+1
C
 150  IF (EQ) EQ= IBUF(JP-10).EQ.IBUF(JP)
 160  CONTINUE
C
      IFMT(IPOINT)=ICLOSE
C
      IF (EQ) GO TO 200
      IF (NEQP.EQ.N1) GO TO 170
      NEQL=N1-1
      WRITE (NTBCD,2) NEQP,NEQL
C
 170  NEQP=N1+10
      WRITE (NTBCD,IFMT) N1,(IBUF(J),J=N1,N2)
C
 200  CONTINUE
      IF (EQ) WRITE (NTBCD,2) NEQP,N
      RETURN
C              IBM 360, SDS
C1    FORMAT('0',40X,3A4)
C              UNIVAC
C1    FORMAT('0',30X,3I6)
C              CDC
 1    FORMAT('0',30('-'),1X,3A10)
 2    FORMAT(61X,I5,' -',I5,3X,'THE SAME')
      END
#endif
