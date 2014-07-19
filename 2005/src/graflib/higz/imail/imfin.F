*
* $Id: imfin.F,v 1.1.1.1 1996/02/14 13:11:02 mclareni Exp $
*
* $Log: imfin.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:02  mclareni
* Higz
*
*
#if defined(CERNLIB_MAIL)
#include "higz/pilot.h"
*CMZ :  1.09/00 05/12/89  11.55.58  by  O.Couet
*-- Author :    O.Couet   30/01/89
      INTEGER FUNCTION IMFIN(CHBUF,NBREAD,X)
*.===========>
*.
*.   This routine write input CHBUF in X. If IMFIN=0,
*.   the array X is not fill with NBREAD value, if IMFIN
*.   =1 the array is filled with NBREAD value, if IMFIN
*.   =-1, an errors has occured.
*.
*. _Input parameters:
*.
*.  CHARACTER CHBUF : Character buffer
*.  INTEGER NBREAD : Number of reals to be read
*.  REAL X(N) : Array.
*.
*..==========> (O.Couet)
      DIMENSION X(*),IX(16)
      CHARACTER*(*) CHBUF
      SAVE FACT,XMIN
      SAVE IPTR,IX
      DATA IPTR /0/
*.______________________________________
*
      IMFIN=0
*
      IF(NBREAD.EQ.2)THEN
         READ (CHBUF,'(2E16.7)') X(1),X(2)
         IPTR=0
         IMFIN=1
         RETURN
      ENDIF
*
      IF(IPTR.EQ.0)THEN
         READ (CHBUF,'(2E16.7)',ERR=40) FACT,XMIN
         IF(FACT.LE.0.)THEN
            DO 10 K=1,NBREAD
               X(K)=XMIN
   10       CONTINUE
            IPTR=0
            IMFIN=1
         ELSE
            IPTR=1
         ENDIF
         RETURN
      ENDIF
      IF(IPTR+15.LE.NBREAD)THEN
         READ (CHBUF,'(16I5)',ERR=40) (IX(I),I=1,16)
         DO 20 I=IPTR,IPTR+15
            X(I)=FACT*IX(I-IPTR+1)+XMIN
   20    CONTINUE
         IF(IPTR+15.EQ.NBREAD)THEN
            IPTR=0
            IMFIN=1
            RETURN
         ELSE
            IPTR=IPTR+16
            RETURN
         ENDIF
      ELSE
         J=NBREAD-IPTR
         READ (CHBUF,'(16I5)',ERR=40) (IX(I),I=1,J+1)
         DO 30 I=IPTR,IPTR+J
            X(I)=FACT*IX(I-IPTR+1)+XMIN
   30    CONTINUE
         IPTR=0
         IMFIN=1
         RETURN
      ENDIF
   40 IMFIN=-1
*
      END
#endif
