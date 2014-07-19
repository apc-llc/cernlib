*
* $Id: pfclos.F,v 1.1.1.1 1996/03/01 11:38:45 mclareni Exp $
*
* $Log: pfclos.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:45  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.04/12 13/02/94  21.37.53  by  Fons Rademakers
*-- Author :    Alfred Nathaniel   01/05/93
      SUBROUTINE PFCLOS
*
*          Close the connection to the Piaf server and
*          remove all remote file table entries
*
#if defined(CERNLIB_CZ)
#include "paw/pawlun.inc"
#include "hbook/hcdire.inc"
#include "hbook/hcpiaf.inc"
*
*=========================================================
*
 1    CONTINUE
      DO 20 I=1,NCHTOP
         IF(ICHTOP(I).GT.1000 .AND.
     +      MOD(ICHTOP(I),10000).EQ.CLINPF) THEN
            IF(ICHLUN(I).EQ.0) THEN
*
*-- Close socket connection to the Piaf server
*
               CALL PFSOCK(0)
               CALL PZSYNC(-1)
               CALL CZCLOS(ISTAT)
               PRINT *,' *** Connection to Piaf server closed'
            ENDIF
*
*-- Remove socket and file entries from Hbook tables (see also HREND)
*
            CALL HNTDEL(CHTOP(I))
            LUNIT(ICHLUN(I)) = 0
            NCHTOP=NCHTOP-1
            DO 10 J=I,NCHTOP
               ICHTOP(J)=ICHTOP(J+1)
               ICHLUN(J)=ICHLUN(J+1)
               ICHTYP(J)=ICHTYP(J+1)
               CHTOP(J)=CHTOP(J+1)
               HFNAME(J)=HFNAME(J+1)
 10         CONTINUE
*
            IF(I.EQ.ICDIR) THEN
               CALL HCDIR('//PAWC',' ')
            ENDIF
*
            GOTO 1
         ENDIF
 20   CONTINUE
      CONNPF=.FALSE.
*
#endif
#if !defined(CERNLIB_CZ)
      PRINT *,' PAW not compiled with communication option'
#endif
      END
