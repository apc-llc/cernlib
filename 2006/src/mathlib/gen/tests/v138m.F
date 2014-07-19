*
* $Id: v138m.F,v 1.1.1.1 1996/04/01 15:01:31 mclareni Exp $
*
* $Log: v138m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:31  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE V138M
C
      CHARACTER*(*) CODE
      PARAMETER (CODE='V138')
C
C     This Subroutine tests the MATHLIB routine V138 RNMNML
CSEQ,IMP64.
#include "iorc.inc"
C     V138   RNMNML
 
      LOGICAL LOK
      CHARACTER*47 HT(10)
 
      DIMENSION NV(47),P(47),PCUM(47),NT(47)
 
      DATA HT( 1) /'34                                             '/
      DATA HT( 2) /'4202450                                        '/
      DATA HT( 3) /'012640506111                                   '/
      DATA HT( 4) /'40063032033203017                              '/
      DATA HT( 5) /'1543313001300112407044                         '/
      DATA HT( 6) /'032011121044222321040645241                    '/
      DATA HT( 7) /'01512142101002014610523932005312               '/
      DATA HT( 8) /'3431210142240120347002420140312810212          '/
      DATA HT( 9) /'346441514002212200064200011242115300301343     '/
      DATA HT(10) /'12433141302001313641340247012411010050316620100'/
 
      CALL HEADER(CODE,1)

      CALL RLUXGO(3,314159265,0,0)
 
      LOK=.TRUE.
      DO 2 J = 1,10
      N=5*J-3
      NSUM=2*N+3
      READ(HT(J),'(47I1)') (NT(I),I=1,N)
      CALL RANLUX(P,N)
      SP=RVSUM(N,P(1),P(2))
      CALL RVSCL(N,1/SP,P(1),P(2),P(1),P(2))
      PCUM(1)=P(1)
      DO 1 I = 2,N-1
    1 PCUM(I)=PCUM(I-1)+P(I)
      PCUM(N)=1
      CALL RNMNML(N,NSUM,PCUM,NV,IERR)
      DO 4 I = 1,N
    4 LOK=LOK .AND. NV(I) .EQ. NT(I)
    2 CONTINUE
      IRC= ITEST(CODE,LOK)
      CALL PAGEND(CODE)
      RETURN
      END
