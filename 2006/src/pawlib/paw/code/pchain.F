*
* $Id: pchain.F,v 1.1.1.1 1996/03/01 11:38:44 mclareni Exp $
*
* $Log: pchain.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:44  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.05/21 17/10/94  20.11.41  by  Fons Rademakers
*-- Author :    Fons Rademakers   23/10/92
      SUBROUTINE PCHAIN
*.
*.     Handling of Ntuple chains
*.
#include "paw/pcaddr.inc"
#include "paw/pcpatl.inc"
#include "paw/pcchar.inc"
#include "paw/pawchn.inc"
#include "hbook/hcpiaf.inc"
*
      INTEGER         ICHAIN, IFILE, ILUN
      PARAMETER      (ICHAIN = 1, IFILE = 2, ILUN = 3)
*
      CHARACTER*128 MEMBER, TMEM
      CHARACTER*80  CCHAIN
      CHARACTER*5   FILCHR
*
      DATA FILCHR /'.:/;$'/
*
      CALL KUPATL(CHPATL, NPAR)
      CALL KUGETS(CCHAIN, N)
*
      IF (CCHAIN .EQ. ' ') THEN
*
*-- list chain
*
         CALL PCHLST(' ', 0)
*
      ELSEIF (CCHAIN .EQ. '-') THEN
*
*-- delete all chains
*
         CALL PCHCLO(' ')
         CALL PCHDEL(' ', 0, ' ', 0)
#if defined(CERNLIB_CZ)
         IF (CONNPF .AND. .NOT.SERVPF) THEN
            CALL PFKUIP('nt/chain -', ISTAT)
         ENDIF
#endif
*
      ELSEIF (CCHAIN(1:1) .EQ. '-') THEN
*
*-- delete chain
*
         IF (N .GT. 1) THEN
            CALL PCHCLO(CCHAIN(2:))
            CALL PCHDEL(CCHAIN(2:), N-1, ' ', 0)
#if defined(CERNLIB_CZ)
            IF (CONNPF .AND. .NOT.SERVPF) THEN
               CALL PFKUIP('nt/chain -', ISTAT)
            ENDIF
#endif
         ENDIF
*
      ELSEIF (CCHAIN(N:N) .EQ. '>') THEN
*
*-- list chain tree
*
         IF (JMCHAIN .EQ. 0) THEN
            CALL PCHLST(CCHAIN, N)
         ELSE
            CHTEMP = CCHAIN(1:N-1)
            CALL JUMPST(JMCHAIN)
            CALL JUMPX0
         ENDIF
*
      ELSEIF (NPAR-1 .GT. 0) THEN
         IADDM = 0
**       CALL KUGETF(MEMBER, NM)
         CALL KUGETS(MEMBER, NM)
         CALL CUTOL(MEMBER(1:NM))
*
  10     CALL KUGETL(MEMBER, NM)
         IF (MEMBER .NE. ' ') THEN
*
            IF (MEMBER.EQ.'-P' .OR. MEMBER.EQ.'-p') THEN
*
*-- set or print path of chain
*
               CALL KUGETL(MEMBER, NM)
               IF (MEMBER .EQ. '-') THEN
                  NM = 0
                  CALL PCHPAT(CCHAIN, N, MEMBER, NM)
               ELSEIF (MEMBER .NE. ' ') THEN
                  CALL PCHPAT(CCHAIN, N, MEMBER, NM)
               ELSE
                  NM = -1
                  CALL PCHPAT(CCHAIN, N, MEMBER, NM)
                  IF (NM .GT. 0) THEN
                     PRINT 1000, CCHAIN(1:N), MEMBER(1:NM)
                  ELSEIF (NM .EQ. 0) THEN
                     PRINT 1010, CCHAIN(1:N)
                  ENDIF
               ENDIF
*
               RETURN
*
            ELSEIF (MEMBER(1:1) .EQ. '-') THEN
*
*-- delete member from chain
*
               IF (NM .GT. 1) THEN
*
*-- this hack is necessary because KUIP eats the // in the middle of a string,
*-- e.g.: -//lun50 becomes -lun50
*
                  IF (MEMBER(1:4) .EQ. '-lun') THEN
                     TMEM = MEMBER
                     MEMBER = '-//'//TMEM(2:)
                     NM = LENOCC(MEMBER)
                  ENDIF
                  CALL PCHDEL(CCHAIN, N, MEMBER(2:), NM-1)
               ENDIF
            ELSE
*
*-- add member, first try to deduce its type
*
               IADDM = 1
               TMEM  = MEMBER
               CALL CLTOU(TMEM)
*
*-- is it a logical unit?
*
               IF (TMEM(1:5) .EQ. '//LUN') THEN
                  I = INDEX(MEMBER(3:),'/')
                  IF (I .EQ. 0) THEN
                     CALL PCHADD(CCHAIN, N, MEMBER, NM, ILUN, ISTAT)
                  ELSE
                     CALL PCHADD(CCHAIN,N,MEMBER(:I+1),I+1,ILUN,ISTAT)
                  ENDIF
               ELSE
*
*-- or a file name?
*
                  DO 20 J = 1, LENOCC(FILCHR)
                     IF (INDEX(TMEM,FILCHR(J:J)) .NE. 0) THEN
                        CALL PCHADD(CCHAIN,N,MEMBER,NM,IFILE,ISTAT)
                        GOTO 10
                     ENDIF
  20              CONTINUE
*
*-- if not then it must be a chain
*
                  CALL PCHADD(CCHAIN, N, MEMBER, NM, ICHAIN, ISTAT)
               ENDIF
            ENDIF
*
            GOTO 10
*
         ENDIF
*
         CHNXMT = .TRUE.
*
*-- make just added chain the CWD and open the first file in chain
*
*** have to de explicit CD //chain
***         IF (IADDM .NE. 0) CALL PCHNCD(CCHAIN, IER)
      ELSE
*
*-- list chain entries
*
         CALL PCHLST(CCHAIN, N)
      ENDIF
*
1000  FORMAT('Chain ',A,': path is ',A)
1010  FORMAT('Chain ',A,': no path specified')
*
      END
