*
* $Id: pfhigz.F,v 1.2 1996/04/24 08:19:56 dinofm Exp $
*
* $Log: pfhigz.F,v $
* Revision 1.2  1996/04/24 08:19:56  dinofm
* Handling of two 'new' remote HIGZ command: IGCOLM and IPM4ID (that were
* previously dummied out on PIAF)
*
* Revision 1.1.1.1  1996/03/01 11:38:45  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.04/09 15/12/93  15.22.11  by  Fons Rademakers
*-- Author :    Alfred Nathaniel   02/05/93
      SUBROUTINE PFHIGZ(CHMESS,CHLINE,ISLAV,ISTAT)
************************************************************************
*                                                                      *
* Remote HIGZ procedure calls from the master server to the client or  *
* from a slave server to the master server. In case a slave calls the  *
* master, the master in turn makes an RPC to the client and, when      *
* necessary, relays the output parameter(s) back to the slave.         *
*                                                                      *
************************************************************************
*
#include "hbook/hcbook.inc"
#include "hbook/hcpiaf.inc"
*
      CHARACTER*(*) CHMESS, CHLINE
      REAL          RVAL(4)
      CHARACTER     CHMAIL*80, CHOPT*16, CHPID*32
*
*-- IGQ cache used on master server
*
      PARAMETER     (MAXOPT = 20)
      CHARACTER*4    IGOPT(MAXOPT)
      REAL           RGVAL(4,MAXOPT)
      INTEGER        NIGOPT, NIGPID, IC1DUM, INIID, INICOL
*
      SAVE           IGOPT, RGVAL, NIGOPT, NIGPID, INIID, INICOL
*
      ISTAT = 0
*
      IF (CHMESS.EQ.'IGPID') THEN
*
         READ(CHLINE,'(2I12,A,A)') ILEVI,IPID,CHOPT,CHPID
* When master server, relay only the first call to IGPID to the client
         IF (MASTPF) THEN
            IF (NIGPID .EQ. 0) THEN
               NIGPID = 1
               CALL PFSOCK(0)
               CALL IGPID(ILEVI,CHPID,IPID,CHOPT)
            ENDIF
         ELSE
            CALL IGPID(ILEVI,CHPID,IPID,CHOPT)
         ENDIF
*
      ELSEIF (CHMESS.EQ.'IGQ') THEN
*
         READ(CHLINE,'(E16.8,A)') RVAL(1),CHOPT
*
*-- When on master check if option values are already in cache, if they are
*-- don't go to client
*
         IF (MASTPF) THEN
            DO 10 I = 1, NIGOPT
               IF (CHOPT .EQ. IGOPT(I)) THEN
                  WRITE(CHMAIL,'(4E16.8)') (RGVAL(J,I),J=1,4)
                  CALL CZPUTA(CHMAIL,ISTAT)
                  RETURN
               ENDIF
   10       CONTINUE
         ENDIF
*
         IF (MASTPF) CALL PFSOCK(0)
         CALL IGQ(CHOPT,RVAL(1))
         WRITE(CHMAIL,'(4E16.8)') (RVAL(I),I=1,4)
         IF (MASTPF) CALL PFSOCK(ISLAV)
         CALL CZPUTA(CHMAIL,ISTAT)
*
*-- Fill the cache
*
         IF (MASTPF .AND. NIGOPT.LT.MAXOPT) THEN
            NIGOPT = NIGOPT + 1
            IGOPT(NIGOPT) = CHOPT
            DO 20 I = 1, 4
               RGVAL(I,NIGOPT) = RVAL(I)
   20       CONTINUE
         ENDIF
*
      ELSEIF (CHMESS.EQ.'IGSET') THEN
*
         READ(CHLINE,'(E16.8,A)') RVAL(1),CHOPT
         IF (MASTPF) CALL PFSOCK(0)
         CALL IGSET(CHOPT,RVAL(1))
*
      ELSEIF (CHMESS.EQ.'IGCOLM') THEN
*
         READ(CHLINE,'(I12,A)') IC2,CHOPT
         CALL CZGETA(CHMAIL,ISTAT)
         READ(CHMAIL,'(3E16.8)') X1,X2,Y1 
         CALL CZGETA(CHMAIL,ISTAT)
         READ(CHMAIL,'(3E16.8)') Y2,ZMIN,ZMAX 
         IF (MASTPF) THEN
            IF (INICOL.EQ.0) THEN
               INICOL = 1
               CALL PFSOCK(0)
               CALL IGCOLM(X1,X2,Y1,Y2,IC1DUM,IC2,ZMIN,ZMAX,CHOPT)
            ENDIF
         ELSE  
            CALL IGCOLM(X1,X2,Y1,Y2,IC1DUM,IC2,ZMIN,ZMAX,CHOPT)
         ENDIF
*
      ELSEIF (CHMESS.EQ.'IPL3') THEN
*
         READ(CHLINE,'(I12)') N
         NUH=0
         CALL FZIN(999,IHWORK,LPIAF,2,' ',NUH,0)
         IF (MASTPF) CALL PFSOCK(0)
         CALL IPL3(N,Q(LPIAF+1),Q(LPIAF+N+1),Q(LPIAF+N*2+1))
         CALL MZDROP(IHWORK,LPIAF,' ')
*
      ELSEIF (CHMESS.EQ.'IPL') THEN
*
         READ(CHLINE,'(I12)') N
         NUH=0
         CALL FZIN(999,IHWORK,LPIAF,2,' ',NUH,0)
         IF (MASTPF) CALL PFSOCK(0)
         CALL IPL(N,Q(LPIAF+1),Q(LPIAF+N+1))
         CALL MZDROP(IHWORK,LPIAF,' ')
*
      ELSEIF (CHMESS.EQ.'IPM') THEN
*
         READ(CHLINE,'(I12)') N
         NUH=0
         CALL FZIN(999,IHWORK,LPIAF,2,' ',NUH,0)
         IF (MASTPF) CALL PFSOCK(0)
         CALL IPM(N,Q(LPIAF+1),Q(LPIAF+N+1))
         CALL MZDROP(IHWORK,LPIAF,' ')
*
      ELSEIF (CHMESS.EQ.'IPMID') THEN
*
         READ(CHLINE,'(2I12)') N,LEVEL
         NUH=0
         CALL FZIN(999,IHWORK,LPIAF,2,' ',NUH,0)
         NUH=0
         CALL FZIN(999,IHWORK,LPIAF2,2,' ',NUH,0)
         IF (MASTPF) CALL PFSOCK(0)
         CALL IPMID(N,Q(LPIAF+1),Q(LPIAF+N+1),LEVEL,IQ(LPIAF2+1))
         CALL MZDROP(IHWORK,LPIAF,' ')
         CALL MZDROP(IHWORK,LPIAF2,' ')
*
      ELSEIF (CHMESS.EQ.'IPM3ID') THEN
*
         READ(CHLINE,'(2I12)') N,LEVEL
         NUH=0
         CALL FZIN(999,IHWORK,LPIAF,2,' ',NUH,0)
         NUH=0
         CALL FZIN(999,IHWORK,LPIAF2,2,' ',NUH,0)
         IF (MASTPF) CALL PFSOCK(0)
         CALL IPM3ID(N,Q(LPIAF+1),Q(LPIAF+N+1),Q(LPIAF+N*2+1),
     +          LEVEL,IQ(LPIAF2+1))
         CALL MZDROP(IHWORK,LPIAF,' ')
         CALL MZDROP(IHWORK,LPIAF2,' ')
*
      ELSEIF (CHMESS.EQ.'IPM4ID') THEN
*
         READ(CHLINE,'(2I12,2E15.9)') N,LEVEL,V,W
         NUH=0
         CALL FZIN(999,IHWORK,LPIAF,2,' ',NUH,0)
         NUH=0
         CALL FZIN(999,IHWORK,LPIAF2,2,' ',NUH,0)
         IF (MASTPF) THEN
            IF (INIID.EQ.0) THEN
*               INIID = 1
               CALL PFSOCK(0)
               CALL IPM4ID(N,Q(LPIAF+1),Q(LPIAF+N+1),Q(LPIAF+N*2+1),
     +                Q(LPIAF+N*3+1),V,W,LEVEL,IQ(LPIAF2+1))
            ENDIF
         ELSE  
            CALL IPM4ID(N,Q(LPIAF+1),Q(LPIAF+N+1),Q(LPIAF+N*2+1),
     +             Q(LPIAF+N*3+1),V,W,LEVEL,IQ(LPIAF2+1))
         ENDIF
         CALL MZDROP(IHWORK,LPIAF,' ')
         CALL MZDROP(IHWORK,LPIAF2,' ')
*
      ELSEIF (CHMESS.EQ.'ISPMCI') THEN
*
         READ(CHLINE,'(I12)') ICOLI
         IF (MASTPF) CALL PFSOCK(0)
         CALL ISPMCI(ICOLI)
*
      ELSEIF (CHMESS.EQ.'CLEAR') THEN
*
*-- Clear IGQ cache
*
         NIGOPT = 0
         NIGPID = 0
         INICOL = 0
         INIID  = 0
*
      ELSE
         PRINT *,' PFHIGZ:',CHMESS,'?? '
      ENDIF
*
      END
