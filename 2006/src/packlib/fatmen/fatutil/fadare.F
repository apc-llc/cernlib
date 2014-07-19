*
* $Id: fadare.F,v 1.1.1.1 1996/03/07 15:18:07 mclareni Exp $
*
* $Log: fadare.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:07  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FADARE(GENAM,LFAT,KEYS,CHLINE,CHOPT,IRC)
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
      CHARACTER*(*)  GENAM,CHLINE,CHOPT
#include "fatmen/fmnkeys.inc"
      DIMENSION      KEYS(LKEYFA)
      CHARACTER*255  DSN
      CHARACTER*8    NODE
      CHARACTER*4    CHFORM
      CHARACTER*1    CHACC
      CHARACTER*20   CHMETH
      CHARACTER*255  CHFILE
      INTEGER        FMNODE
*
*                 1 : VM/CMS mini-disk
*                 2 : VM/CMS SFS
*                11 : VAX/VMS disk
*                12 : VAXcluster disk
*                13 : DFS (DECnet)
*                14 : DECnet
*                15 : CSPACK (zserv)
*                16 : FPACK
*                21 : Unix disk
*                22 : NFS (e.g. $VARIABLE/file)
*                23 : AFS
*                24 : Shift pool file
*                25 : Shift private file
*                31 : Lachman OSM file
*                32 : Unitree file
*                33 : ADSM file
*                34 : E-MASS file
*
      LBANK = LFAT + KOFUFA
      DSN   = ' '
      NODE  = ' '
      CALL UHTOC(IQ(LBANK+MFQNFA),4,DSN,NFQNFA)
      CALL UHTOC(IQ(LBANK+MHSNFA),4,NODE,NHSNFA)
      CALL UHTOC(IQ(LBANK+MFLFFA),4,CHFORM,NFLFFA)
      LDSN  = LENOCC(DSN)
      CALL FAINQR(DSN(1:LDSN),NODE,CHFILE,IRC)
#if defined(CERNLIB_FPACK)
*
*     FPACK files
*
      IF(IRC.NE.0.AND.FMNODE(NODE).NE.0.AND.
     +   CHFORM(1:2).EQ.'FP') THEN
         CALL FMNTRC(NODE,CHFORM,IRC)
         IF(IRC.EQ.0) IQUEST(1) = 16
      ENDIF
#endif
#if defined(CERNLIB_CSPACK)
*
*     ZEBRA exchange format files
*
      IF(IRC.NE.0.AND.FMNODE(NODE).NE.0.AND.
     +   (CHFORM(1:2).EQ.'FX'.OR.CHFORM(1:3).EQ.'FFX')) THEN
         CALL FMNTRC(NODE,CHFORM,IRC)
         IF(IRC.EQ.0) IQUEST(1) = 15
      ENDIF
#endif
 
      IACC  = IQUEST(1)
      IF(IRC.EQ.0) THEN
         CHACC = 'Y'
      IF(IACC.EQ.1) THEN
         CHMETH = 'VM/CMS mini-disk'
      ELSEIF(IACC.EQ.2) THEN
         CHMETH = 'VM/CMS SFS'
      ELSEIF(IACC.EQ.11) THEN
         CHMETH = 'VAX/VMS disk'
      ELSEIF(IACC.EQ.12) THEN
         CHMETH = 'VAXcluster disk'
      ELSEIF(IACC.EQ.13) THEN
         CHMETH = 'VAX DFS'
      ELSEIF(IACC.EQ.14) THEN
         CHMETH = 'DECnet'
      ELSEIF(IACC.EQ.15) THEN
         CHMETH = 'CSPACK'
      ELSEIF(IACC.EQ.16) THEN
         CHMETH = 'FPACK'
      ELSEIF(IACC.EQ.21) THEN
         CHMETH = 'Unix disk'
      ELSEIF(IACC.EQ.22) THEN
         CHMETH = 'NFS'
      ELSEIF(IACC.EQ.23) THEN
         CHMETH = 'AFS'
      ELSEIF(IACC.EQ.24) THEN
         CHMETH = 'Shift pool file'
      ELSEIF(IACC.EQ.23) THEN
         CHMETH = 'Shift private file'
      ELSEIF(IACC.EQ.31) THEN
         CHMETH = 'Lachman OSM file'
      ELSEIF(IACC.EQ.32) THEN
         CHMETH = 'Unitree file'
      ELSEIF(IACC.EQ.33) THEN
         CHMETH = 'Adstar ADSM file'
      ELSEIF(IACC.EQ.34) THEN
         CHMETH = 'E-MASS file'
      ELSE
         CHMETH = 'unknown'
      ENDIF
      ELSE
         CHACC = 'N'
         CHMETH = 'unknown'
      ENDIF
 
      WRITE(CHLINE,9001) CHACC,CHMETH
9001  FORMAT('Accessible: ',A1,' via: ',A)
 
      END
