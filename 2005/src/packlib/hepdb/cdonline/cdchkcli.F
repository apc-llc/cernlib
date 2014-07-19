*
* $Id: cdchkcli.F,v 1.1.1.1 1996/02/28 16:24:18 mclareni Exp $
*
* $Log: cdchkcli.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:18  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CD_CHK_CLIENT (CD_CURRENT_SNAME, CD_CURRENT_LNAME,
     +                          CD_CLUSCOM_NAME, IERR)
*     =============================================================
*
************************************************************************
*                                                                      *
*        SUBR. CD_CHK_CLIENT (CD_CURRENT_SNAME*, CD_CURRENT_LNAME*,    *
*                             CD_CLUSCOM_NAME*, IERR*)                 *
*                                                                      *
*   Finds the short, long names and the CLUSCOM name for the task      *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     CD_CURRENT_SNAME(*) Short name                                   *
*     CD_CURRENT_LNAME(*) Long name                                    *
*     CD_CLUSCOM_NAME(*)  Name of the CLUSCOM                          *
*     IERR(*)             Error code (0 if no error)                   *
*                                                                      *
*   Called by CDINIT                                                   *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
      CHARACTER*2     CD_CURRENT_SNAME
      CHARACTER*4     CD_CURRENT_LNAME
      CHARACTER*9     CD_CLUSCOM_NAME
      CHARACTER*2     CLIENT_LABEL
*           Process names stuff
      CHARACTER       P_NODENAME*31, P_PRCNAME*31, P_IMGNAME*80
      INTEGER*4       P_NODEN_L , P_PRCN_L, P_IMGN_L
      LOGICAL         VALID_PRC , VALID_IMG
*           Database Ids for logical names
      PARAMETER       (NUM_CD=9)
      CHARACTER*2     CD_SNAME(NUM_CD)
      CHARACTER*4     CD_LNAME(NUM_CD)
      DATA            CD_SNAME /'TE', 'EC', 'HC', 'MU', 'FL', 'JT',
     +                          'L3', 'SC', 'VS'/
      DATA            CD_LNAME /'TECH', 'ECAL', 'HCAL', 'MUCH', 'FLUM',
     +                          'TRIG', 'L3RC', 'SCNT', 'VSMD'/
*
*     ------------------------------------------------------------------
*
      VALID_IMG    = .FALSE.
      VALID_PRC    = .FALSE.
      CLIENT_LABEL = 'XX'
      CALL CD_G_NODE    (P_NODENAME, P_NODEN_L)
      CALL CD_G_NAME    (P_PRCNAME , P_PRCN_L)
      CALL CD_G_IMGNAME (P_IMGNAME , P_IMGN_L)
      K_CD_P     = 0
      K_CD_I     = 0
      DO 10 I = 1, NUM_CD
        IF (P_IMGNAME(1:1).EQ.CD_SNAME(I)(1:1)) THEN
          VALID_IMG = .TRUE.
          K_CD_I    = I
        ENDIF
   10 CONTINUE
      IF (.NOT.VALID_IMG) THEN
        CALL CDPRNT (L3PRDB, '('' CD_CHK_CLIENT (CDINIT) NON FATAL ER'//
     +       'ROR -Incorrect Image Name: '//P_IMGNAME(1:P_IMGN_L)//''')'
     +,      IARGCD, 0)
        DO 15 I = 1, NUM_CD
          IF (P_PRCNAME(1:1).EQ.CD_SNAME(I)(1:1)) THEN
            VALID_PRC = .TRUE.
            K_CD_P    = I
          ENDIF
   15   CONTINUE
        IF (VALID_PRC) THEN
          CD_CURRENT_SNAME = CD_SNAME(K_CD_P)
          CD_CURRENT_LNAME = CD_LNAME(K_CD_P)
          IF (P_PRCN_L.EQ.9) CLIENT_LABEL = P_PRCNAME(8:9)
        ENDIF
      ELSE
        CD_CURRENT_SNAME = CD_SNAME(K_CD_I)
        CD_CURRENT_LNAME = CD_LNAME(K_CD_I)
        IF(P_IMGN_L.EQ.9) CLIENT_LABEL = P_IMGNAME(8:9)
      ENDIF
      IF (VALID_PRC.OR.VALID_IMG) THEN
        CD_CLUSCOM_NAME(1:2) = CD_CURRENT_SNAME
        CD_CLUSCOM_NAME(3:7) = '_CDC_'
        CD_CLUSCOM_NAME(8:9) = CLIENT_LABEL
        IERR = 0
      ELSE
        CD_CLUSCOM_NAME = 'WRONGNAME'
        IERR = 1
      ENDIF
*                                                      END CD_CHK_CLIENT
      END
