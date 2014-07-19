*
* $Id: fmvaxc.F,v 1.1.1.1 1996/03/07 15:18:22 mclareni Exp $
*
* $Log: fmvaxc.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:22  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMVAXC(NODE,ICODE)
*CMZ :          26/11/90  16.11.50  by  Jamie Shiers
*-- Author :    Jamie Shiers   26/11/90
        CHARACTER*(*) NODE
        CHARACTER*8   TNODE
        INTEGER       FMHOST
        CHARACTER*8   CHNODE,CHTYPE,CHSYS,CLUNAM,CLUNOD
#include "fatmen/fatbug.inc"
#if defined(CERNLIB_VAXVMS)
      INCLUDE '($SSDEF)'
       STRUCTURE /ITMLST/
         UNION
            MAP
            INTEGER*2 BUFFER_LENGTH
              INTEGER*2 ITEM_CODE
            INTEGER*4 BUFFER_ADDRESS
              INTEGER*4 RETURN_LENGTH_ADDRESS
            ENDMAP
            MAP
            INTEGER*4 END_LIST /0/
            ENDMAP
         END UNION
      END STRUCTURE
 
      RECORD /ITMLST/ SYI_LIST(2)
 
      INCLUDE '($SYIDEF)'
#endif
      LN     = LENOCC(NODE)
      IF(IDEBFA.GE.2) PRINT *,'FMVAXC. enter for ',NODE(1:LN)
      TNODE  = NODE
      IC     = FMHOST(CHNODE,CHTYPE,CHSYS)
      LCHN   = LENOCC(CHNODE)
      IF(IDEBFA.GE.2) PRINT *,'FMVAXC. current node is ',
     +   CHNODE(1:LCHN)
 
      CALL CLTOU(TNODE)
      CALL CLTOU(CHNODE)
*
*     Node names match...
*
      IF(TNODE(1:LN).EQ.CHNODE(1:LCHN)) THEN
         ICODE = 0
         RETURN
      ENDIF
#if defined(CERNLIB_VAXVMS)
*
*     Nodename specified is the VAXcluster alias
*
      CALL FMGTLG('SYS$CLUSTER_NODE',CLUNAM,'LNM$SYSTEM',IRC)
      IF(IRC.EQ.0) THEN
         LENCLU = INDEX(CLUNAM,':') -1
         IF(IDEBFA.GE.2) PRINT *,'FMVAXC. cluster alias is ',
     +      CLUNAM(1:LENCLU)
         IF(TNODE(1:LN).EQ.CLUNAM(1:LENCLU)) THEN
            ICODE = 1
            RETURN
         ENDIF
      ENDIF
*
*     Check whether the node is in the same VAXcluster as us
*
      MEMBER                            = 0
      SYI_LIST(1).BUFFER_LENGTH         = 1
      SYI_LIST(1).ITEM_CODE             = SYI$_CLUSTER_MEMBER
      SYI_LIST(1).BUFFER_ADDRESS        = %LOC(MEMBER)
      SYI_LIST(1).RETURN_LENGTH_ADDRESS = %LOC(LENGTH)
      SYI_LIST(2).END_LIST              = 0
 
      ISTAT = SYS$GETSYIW(,,TNODE(1:LN),SYI_LIST,,,)
 
*     IF (ISTAT .NE. SS$_NOSUCHNODE) GOTO 99
      IF(MEMBER.EQ.0) GOTO 99
      IF(IDEBFA.GE.2) PRINT *,'FMVAXC. ',TNODE(1:LN),
     +   ' is a VAXcluster member'
       ICODE = 1
       RETURN
#endif
99    CONTINUE
      ICODE = 2
      END
 
