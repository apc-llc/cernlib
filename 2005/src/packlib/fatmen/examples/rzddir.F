*
* $Id: rzddir.F,v 1.1.1.1 1996/03/07 15:18:00 mclareni Exp $
*
* $Log: rzddir.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:00  mclareni
* Fatmen
*
*
      DIMENSION IQ(2),Q(2),LQ(8000)
      COMMON/PAWC/NWPAW,IXPAWC,IHDIV,IXHIGZ,IXKU,FENC(5),LMAIN,
     +            HCV(499989)
      EQUIVALENCE (LQ(1),LMAIN),(IQ(1),LQ(9)),(Q(1),IQ(1))
      COMMON/QUEST/IQUEST(100)
      COMMON/FATUSE/IDEBFA,IDIVFA,IKDRFA,KOFSFA,KOFUFA,LBFXFA,
     +              LSAVFA,LTOPFA,LBBKFA,LBGNFA,LTDSFA,LBDSFA,
     +              LPRTFA
 
      DIMENSION    KEYZ(10,100)
      DIMENSION    KEYS(10)
      CHARACTER*1  CHQUOT
      CALL HLIMIT(50000)
*
*     Delete bad directories
*
      LPRTFA = 6
 
      CHQUOT = ''''
 
      CALL RZOPEN(1,'//CERN','/fatmen/fmopal/CERN.FATRZ',' ',1024,irc)
 
      CALL RZFILE(1,'CERN','U')
 
      CALL RZCDIR('//CERN/OPAL/DDST/PASS4/FYZ1',' ')
      CALL RZCDIR(' ','P')
 
      CALL RZDELT('P09')
      PRINT *,'Return code ',IQUEST(1),' from RZDELT'
 
      CALL RZEND('CERN')
      END
