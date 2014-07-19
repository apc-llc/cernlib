*
* $Id: uxtovm.F,v 1.1.1.1 1996/02/15 17:47:41 mclareni Exp $
*
* $Log: uxtovm.F,v $
* Revision 1.1.1.1  1996/02/15 17:47:41  mclareni
* Kernlib
*
*
*  UXTOVM: Translate Unix-filenames to VM
* This directory was created from kernbit.car patch m444
      CHARACTER*(*) FUNCTION UXTOVM(UXFILE)
*
* CERN PROGLIB# M444    UXTOVM           .VERSION KERNBIT  1.07  910903
* Author: Miguel Marquina 91/09/03
*
* Translates Unix-filenames to VM syntax (adequate for OPEN,INQUIRE)
*
* Mods       Date   Comments
*
      CHARACTER*(*) UXFILE
      CHARACTER*21  VMFILE
*
      VMFILE='/FNNONE   FTNONE   * '
      LS =LENOCC(UXFILE)
      IFN=INDEXB(UXFILE,'/')+1
      IFT=INDEX(UXFILE(IFN:),'.')+IFN
      IFM=INDEX(UXFILE(IFT:),'.')+IFT
*
      IF(IFM.EQ.IFT) IFM=LS+2
*
      IF    (IFT.EQ.1) THEN
*--   /dir/fn
          VMFILE( 2: 9)=UXFILE(IFN:)
      ELSEIF(IFT.EQ.2) THEN
*--   /dir/.ft
          VMFILE( 2: 9)='DOT'
          VMFILE(11:18)=UXFILE(IFT:IFM-2)
      ELSE
*--   /dir/fn.ft
          VMFILE( 2: 9)=UXFILE(IFN:IFT-2)
          IF(IFT.LE.LS)
     +    VMFILE(11:18)=UXFILE(IFT:IFM-2)
      ENDIF
      IF(IFM.LE.LS.AND.IFN.EQ.1) THEN
*--   fn.ft.fm
          VMFILE(20:21)=UXFILE(IFM:MIN(2,IFM+2))
      ENDIF
*--   Translate special types when there is a directory
      IF(IFN.GT.1) THEN
        IF(VMFILE(11:18).EQ.'f') VMFILE(11:18)='FORTRAN'
        IF(VMFILE(11:18).EQ.'o') VMFILE(11:18)='TEXT'
        IF(VMFILE(11:18).EQ.'a') THEN
                                 VMFILE(11:18)='TXTLIB'
         IF(VMFILE(2:4).EQ.'lib')VMFILE( 2: 9)=UXFILE(IFN+3:IFT-2)
        ENDIF
      ENDIF
      CALL CLTOU(VMFILE)
      UXTOVM=VMFILE
      RETURN
      END
