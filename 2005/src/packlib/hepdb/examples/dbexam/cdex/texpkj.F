*
* $Id: texpkj.F,v 1.1.1.1 1996/02/28 16:23:40 mclareni Exp $
*
* $Log: texpkj.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:40  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE TEXPKJ(JTX,JNM,JNX,JMX,JPAK)
*     ----------------------------------====-
*     -- Help-to-TEXINS : pack and unpack --
*     --   string pointers to/from JPAK   --
*
      IMPLICIT NONE
      INTEGER  JTX,JNM,JNX,JMX,JPAK
*
      JPAK=((JTX*200+JNM)*200+JNX)*200+JMX
      RETURN
*
*--                    Author :    Boris Khomenko   28/02/94
      ENTRY      TEXUPJ(JPAK,JTX,JNM,JNX,JMX)
*     -----------------------===-===-===-===-
      JTX=MOD(JPAK/8000000,200)
      JNM=MOD(JPAK/  40000,200)
      JNX=MOD(JPAK/    200,200)
      JMX=MOD(JPAK        ,200)
      END
