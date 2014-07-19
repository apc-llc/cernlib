*
* $Id: putget.F,v 1.1.1.1 1996/02/15 17:50:00 mclareni Exp $
*
* $Log: putget.F,v $
* Revision 1.1.1.1  1996/02/15 17:50:00  mclareni
* Kernlib
*
*
#include "kerngen/pilot.h"
#if defined(CERNLIB_QS_DOS)
      SUBROUTINE PUTGET(EINSTR,LCHSTR,HISCMD,MAXCUR,HICMD)
*
*     PARAMETERS:
*
*     EINSTR  C  ENTERED COMMAND STRING
*     LCHSTR I   LENGTH OF THE INSTR
*     HISCMD C   STACK CONTAINING STRINGS ENTERED EARLIER
*     MAXCUR I   MAX CAPACITY OF THE STACK
*     HICMD  I   CURRENT POSITION INSIDE STACK
*
c
c       These are the keyboard scan codes used

        PARAMETER (k_ctrl_C = 3, k_ctrl_Z=26,k_ctrl_Q=17)
        PARAMETER (k_ctrl_K =11)
        PARAMETER (k_rt_arr = -77, k_lt_arr = -75)
        PARAMETER (k_up_arr = -72, k_dn_arr = -80)
        PARAMETER (k_PGUP = -73, k_PGDW = -81)
        PARAMETER (k_HOME = -71, k_END = -79)
        PARAMETER (k_INS = -82, k_DEL = -83, k_ENTER = 13)
        PARAMETER (k_CR = 13)
        PARAMETER (k_BS = 8, k_ESC = 27)
        PARAMETER (k_alt_H = -35)
        PARAMETER (k_beep = 7)

*
*
      INTEGER HICMD
      CHARACTER*(*) HISCMD(*)
      INTEGER*2    MAXCUR

      PARAMETER (ICRCOL = 2)
      integer*2 icur, lstr, ival, isym, iupdt, icurod, INS
      CHARACTER*(*) EINSTR
      CHARACTER*(255) INSTR
      CHARACTER*1 CURSOR,CURTYP(2)
#include "kerngen/grex.inc"
      ixg(ix) = ix*8
      iyg(iy) = iy*16+5
      IND(I) = ISHFT(I+2,-1)+1
*
      data ins /1/
     *     CURTYP / '','_'/
*
*     Init work
*
CCC      MAXLIN = min(LEN(EINSTR),LEN(INSTR))
      MAXLIN = LEN(INSTR)
      icur = 1
      lstr = icur
      ival = 0
      einstr = ' '
      instr = ' '
      ivideo =  get_video_mode(max_row,max_column)
**      WRITE(77,*) ' video MODE=',IVIDEO
      ier = get_cursor(ixc,iyc)
      ixc = ixc-1
      if (ivideo .ne. 3) then
        icurod = icur
        ier = magnify_text(1,1)
**        iii = get_clip_limits(imlimx,imlimy,ixlimx,ixlimy)
        iii = get_device_limits(limx,limy,limc)
        ier = set_clip_limits(0,0,limx,limy)
        CURSOR =CURTYP(IND(INS))
*
*       Set Graphic cursor
*
        ier=graphic_text(CURSOR,
     *                 ixg(icur+ixc),iyg(iyc),ICRCOL)
      ELSE
        ier = locate(icur+ixc,iyc)
      END IF

*
   10 continue
*
*       Reading next char from keyboard
*
      isym = pause()
      chone =char(isym)
***      write(77,222) '  Input char=',chone,' lstr=',lstr
***  222  format(3a,i2)
*
      IUPDT = -1
      if (isym .ge. 32 .AND. LSTR .LE. MAXLIN-2) then
        IF (INS.EQ.1) THEN
          do i=lstr,icur,-1
            j = i+1
            INSTR(j:j)=INSTR(i:i)
          end do
          INSTR(ICUR:ICUR)=chone
          LSTR = LSTR+1
*
*   Clear the next free positon
*
          INSTR(LSTR:LSTR)=' '
        ELSE
          INSTR(ICUR:ICUR)=chone
          IF (ICUR .EQ. LSTR) LSTR=LSTR+1
        END IF
        ICUR = ICUR+1
        isht = 1
        IUPDT = LSTR-isht
*
      else if (isym .eq. k_enter) then
        lstr = lstr-1
        go to 999
*
      else if (isym .eq. k_ctrl_Z) then
*        lstr = -lstr
        lstr = -1
        go to 999
*
*
      else if (isym .eq. k_up_arr) then
         if (ival .ge. maxcur) then
             ier = beep()
             go to 500
         end if
         ival = ival+1
         go to 20
*
      else if (isym .eq. k_dn_arr) then
        ival = ival-1
*******
   20   if (ival .gt. 0 .AND. HICMD .GT. 0) then
           I = HICMD - IVAL+1
           IF (I .LT. 1) I = MAXCUR+I
           LFUL = LENOCC(HISCMD(I))
           IF (LFUL .LT. MAXLIN) THEN
             IF (LFUL .GE. ICUR) THEN
*
*       Copy line from history into the current string
*
              INSTR(ICUR:LFUL)=HISCMD(I)(ICUR:LFUL)
              IF (LFUL .LT. LSTR) THEN
                INSTR(LFUL+1:LSTR) = ' '
              END IF
              IUPDT = MAX(LSTR,LFUL+1)
              LSTR  = LENOCC(INSTR)+1
             END IF
           ELSE
             ier = beep()
             go to 500
           END IF
        else
*
*    Clear line
*
          ier   = beep()
          IUPDT = LSTR
          ICUR  = 1
          LSTR  = ICUR
          INSTR = ' '
          IVAL  = 0
        end if
*
      else if (isym .eq. k_rt_arr) then
         icur = MIN(ICUR,LSTR)+1
         IF (ICUR .EQ. LSTR+1) THEN
           IER = BEEP()
           ICUR = ICUR - 1
         END IF
*
      else if (isym .eq. k_lt_arr) then
         icur = MAX(1,ICUR)-1
         IF (ICUR .EQ. 0) THEN
           IER = BEEP()
           ICUR = ICUR + 1
         END IF
*
      else if (isym .eq. k_DEL)    then
         IF (ICUR .LE. LSTR-1) THEN
           INSTR(ICUR:LSTR)=INSTR(ICUR+1:LSTR)//' '
           IUPDT = LSTR
           LSTR  = LSTR - 1
         ELSE
           IER = BEEP()
         END IF
*
      else if (isym .eq. k_BS)    then
         ICUR = ICUR-1
         IF (ICUR .GE. 1) THEN
           IUPDT = LSTR
           INSTR(ICUR:LSTR)=INSTR(ICUR+1:LSTR)//' '
           LSTR = MAX(ICUR,LSTR-1)
         ELSE
           IER = BEEP()
           ICUR = 1
         END IF
*
      else if (isym .eq. k_INS)    then
         INS = -INS
         IF (IVIDEO .GT. 3) THEN
           IER = SET_XOR(1)
*
*       Clear OLD Graphic cursor
*
           ier=graphic_text(CURSOR,
     *                 ixg(icur+ixc),iyg(iyc),ICRCOL)
           CURSOR = CURTYP(IND(INS))
*
*       Set NEW Graphic cursor
*
           ier=graphic_text(CURSOR,
     *                 ixg(icur+ixc),iyg(iyc),ICRCOL)
           IER = SET_XOR(0)
         END IF
*
      else if (isym .eq. k_END)    then
         ICUR = LSTR
*
      else if (isym .eq. k_HOME)   then
         ICUR = 1
*
      else if (isym .eq. k_ESC)    then
         IUPDT = LSTR
         ICUR = 1
         LSTR = ICUR
         INSTR = ' '
*
      else if (isym .eq. k_ctrl_K) then
         IUPDT = LSTR
         INSTR(ICUR:LSTR) = ' '
         LSTR = ICUR
*
      else if (isym .eq. k_ctrl_Q) then
*         CALL KIFLTH(2)
         LSTR = -2
         GO TO 999
*
      else if (isym .eq. k_ctrl_C) then
*         CALL KIFLTH(2)
         LSTR = -2
         GO TO 999
*
*
      else
         IER = BEEP()
      end if
*
*       Update the displaied line
*
      IF (ivideo .le. 3) THEN
         ier = locate(icur+ixc-ISHT,iyc)
      ELSE
*
*       Clear Graphic cursor
*
        IER = SET_XOR(1)
        ier=graphic_text(CURSOR,
     *                 ixg(icurod+ixc),iyg(iyc),ICRCOL)
        IER = SET_XOR(0)
      END IF
      if (IUPDT .GT. 0) then
        if (ivideo .le. 3) then
           call write_string(instr(icur-isht:iupdt))
           ier = locate(icur+ixc,iyc)
        else
           ls = iupdt-(icur-isht)+1
           icol = set_color(0)
**
           ier = filled_rectangle(
     *      ixg(icur+ixc-isht),iyg(iyc),
     *      ixg(icur+ixc-isht+ls),iyg(iyc+1))

           ier=graphic_text(instr(icur-isht:iupdt),
     *                 ixg(icur+ixc-isht),iyg(iyc),1)

           icol = set_color(1)
*           icol = set_color(icol)
        end if
        isht = 0
      end if
      IF (IVIDEO .GT. 3) THEN
*
*       Set Graphic cursor
*
        IER = SET_XOR(1)
        ier=graphic_text(CURSOR,
     *                 ixg(icur+ixc),iyg(iyc),ICRCOL)
        IER = SET_XOR(0)
        ICUROD = ICUR
      END IF
*
  500 go to 10

  999 continue
      IF (IVIDEO .GT. 3) THEN
*
*       Clear Graphic cursor
*
        IER = SET_XOR(1)
        ier=graphic_text(CURSOR,
     *                 ixg(icurod+ixc),iyg(iyc),ICRCOL)
        IER = SET_XOR(0)
      END IF
      IF (LSTR .GT. MAXLIN) LSTR=MAXLIN
      lchstr = lstr
CC      EINSTR(:lstr) = INSTR(:lstr)
      return
      end
#endif
