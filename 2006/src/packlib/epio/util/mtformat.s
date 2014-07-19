 .TITLE MT_FORMAT
       .IDENT  /FGV02.01/
 
 ; MT_FORMAT
 ; ---------
 ;
 ; The MT_FORMAT routine allows a process to set the format in which
 ; data are written/read to/from an already assigned tape.
 ;
 ; High-level Language format:
 ;
 ;   CALL MT_FORMAT(CHAN,DENSITY)
 ;
 ;    CHAN    Address of a word containing the number of the
 ;            I/O channel assigned to the magnetic tape
 ;            device.
 ;
 ;    DENSITY Format value.
 ;            Possible format values:
 ;
 ;            MT$K_NORMAL11 Normal PDP-11 format.Data bytes are
 ;                          recorded sequentially on tape with
 ;                          byte 0 of each word read/written
 ;                           before byte 1.
 ;
 ;            MT$K_NORMAL15 16-bits word mode.Data byte 1 of each word
 ;                          is read/written before data byte 0.
 ;
 ; Return status:
 ;
 ;   Normal VMS status.
 ;
 ;
 ; Author:
 ;   M.Scire'
 ;   CERN
 ;   1211 Geneve 23
 ;
 ; Update history:
 ; MSV01.01 14JAN81 MS - Original version.
 ;
 ; FGV02.01 19NOV82 F.Gagliardi - Fix bug:
 ;          the first long word of the quadword characteristics buffer
 ;          used as P1 argument for IO$_SETMODE QIO, was overwritten
 ;          by the content of DIB$L_DEVCHAR longword of the DIB block.
 ;
 ;
       .LIBRARY /SYS$LIBRARY:LIB.MLB/
       $DIBDEF                   ;DEVICE CHAR BUFFER OFFSETS
       $IODEF                    ;I/O FUNCTION CODES
       $MTDEF   GLOBAL           ;MAG TAPE SYMBOLIC
 
 IOSTATUS:    .BLKQ    1
 
 DEV_BUFFER:  .LONG DEV_BUFFER_E-DEV_BUFFER_S        ;LENGTH OF BUFFER
              .ADDRESS DEV_BUFFER_S            ;ADDRESS OF BUFFER
 DEV_BUFFER_S:.BLKB DIB$K_LENGTH   ;
 DEV_BUFFER_E:                              ;
       .PSECT $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
 CHANNEL = 4                       ;ARG1 ADDRESS
 FORMAT  = 8                       ;ARG2
 
       .ENTRY MT_FORMAT,^M<R2,R3,R4,R5>
 
       $GETCHN_S CHAN=@CHANNEL(AP),-   ;MAGTAPE CHANNEL
                 PRIBUF=DEV_BUFFER ;DEVICE BUFFER
       BLBC      R0,1000$
 
       INSV      FORMAT(AP),#MT$V_FORMAT,- ;
                 #MT$S_FORMAT,-    ;
                 DEV_BUFFER_S+DIB$L_DEVDEPEND     ;Insert new format
       $QIOW_S   EFN=#0,-           ; EVENT FLAG
                 CHAN=@CHANNEL(AP),-   ;CHANNEL
                 FUNC=#IO$_SETMODE,- ;FUNCTION SET MODE
                 IOSB=IOSTATUS,-     ;ADDRESS OF I/O STATUS
                 P1=DEV_BUFFER_S+DIB$B_DEVCLASS
 
       BLBC      R0,1000$            ; OK ?
       MOVZWL    IOSTATUS,R0         ;
 
 1000$:RET                         ;
       .END
 
