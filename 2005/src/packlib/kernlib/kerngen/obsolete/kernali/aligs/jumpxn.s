*
* $Id: jumpxn.s,v 1.1.1.1 1996/02/15 17:50:53 mclareni Exp $
*
* $Log: jumpxn.s,v $
* Revision 1.1.1.1  1996/02/15 17:50:53  mclareni
* Kernlib
*
*
 #
 #CERN PROGLIB# Z042    JUMPXN          .VERSION KERNALI  1.00  900919
 #ORIG. 8/11/89 R.WARREN
 #
        .verstamp       1 31
        .lcomm  $$6 4
        .lcomm  $$7 4
        .text
        .align  2
        .file   2 "jumpxq.f"
        .globl  jumpad_
        .loc    2 3
        .ent    jumpad_ 2
jumpad_:
        .option O1
        subu    $sp, 48
        sw      $31, 20($sp)
        .mask    0x80000000, -28
        .frame  $sp, 48, $31
        b       $37
$32:
        .loc    2 5
        .set     volatile
        lw      $4, 28($sp)
        .set     novolatile
        jal     locb_
        sw      $2, 40($sp)
        b       $36
        .globl  jumpst_
        .aent   jumpst_ 2
jumpst_:
        subu    $sp, 48
        sw      $31, 20($sp)
        sw      $4, 48($sp)
        b       $38
$33:
        .loc    2 8
        .loc    2 9
        .set     volatile
        lw      $14, 28($sp)
        .set     novolatile
        lw      $15, 0($14)
        sw      $15, $$7
        b       $36
        .globl  jumpxq_
        .aent   jumpxq_ 2
jumpxq_:
        .globl  jumpx0_
        .aent   jumpx0_ 2
jumpx0_:
        .globl  jumpx1_
        .aent   jumpx1_ 2
jumpx1_:
        .globl  jumpx2_
        .aent   jumpx2_ 2
jumpx2_:
        .loc    2 15
        lw      $24, $$7
        j       $24
$35:
        .lab    $$6
$36:
        .loc    2 17
        lw      $2, 40($sp)
        b       $39
$37:
        sw      $4, 28($sp)
        b       $32
$38:
        lw      $4, 48($sp)
        sw      $4, 28($sp)
        b       $33
        .endb   12
        .endb   13
        .endb   14
$39:
        lw      $31, 20($sp)
        addu    $sp, 48
        j       $31
        .end    jumpad_
#ifdef CERNLIB_TCGEN_JUMPXN
#undef CERNLIB_TCGEN_JUMPXN
#endif
