/*
 * $Id: jibdef.h,v 1.1.1.1 1996/03/08 15:44:26 mclareni Exp $
 *
 * $Log: jibdef.h,v $
 * Revision 1.1.1.1  1996/03/08 15:44:26  mclareni
 * Cspack
 *
 */
#if defined(CERNLIB_VAXVMS)
#define JIB$C_DETACHED 0
#define JIB$C_NETWORK 1
#define JIB$C_BATCH 2
#define JIB$C_LOCAL 3
#define JIB$C_DIALUP 4
#define JIB$C_REMOTE 5
#define JIB$M_BYTCNT_WAITERS 1
#define JIB$M_TQCNT_WAITERS 2
#define JIB$K_LENGTH 148
#define JIB$C_LENGTH 148
#define JIB$S_JIBDEF 148
#define JIB$L_MTLFL 0
#define JIB$L_MTLBL 4
#define JIB$W_SIZE 8
#define JIB$B_TYPE 10
#define JIB$B_DAYTYPES 11
#define JIB$S_USERNAME 12
#define JIB$T_USERNAME 12
#define JIB$S_ACCOUNT 8
#define JIB$T_ACCOUNT 24
#define JIB$L_BYTCNT 32
#define JIB$L_BYTLM 36
#define JIB$L_PBYTCNT 40
#define JIB$L_PBYTLIM 44
#define JIB$W_FILCNT 48
#define JIB$W_FILLM 50
#define JIB$W_TQCNT 52
#define JIB$W_TQLM 54
#define JIB$L_PGFLQUOTA 56
#define JIB$L_PGFLCNT 60
#define JIB$L_CPULIM 64
#define JIB$W_PRCCNT 68
#define JIB$W_PRCLIM 70
#define JIB$W_SHRFCNT 72
#define JIB$W_SHRFLIM 74
#define JIB$W_ENQCNT 76
#define JIB$W_ENQLM 78
#define JIB$W_MAXJOBS 80
#define JIB$W_MAXDETACH 82
#define JIB$L_MPID 84
#define JIB$L_JLNAMFL 88
#define JIB$L_JLNAMBL 92
#define JIB$L_PDAYHOURS 96
#define JIB$L_ODAYHOURS 100
#define JIB$B_JOBTYPE 104
#define JIB$B_FLAGS 105
#define JIB$V_BYTCNT_WAITERS 0
#define JIB$V_TQCNT_WAITERS 1
#define JIB$L_ORG_BYTLM 108
#define JIB$L_ORG_PBYTLM 112
#define JIB$L_SPARE 116
#define JIB$L_CWPS_TIME 120
#define JIB$L_CWPS_COUNT 124
#define JIB$S_CWPS_Q1 8
#define JIB$Q_CWPS_Q1 128
#define JIB$L_CWPS_L1 136
#define JIB$L_CWPS_L2 140
#define JIB$L_JTQUOTA 144
#endif
